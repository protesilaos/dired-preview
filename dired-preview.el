;;; dired-preview.el --- Automatically preview file at point in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/dired-preview
;; Version: 0.5.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: files, convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a simple package to automatically preview in a side window the
;; file at point in Dired buffers.  Preview windows are closed when they
;; are no longer relevant, while preview buffers are killed if they have
;; not been used for other purposes beside previewing.
;;
;; Enable the `dired-preview-mode' in the current Dired buffer or
;; `dired-preview-global-mode' for all Dired buffers and then perform
;; the regular up/down motions.  Those will trigger the preview.
;;
;; The previewed file is displayed in a side window if its size is below
;; the number specified in the user option `dired-preview-max-size'.
;; Previews are shown subject to a small delay, per ther user option
;; `dired-preview-delay'.
;;
;; Files matching the `dired-preview-ignored-extensions-regexp' are
;; not previewed, though a preview window is still displayed if the
;; user option `dired-preview-ignored-show-ignored-placeholders' is
;; non-nil.  This is to avoid windows jumping in and out of focus as
;; the user moves between files.
;;
;; [ In the future, we may find ways to quickly preview any file type
;;   without affecting the performance of Emacs.]
;;
;; I took inspiration for `dired-preview' from the now unmaintained
;; `peep-dired' package by Adam Sokolnicki: <https://github.com/asok/peep-dired>.
;; My original plan was to volunteer to maintain `peep-dired` but I
;; decided to write it my own way: it was easier for me, plus I can
;; implement only what I consider necessary without upsetting existing
;; users.

;;; Code:

(require 'dired)
(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup dired-preview nil
  "Automatically preview file at point in Dired."
  :group 'dired)

(defcustom dired-preview-ignored-extensions-regexp
  (concat "\\."
          "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a\\|flac\\|wav"
          "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
          "\\|iso\\|epub\\|pdf\\)\\'")
  "Regular expression of file type extensions to not preview.
When the value is nil, do not ignore any file: preview
everything.

A placeholder window will be displayed even for files that are ignored,
in order to avoid windows jumping in and out of focus.  This behaviour
is controlled by the `dired-preview-ignored-show-ignored-placeholders'
user option."
  :group 'dired-preview
  :type '(choice (const :tag "Do not ignore any file (preview everything)" nil)
                 (string :tag "Ignore files matching regular expression")))

(defcustom dired-preview-ignored-show-ignored-placeholders t
  "When non-nil, show a placeholder preview buffer for ignored files.
Ignored files are controlled by the `dired-preview-ignored-extensions-regexp'
user option."
  :type 'boolean
  :package-version '(dired-preview . "0.3.0")
  :group 'dired-preview)

(defcustom  dired-preview-image-extensions-regexp "\\.\\(png\\|jpg\\|jpeg\\|tiff\\)"
  "List of file extensions representing image types."
  :group 'dired-preview
  :type '(string :tag "Image files matching regular expression"))

(defcustom dired-preview-max-size (expt 2 20)
  "Files larger than this byte limit are not previewed."
  :group 'dired-preview
  :type 'natnum)

(defcustom dired-preview-kill-buffers-method (cons 'buffer-number 10)
  "Determine whether to periodically kill preview buffers while in Dired.
When the value is nil, do not kill any preview buffer.

When the value is a cons cell of the form (SYMBOL . NATURAL-NUMBER),
check if symbol is one of the following to derive the meaning of its
NATURAL-NUMBER.

- `buffer-number' means to kill the number of preview buffers that
  exceed the NATURAL-NUMBER.

- `combined-size' means to kill buffers until their combined size does
  not exceed the NATURAL-NUMBER.

Whatever the SYMBOL, buffers are killed from oldest to newest.

Buffers are always killed when exiting Dired."
  :type '(choice
          (cons (choice (const :tag "Maximum number of buffers" buffer-number)
                        (const :tag "Maximum cumulative buffer size" combined-size))
                natnum)
          (const :tag "Do not kill any preview buffers" nil))
  :package-version '(dired-preview . "0.4.0")
  :group 'dired-preview)

(define-obsolete-variable-alias
  'dired-preview-display-action-alist-function
  'dired-preview-display-action-alist
  "0.3.0")

(defcustom dired-preview-display-action-alist #'dired-preview-display-action-alist-dwim
  "The `display-buffer' action alist for the preview window.
This is the same data that is passed to `display-buffer-alist'.
Read Info node `(elisp) Displaying Buffers'.  As such, it is
meant for experienced users.

Example of a valid value:

    \\='((display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.2)
      (preserve-size . (t . t)))

The value may also be a function, which returns a `display-buffer'
action alist.  See `dired-preview-display-action-alist-dwim' (the
default value of this variable) for the implementation details.  A
simpler alternative is the function `dired-preview-display-action-alist-below'.

The `dired-preview-display-action-alist-dwim' will display the preview
window either at the right hand side or the bottom of the frame,
depending on the available space.  It will also try to resize the window
accordingly.

Whereas the `dired-preview-display-action-alist-below' has a more simple
behaviour of always displaying the preview window below the currently
selected window and always setting the preview window's height to 0.3
times the height of the frame."
  :group 'dired-preview
  :package-version '(dired-preview . "0.4.0")
  :type `(choice
          (alist :key-type
                 (choice :tag "Condition"
                         regexp
                         (function :tag "Matcher function"))
                 :value-type ,display-buffer--action-custom-type)
          (function :tag "Open to the side or bottom" dired-preview-display-action-alist-dwim)
          (function :tag "Open below the selected window" dired-preview-display-action-alist-below)
          (function :tag "Custom function like `dired-preview-display-action-alist-dwim'"))
  :risky t)

(defcustom dired-preview-delay 0.7
  "Time in seconds to wait before previewing.
If the value is 0, then it is internally understood as 0.1 as no delay
can affect performance."
  :group 'dired-preview
  :type 'number)

(defcustom dired-preview-chunk-size 10240
  "Size in bytes to read from large files."
  :group 'dired-preview
  :type 'natnum)

(defcustom dired-preview-buffer-name-indicator "[P]"
  "String to prepend to the name of preview buffers."
  :type 'string
  :package-version '(dired-preview . "0.4.0")
  :group 'dired-preview)

(defvar dired-preview--buffers nil
  "List with buffers of previewed files.")

(defvar dired-preview--large-files-alist nil
  "Alist mapping previewed large files to buffer names.")

(defun dired-preview--get-buffers ()
  "Return buffers that show previews."
  (seq-filter #'buffer-live-p dired-preview--buffers))

(defun dired-preview--get-buffer-cumulative-size (buffers)
  "Return cumulative size of BUFFERS."
  (let ((size 0))
    (dolist (buffer buffers)
      (setq size (+ (buffer-size buffer) size)))
    size))

(defun dired-preview--kill-buffers-by-size (buffers max-combined-size)
  "Kill BUFFERS to not exceed MAX-COMBINED-SIZE."
  (catch 'enough
    (dolist (buffer buffers)
      (if (>= (dired-preview--get-buffer-cumulative-size buffers) max-combined-size)
          (if (eq buffer (current-buffer))
              (setq buffers (delq buffer buffers))
            (ignore-errors (kill-buffer-if-not-modified buffer)))
        (throw 'enough t))))
  (setq dired-preview--buffers (delq nil (nreverse buffers))))

(defun dired-preview--kill-buffers-by-length (buffers max-length)
  "Kill BUFFERS up to MAX-LENGTH."
  (let ((length (length buffers)))
    (catch 'enough
      (dolist (buffer buffers)
        (if (>= length max-length)
            (progn
              (if (eq buffer (current-buffer))
                  (setq buffers (delq buffer buffers))
                (ignore-errors (kill-buffer-if-not-modified buffer)))
              (setq length (1- length)))
          (throw 'enough t)))))
  (setq dired-preview--buffers (delq nil (nreverse buffers))))

(defun dired-preview--kill-buffers-unconditionally (buffers)
  "Kill all BUFFERS except the current one."
  (dolist (buffer buffers)
    (when (not (eq buffer (current-buffer)))
      (ignore-errors (kill-buffer-if-not-modified buffer)))
    (setq buffers (delq buffer buffers)))
  (setq dired-preview--buffers (delq nil (nreverse buffers))))

(defun dired-preview--kill-buffers (&optional kill-all)
  "Implement `dired-preview-kill-buffers-method'.
With optional KILL-ALL, kill all buffers regardless of the
aforementioned user option."
  (when-let* ((buffers (nreverse (dired-preview--get-buffers))))
    (cond
     (kill-all
      (dired-preview--kill-buffers-unconditionally buffers))
     (dired-preview-kill-buffers-method
      (pcase-let ((`(,method . ,number) dired-preview-kill-buffers-method))
        (pcase method
          ('combined-size (dired-preview--kill-buffers-by-size buffers number))
          ('buffer-number (dired-preview--kill-buffers-by-length buffers number))
          (_ (error "The `%s' in `dired-preview-kill-buffers-method' is unknown" method))))))))

(defun dired-preview--kill-large-buffers ()
  "Kill buffers previewing large files."
  (dolist (pair dired-preview--large-files-alist)
    (let ((buffer (cdr pair)))
      (and (bufferp buffer)
           (kill-buffer buffer))))
  (setq dired-preview--large-files-alist nil))

(defun dired-preview--kill-placeholder-buffers ()
  "Kill all placeholder preview buffers."
  (setq dired-preview--buffers
        (seq-remove (lambda (buffer)
                      (with-current-buffer buffer
                        (when (and (boundp 'dired-preview--placeholder-buffer-p)
                                   dired-preview--placeholder-buffer-p)
                          (ignore-errors (kill-buffer buffer))
                          t)))
                    (dired-preview--get-buffers))))

(defun dired-preview--window-parameter-p (window)
  "Return non-nil if WINDOW has `dired-preview-window' parameter."
  (window-parameter window 'dired-preview-window))

(defun dired-preview--get-windows ()
  "Return windows that show previews."
  (seq-filter #'dired-preview--window-parameter-p (window-list)))

(defun dired-preview--delete-windows ()
  "Delete preview windows or clean them up if they should not be deleted."
  (dolist (window (dired-preview--get-windows))
    (if (and (not (one-window-p))
               (window-live-p window)
               (not (eq window (minibuffer-window)))
               (not (window-prev-buffers window)))
        (delete-window window)
      (dired-preview--clean-up-window window))))

(defun dired-preview--file-ignored-p (file)
  "Return non-nil if FILE extension is among the ignored extensions.
See user option `dired-preview-ignored-extensions-regexp'."
  (when-let* (((stringp dired-preview-ignored-extensions-regexp))
              ((not (file-directory-p file)))
              (file-nondir (file-name-nondirectory file)))
    (string-match-p dired-preview-ignored-extensions-regexp file-nondir)))

(defun dired-preview--file-large-p (file)
  "Return non-nil if FILE exceeds `dired-preview-max-size'."
  (>= (or (file-attribute-size (file-attributes file))
          0)
      dired-preview-max-size))

(defun dired-preview--file-displayed-p (file)
  "Return non-nil if FILE is already displayed in a window."
  (when-let* ((buffer (get-file-buffer file))
              (window (get-buffer-window buffer)))
    (window-live-p window)))

(defun dired-preview--set-window-parameters (window value)
  "Set desired WINDOW parameters to VALUE."
  (with-selected-window window
    (set-window-parameter window 'dired-preview-window value)
    (set-window-parameter window 'dedicated value)
    (set-window-parameter window 'no-other-window value)))

(defun dired-preview--clean-up-window (&optional window)
  "Remove preview state from WINDOW or `selected-window'."
  (let* ((w (or window (selected-window)))
        (buffer (window-buffer w)))
    (dired-preview--rename-buffer (window-buffer w) :make-public)
    (setq dired-preview--buffers (delq buffer dired-preview--buffers))
    (dired-preview--set-window-parameters w nil)
    (remove-hook 'post-command-hook #'dired-preview--clean-up-window :local)))

;; TODO 2024-04-22: Add PDF type and concomitant method to display its buffer.
(defun dired-preview--infer-type (file)
  "Infer what type FILE is.
Return a cons cell whose `car' is a symbol describing FILE and `cdr' is
FILE."
  (let* ((file (expand-file-name file))
         (file-nondir (file-name-nondirectory file)))
    (cond
     ((and dired-preview-ignored-extensions-regexp
           (string-match-p dired-preview-ignored-extensions-regexp file-nondir))
      (cons 'ignore file))
     ((file-directory-p file)
      (cons 'directory file))
     ((dired-preview--file-large-p file)
      (cons 'large file))
     ((string-match-p dired-preview-image-extensions-regexp file-nondir)
      (cons 'image file))
     (t
      (cons 'text file)))))

(defmacro dired-preview-with-file-setup (&rest body)
  "Run BODY while setting up the right preview environment."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore))
     (let ((file (cdr file))
           (inhibit-message t)
           (enable-dir-local-variables nil)
           (enable-local-variables :safe)
           (non-essential t))
       ,@body)))

(cl-defgeneric dired-preview--get-buffer (file)
  "Get a buffer for FILE.")

;; FIXME 2024-04-22: We have a lot of repetitive code.  Can we expand
;; a macro inside of a `cl-defmethod' or, alternatively, have a macro
;; that returns the method with its implementation?
(cl-defmethod dired-preview--get-buffer ((file (head text)))
  "Get preview buffer for text FILE type."
  (dired-preview-with-file-setup
   (find-file-noselect file :nowarn)))

(defun dired-preview--add-truncation-message ()
  "Add a message indicating that the previewed file is truncated."
  (let* ((max (point-max))
         (end-ov (make-overlay (1- max) max)))
    (overlay-put
     end-ov 'display
     (propertize "\n--PREVIEW TRUNCATED--" 'face 'shadow))))

;;;###autoload
(defmacro dired-preview-with-window (&rest body)
  "Evaluate BODY with the Dired preview window as selected."
  (declare (indent 0))
  `(if-let* ((windows (dired-preview--get-windows)))
       (dolist (win windows)
         (with-selected-window win
           ,@body))
     (user-error "No dired-preview window available")))

(defun dired-preview-find-file ()
  "Visit the currently previewed buffer with `find-file'.
This means that the buffer is no longer among the previews.

Also see `dired-preview-open-dwim'."
  (interactive)
  (let (file buffer)
    (dired-preview-with-window
      (setq file buffer-file-name)
      (dired-preview--close-previews-outside-dired)
      (setq buffer (find-file-noselect file)))
    (pop-to-buffer buffer)))

(defvar dired-preview-media-extensions-regexp
  "\\.\\(mp3\\|m4a\\|flac\\|mp4\\|ogg\\|mpv\\|webm\\|mov\\|wav\\)"
  "Regular expression matching media file extensions.")

(declare-function w32-shell-execute "w32fns.c")

;; NOTE 2024-07-29: Adapted from the `dired-do-open' found in Emacs 31.0.50.
(defun dired-preview--open-externally (file)
  "Run appropriate command to open FILE externally."
  (if-let* ((command (cond
                      ((executable-find "xdg-open")
                       "xdg-open")
                      ((memq system-type '(gnu/linux darwin))
                       "open")
                      ((memq system-type '(windows-nt ms-dos))
                       "start")
                      ((eq system-type 'cygwin)
                       "cygstart")
                      ((executable-find "run-mailcap")
                       "run-mailcap"))))
      (cond
       ((memq system-type '(gnu/linux))
        (call-process command nil 0 nil file))
       ((memq system-type '(ms-dos))
        (shell-command (concat command " " (shell-quote-argument file))))
       ((memq system-type '(windows-nt))
        (w32-shell-execute command (convert-standard-filename file)))
       ((memq system-type '(cygwin))
        (call-process command nil nil nil file))
       ((memq system-type '(darwin))
        (start-process (concat command " " file) nil command file)))
    (error "Cannot find a command to open `%s' externally" file)))

(defun dired-preview-open-dwim ()
  "Do-What-I-Mean open the currently previewed file.
This means that the buffer is no longer among the previews.

If the file name matches `dired-preview-media-extensions-regexp',
`dired-preview-ignored-extensions-regexp', or
`dired-preview-image-extensions-regexp', then open it externally.
Otherwise, visit the file in an Emacs buffer.

Also see `dired-preview-find-file'."
  (interactive)
  (let (buffer)
    (dired-preview-with-window
      (when-let* ((file buffer-file-name))
        (cond
         ((or (string-match-p dired-preview-media-extensions-regexp file)
              (string-match-p dired-preview-ignored-extensions-regexp file)
              (string-match-p dired-preview-image-extensions-regexp file))
          (dired-preview--open-externally file))
         (t
          (dired-preview--close-previews-outside-dired)
          (setq buffer (find-file-noselect file))))))
    (when buffer
      (pop-to-buffer buffer))))

;; NOTE 2024-07-29: "Scroll up/down" confuses me in this context
;; because the motion is in the opposite direction.  So "page up/down"
;; is fine, based on what the keys of the same name do.
(defun dired-preview-page-down ()
  "Move a page down in the preview window.
This technically runs `scroll-up-command'."
  (interactive)
  (dired-preview-with-window
    (call-interactively
     (pcase (derived-mode-p major-mode)
       ('doc-view-mode 'doc-view-scroll-up-or-next-page)
       ('pdf-view-mode 'pdf-view-scroll-up-or-next-page)
       (_ 'scroll-up-command)))))

;; Same as above for the terminology.
(defun dired-preview-page-up ()
  "Move a page up in the preview window.
This technically runs `scroll-down-command'."
  (interactive)
  (dired-preview-with-window
    (call-interactively
     (pcase (derived-mode-p major-mode)
       ('doc-view-mode 'doc-view-scroll-down-or-previous-page)
       ('pdf-view-mode 'pdf-view-scroll-down-or-previous-page)
       (_ 'scroll-down-command)))))

(declare-function hexl-mode "hexl")
(declare-function hexl-mode-exit "hexl" (&optional arg))
(defvar hexl-follow-ascii)

(defun dired-preview-hexl-toggle ()
  "Toggle preview between text and `hexl-mode'."
  (interactive)
  (dired-preview-with-window
    (let ((hexl-follow-ascii nil))
      (if (eq major-mode 'hexl-mode)
          (hexl-mode-exit)
        (hexl-mode 1)))
    (dired-preview--add-truncation-message)))

(cl-defmethod dired-preview--get-buffer ((file (head large)))
  "Get preview buffer for large FILE.
The size of the leading chunk is specified by
`dired-preview-chunk-size'."
  (dired-preview-with-file-setup
   (if-let* ((buffer (or (get-file-buffer file)
                         (find-buffer-visiting file)
                         (alist-get file dired-preview--large-files-alist nil nil #'equal))))
       buffer ; Buffer is already being visited, we can reuse it
     (with-current-buffer (create-file-buffer file)
       ;; We create a buffer with a partial preview
       (buffer-disable-undo)
       (insert-file-contents file nil 1 dired-preview-chunk-size 'replace)
       (when (eq buffer-file-coding-system 'no-conversion)
         (let ((hexl-follow-ascii nil))
           (hexl-mode 1)))
       (dired-preview--add-truncation-message)
       (read-only-mode t)
       ;; Because this buffer is not marked as visiting FILE, we need to keep
       ;; track of it ourselves.
       (setf (alist-get file dired-preview--large-files-alist nil nil 'equal) (current-buffer))))))

(cl-defmethod dired-preview--get-buffer ((file (head ignore)))
  "Get preview placeholder buffer for an ignored FILE."
  (let* ((file (cdr file))
         (buffer-name (format "%s (no preview)" (file-name-nondirectory file))))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (set-visited-file-name file t)
          (rename-buffer buffer-name)
          (set-buffer-modified-p nil)
          (setq-local dired-preview--placeholder-buffer-p t)
          (setq buffer-read-only t)
          (current-buffer)))))

(cl-defmethod dired-preview--get-buffer ((file (head directory)))
  "Get preview buffer for directory FILE type."
  (dired-preview-with-file-setup
   (dired-noselect file)))

;; FIXME 2024-04-22: Best way to preview images and PDF files?  For now
;; this is the same as the text file type, though we need to refine
;; it.
(cl-defmethod dired-preview--get-buffer ((file (head image)))
  "Get preview buffer for image FILE type."
  (dired-preview-with-file-setup
   (find-file-noselect file :nowarn)))

(defun dired-preview--add-to-previews (file)
  "Add FILE to `dired-preview--buffers', if not already in a buffer.
Return FILE buffer or nil."
  (cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore))
    (let* ((inhibit-message t)
           (existing-buffer (or (find-buffer-visiting file)
                                (and (file-directory-p file)
                                     (car (dired-buffers-for-dir file)))))
           (new-buffer (unless existing-buffer
                         (dired-preview--get-buffer (dired-preview--infer-type file)))))
      (dired-preview--kill-buffers)
      (when new-buffer
        (with-current-buffer new-buffer
          (add-hook 'post-command-hook #'dired-preview--clean-up-window nil :local))
        (add-to-list 'dired-preview--buffers new-buffer))
      (or existing-buffer new-buffer))))

(defun dired-preview--get-preview-buffer (file)
  "Return buffer to preview FILE in."
  (dired-preview--add-to-previews file))

(defvar dired-preview-buffer-name "*dired-preview*"
  "Name of preview buffer.")

(defun dired-preview-get-window-size (dimension)
  "Return window size by checking for DIMENSION.
DIMENSION is either a `:width' or `:height' keyword.  It is
checked against `split-width-threshold' or
`split-height-threshold'"
  (pcase dimension
    (:width (if-let* ((window-width (floor (window-total-width) 2))
                      ((>= window-width fill-column)))
                window-width
              fill-column))
    (:height (floor (window-height) 2))))

(defun dired-preview-display-action-side ()
  "Pick a side window that is appropriate for the given frame."
  (if-let* (split-width-threshold
            (width (window-body-width))
            ((>= width (window-body-height)))
            ((>= width split-width-threshold)))
      `(:side right :dimension window-width :size ,(dired-preview-get-window-size :width))
    `(:side bottom :dimension window-height :size ,(dired-preview-get-window-size :height))))

(defun dired-preview-display-action-alist-dwim ()
  "Reference function for `dired-preview-display-action-alist'.
Determine whether to show a preview window on the right side or at the
bottom of the frame, depending on the available space, and set the size
of the window accordingly.

Return a `display-buffer' action alist, as described in the
aforementioned user option."
  (let ((properties (dired-preview-display-action-side)))
    `((display-buffer-in-side-window)
      (side . ,(plist-get properties :side))
      (,(plist-get properties :dimension) . ,(plist-get properties :size)))))

(defun dired-preview-display-action-alist-below ()
  "Reference function for `dired-preview-display-action-alist'.
Always show the preview window below the currently selected window and
always keep the height of the preview window to 0.3 times that of the
total height of the frame.

Return a `display-buffer' action alist, as described in the
aforementioned user option."
  `((display-buffer-below-selected)
    (window-height . 0.3)
    (dedicated . t)
    (preserve-size . (t . t))))

(defvar dired-preview-trigger-commands
  '( dired-next-line
     dired-previous-line
     dired-flag-file-deletion
     dired-mark
     dired-unmark
     dired-unmark-backward
     dired-del-marker
     dired-goto-file
     dired-find-file
     scroll-up-command
     scroll-down-command)
  "List of Dired commands that trigger a preview.")

(defvar dired-preview--timer nil
  "Most recent timer object to display a preview.")

(defun dired-preview--cancel-timer ()
  "Cancel `dired-preview--timer' if it is a timer object."
  (when (timerp dired-preview--timer)
    (cancel-timer dired-preview--timer)))

(defun dired-preview--close-previews ()
  "Kill preview buffers and delete their windows."
  (dired-preview--cancel-timer)
  (dired-preview--delete-windows)
  (dired-preview--kill-buffers :kill-all)
  (dired-preview--kill-large-buffers)
  (dired-preview--kill-placeholder-buffers))

(defun dired-preview--close-previews-outside-dired ()
  "Call `dired-preview--close-previews' if the current buffer is not in Dired mode.
Do not consider the minibuffer as being another mode."
  (unless (or (eq major-mode 'dired-mode) (minibufferp))
    (dired-preview--close-previews)
    (remove-hook 'window-state-change-hook #'dired-preview--close-previews-outside-dired)
    (put 'dired-preview-start 'function-executed nil)))

(defun dired-preview--display-buffer (buffer)
  "Call `display-buffer' for BUFFER.
Only do it with the current major mode is Dired."
  (let ((action-alist (cond
                       ((functionp dired-preview-display-action-alist)
                        (funcall dired-preview-display-action-alist))
                       (dired-preview-display-action-alist))))
    (display-buffer buffer action-alist)))

(defun dired-preview--remove-preview-indicator (name)
  "Remove `dired-preview-buffer-name-indicator' from NAME."
  (string-trim
   (string-replace dired-preview-buffer-name-indicator "" name)
   "[\s\t\n\r]+" nil))

(defun dired-preview--rename-buffer (buffer &optional make-public)
  "Rename BUFFER to have `dired-preview-buffer-name-indicator'.
With optional MAKE-PUBLIC, remove the indicator."
  (let ((name (buffer-name buffer)))
    (with-current-buffer buffer
      (cond
       (make-public
        (rename-buffer (dired-preview--remove-preview-indicator name) :make-unique))
       ((and (not (string-match-p (regexp-quote dired-preview-buffer-name-indicator) name))
             (memq buffer dired-preview--buffers))
        (rename-buffer (format "%s %s" dired-preview-buffer-name-indicator name) :make-unique))))))

(defun dired-preview-display-file (file)
  "Display preview of FILE if appropriate."
  (dired-preview--delete-windows)
  (when-let* ((buffer (dired-preview--get-preview-buffer file)))
    (dired-preview--display-buffer buffer)
    (dired-preview--rename-buffer buffer)
    (when-let* ((window (get-buffer-window buffer)))
      (dired-preview--set-window-parameters window t))))

(defvar dired-preview-encryption-file-extensions '(".gpg" ".age")
  "List of strings specifying file extensions for encryption.")

(defun dired-preview--file-encrypted-p (file)
  "Return non-nil if FILE is encrypted.
More specifically, test if FILE has an extension among the
`dired-preview-encryption-file-extensions'."
  (when-let* ((extension (file-name-extension file :include-period)))
    (member extension dired-preview-encryption-file-extensions)))

(defun dired-preview--preview-p (file)
  "Return non-nil if FILE can be previewed."
  (and file
       (not (string-match-p "/\\./" file))
       (not (dired-preview--file-encrypted-p file))
       (or (file-regular-p file) (file-directory-p file))
       (file-readable-p file)
       (not (dired-preview--file-displayed-p file))
       (or dired-preview-ignored-show-ignored-placeholders
           (not (dired-preview--file-ignored-p file)))))

(defun dired-preview-start (file)
  "Preview FILE instantly when invoking Dired."
  (unless (get 'dired-preview-start 'function-executed)
    (put 'dired-preview-start 'function-executed t)
    (dired-preview-display-file file)))

(defun dired-preview--start-idle-timer (file)
  "Start the idle timer to preview FILE."
  (setq dired-preview--timer
        (run-with-idle-timer
         (if (> dired-preview-delay 0)
             dired-preview-delay
           0.1)
         nil #'dired-preview-display-file file)))

(defun dired-preview-trigger (&optional no-delay)
  "Trigger display of file at point after `dired-preview-trigger-commands'.
With optional NO-DELAY do not start a timer.  Otherwise produce
the preview with `dired-preview-delay' of idleness."
  (condition-case nil
      (if (eq major-mode 'dired-mode)
          (progn
            (add-hook 'window-state-change-hook #'dired-preview--close-previews-outside-dired)
            (dired-preview--cancel-timer)
            (let* ((file (dired-file-name-at-point))
                   (preview (dired-preview--preview-p file)))
              (cond
               ((and preview (memq this-command dired-preview-trigger-commands))
                (if no-delay
                    (dired-preview-display-file file)
                  (dired-preview--start-idle-timer file)))
               (preview
                (dired-preview-start file))
               ((and (not preview)
                     (memq this-command dired-preview-trigger-commands))
                (dired-preview--delete-windows)))))
        (dired-preview--close-previews-outside-dired))
    ((error user-error quit) nil)))

(defun dired-preview-get-first-window ()
  "Return a window object for `other-window-scroll-default'."
  (car (dired-preview--get-windows)))

(defun dired-preview-disable-preview ()
  "Disable Dired preview."
  (unless (eq major-mode 'dired-mode)
    (user-error "Can only use `dired-preview' in Dired"))
  (when (and other-window-scroll-default
             (eq other-window-scroll-default #'dired-preview-get-first-window))
    (setq-local other-window-scroll-default nil))
  (remove-hook 'post-command-hook #'dired-preview-trigger :local)
  (dired-preview--close-previews)
  (put 'dired-preview-start 'function-executed nil))

(defun dired-preview-enable-preview ()
  "Enable Dired preview."
  (unless (eq major-mode 'dired-mode)
    (user-error "Can only use `dired-preview' in Dired"))
  (when (>= emacs-major-version 29)
    (setq-local other-window-scroll-default #'dired-preview-get-first-window))
  (add-hook 'post-command-hook #'dired-preview-trigger nil :local)
  (dired-preview-trigger :no-delay))

(defvar dired-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x") #'dired-preview-hexl-toggle)
    (define-key map (kbd "C-c C-f") #'dired-preview-find-file)
    (define-key map (kbd "C-c C-o") #'dired-preview-open-dwim)
    (define-key map (kbd "C-c C-u") #'dired-preview-page-up)
    (define-key map (kbd "C-c C-d") #'dired-preview-page-down)
    map)
  "Key map for `dired-preview-mode'.")

;;;###autoload
(define-minor-mode dired-preview-mode
  "Buffer-local mode to preview file at point in Dired."
  :keymap dired-preview-mode-map
  :global nil
  (if dired-preview-mode
      (dired-preview-enable-preview)
    (dired-preview-disable-preview)))

(defun dired-preview--on ()
  "Enable `dired-preview-mode' in Dired."
  (when (eq major-mode 'dired-mode)
    (dired-preview-mode 1)))

;;;###autoload
(define-globalized-minor-mode dired-preview-global-mode
  dired-preview-mode dired-preview--on)

(provide 'dired-preview)
;;; dired-preview.el ends here
