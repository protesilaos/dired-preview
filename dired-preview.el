;;; dired-preview.el --- Automatically preview file at point in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/dired-preview
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
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
          "\\|iso\\|epub\\|pdf\\)")
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

(define-obsolete-variable-alias
  'dired-preview-display-action-alist-function
  'dired-preview-display-action-alist
  "0.3.0")

(defcustom dired-preview-display-action-alist
  #'dired-preview-display-action-alist-dwim
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
action alist.  See `dired-preview-display-action-alist-dwim' for the
implementation details."
  :group 'dired-preview
  :type 'function)

(defcustom dired-preview-delay 0.7
  "Time in seconds to wait before previewing."
  :group 'dired-preview
  :type 'number)

(defcustom dired-preview-chunk-size 10240
  "Size in bytes to read from large files."
  :group 'dired-preview
  :type 'natnum)

(defvar dired-preview--buffers nil
  "List with buffers of previewed files.")

(defvar dired-preview--large-files-alist nil
  "Alist mapping previewed large files to buffer names.")

(defun dired-preview--get-buffers ()
  "Return buffers that show previews."
  (seq-filter #'buffer-live-p dired-preview--buffers))

;; TODO 2023-07-07: This can become a user option, but let's keep it
;; simple for now.  We need to be sure this is always doing the right
;; thing.
(defvar dired-preview--buffers-threshold (* 1000 1024)
  "Maximum cumulative buffer size of previews.
When the accumulated preview buffers exceed this number and
`dired-preview--kill-buffers' is called, it will kill buffers
until it drops below this number.")

(defun dired-preview--get-buffer-cumulative-size ()
  "Return cumulative buffer size of `dired-preview--get-buffers'."
  (let ((size 0))
    (mapc
     (lambda (buffer)
       (setq size (+ (buffer-size buffer) size)))
     (dired-preview--get-buffers))
    size))

(defun dired-preview--kill-buffers ()
  "Kill preview buffers up to `dired-preview--buffers-threshold'."
  (let ((buffers (nreverse (dired-preview--get-buffers))))
    (catch 'stop
      (mapc
       (lambda (buffer)
         (when (and (>= (dired-preview--get-buffer-cumulative-size) dired-preview--buffers-threshold)
                    (not (eq buffer (current-buffer))))
           (ignore-errors (kill-buffer-if-not-modified buffer))
           (setq buffers (delq buffer buffers)))
         (throw 'stop t))
       buffers))
    (setq dired-preview--buffers (delq nil (nreverse buffers)))))

(defun dired-preview--kill-large-buffers ()
  "Kill buffers previewing large files."
  (mapc (lambda (pair)
          (let ((buffer (cdr pair)))
            (and (bufferp buffer)
                 (kill-buffer buffer))))
        dired-preview--large-files-alist)
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
  "Delete preview windows."
  (mapc
   (lambda (window)
     (unless (or (one-window-p)
                 (eq window (minibuffer-window)))
       (delete-window window)))
   (dired-preview--get-windows)))

(defun dired-preview--file-ignored-p (file)
  "Return non-nil if FILE extension is among the ignored extensions.
See user option `dired-preview-ignored-extensions-regexp'."
  (when-let (((not (file-directory-p file)))
             ((stringp dired-preview-ignored-extensions-regexp))
             (ext (file-name-extension file :include-dot))
             ((not (string-blank-p ext))))
    (string-match-p ext dired-preview-ignored-extensions-regexp)))

(defun dired-preview--file-large-p (file)
  "Return non-nil if FILE exceeds `dired-preview-max-size'."
  (> (file-attribute-size (file-attributes file)) dired-preview-max-size))

(defun dired-preview--file-displayed-p (file)
  "Return non-nil if FILE is already displayed in a window."
  (when-let ((buffer (get-file-buffer file))
             (window (get-buffer-window buffer)))
    (window-live-p window)))

(defun dired-preview--set-window-parameters (window value)
  "Set desired WINDOW parameters to VALUE."
  (with-selected-window window
    (set-window-parameter window 'dired-preview-window value)
    (set-window-parameter window 'dedicated value)
    (set-window-parameter window 'no-other-window value)))

(defun dired-preview--clean-up-window ()
  "Delete or clean up preview window."
  (if (window-parameter (selected-window) 'dired-preview-window)
      (dired-preview--delete-windows)
    (dired-preview--set-window-parameters (selected-window) nil)
    (remove-hook 'post-command-hook #'dired-preview--clean-up-window :local)))

;; TODO 2024-04-22: Add PDF type and concomitant method to display its buffer.
(defun dired-preview--infer-type (file)
  "Infer what type FILE is.
Return a cons cell whose `car' is a symbol describing FILE and `cdr' is
FILE."
  (let ((ext (file-name-extension file :include-dot))
        (file (expand-file-name file)))
    (cond
     ((and (not (string-empty-p ext))
           dired-preview-ignored-extensions-regexp
           (string-match-p ext dired-preview-ignored-extensions-regexp))
      (cons 'ignore file))
     ((dired-preview--file-large-p file)
      (cons 'large file))
     ((and (not (string-empty-p ext))
           (string-match-p ext dired-preview-image-extensions-regexp))
      (cons 'image file))
     ((file-directory-p file)
      (cons 'directory file))
     (t
      (cons 'text file)))))

(cl-defgeneric dired-preview--get-buffer (file)
  "Get a buffer for FILE.")

;; FIXME 2024-04-22: We have a lot of repetitive code.  Can we expand
;; a macro inside of a `cl-defmethod' or, alternatively, have a macro
;; that returns the method with its implementation?
(cl-defmethod dired-preview--get-buffer ((file (head text)))
  "Get preview buffer for text FILE type."
  (cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore))
    (let ((file (cdr file))
          (inhibit-message t)
          (enable-dir-local-variables nil)
          (enable-local-variables :safe)
          (non-essential t))
      (find-file-noselect file :nowarn))))

(defun dired-preview--add-truncation-message ()
  "Add a message indicating that the previewed file is truncated."
  (let ((end-ov (make-overlay (1- (point-max)) (point-max))))
    (overlay-put
     end-ov 'display
     (propertize "\n--PREVIEW TRUNCATED--" 'face 'shadow))))

;;;###autoload
(defmacro dired-preview-with-window (&rest body)
  "Evaluate BODY with the Dired preview window as selected."
  (declare (indent 0))
  `(dolist (win (dired-preview--get-windows))
     (with-selected-window win
       ,@body)))

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
  (if-let ((command (cond
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
      (when-let ((file buffer-file-name))
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
    (call-interactively 'scroll-up-command)))

;; Same as above for the terminology.
(defun dired-preview-page-up ()
  "Move a page up in the preview window.
This technically runs `scroll-down-command'."
  (interactive)
  (dired-preview-with-window
    (call-interactively 'scroll-down-command)))

(declare-function hexl-mode "hexl")
(declare-function hexl-mode-exit "hexl" (&optional arg))

(defun dired-preview-hexl-toggle ()
  "Toggle preview between text and `hexl-mode'."
  (interactive)
  (dired-preview-with-window
    (if (eq major-mode 'hexl-mode)
        (hexl-mode-exit)
      (hexl-mode)
      (dired-preview--add-truncation-message))))

(cl-defmethod dired-preview--get-buffer ((file (head large)))
  "Get preview buffer for large FILE.
The size of the leading chunk is specified by
`dired-preview-chunk-size'."
  (cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore))
    (let ((file (cdr file))
          (inhibit-message t)
          (enable-dir-local-variables nil)
          (enable-local-variables :safe)
          (non-essential t))
      (if-let* ((buffer (or (get-file-buffer file)
                            (find-buffer-visiting file)
                            (alist-get file dired-preview--large-files-alist
                                       nil nil #'equal))))
          buffer ; Buffer is already being visited, we can reuse it
        (with-current-buffer (create-file-buffer file)
          ;; We create a buffer with a partial preview
          (buffer-disable-undo)
          (insert-file-contents file nil 1 dired-preview-chunk-size 'replace)
          (when (eq buffer-file-coding-system 'no-conversion)
            (hexl-mode))
          (dired-preview--add-truncation-message)
          (read-only-mode t)
          ;; Because this buffer is not marked as visiting FILE, we need to keep
          ;; track of it ourselves.
          (setf (alist-get file dired-preview--large-files-alist
                           nil nil 'equal)
                (current-buffer)))))))

(cl-defmethod dired-preview--get-buffer ((file (head ignore)))
  "Get preview placeholder buffer for an ignored FILE."
  (let* ((file (cdr file))
         (buffer-name (format "%s (no preview)"
                              (file-name-nondirectory file))))
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
  (cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore))
    (let ((file (cdr file))
          (inhibit-message t)
          (enable-dir-local-variables nil)
          (enable-local-variables :safe)
          (non-essential t))
      (dired-noselect file))))

;; FIXME 2024-04-22: Best way to preview images and PDF files?  For now
;; this is the same as the text file type, though we need to refine
;; it.
(cl-defmethod dired-preview--get-buffer ((file (head image)))
  "Get preview buffer for image FILE type."
  (cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore))
    (let ((file (cdr file))
          (inhibit-message t)
          (enable-dir-local-variables nil)
          (enable-local-variables :safe)
          (non-essential t))
      (find-file-noselect file :nowarn))))

(defun dired-preview--add-to-previews (file)
  "Add FILE to `dired-preview--buffers', if not already in a buffer.
Always return FILE buffer."
  (cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore))
    (let ((buffer (find-buffer-visiting file)))
      (if (buffer-live-p buffer)
          buffer
        (setq buffer (dired-preview--get-buffer (dired-preview--infer-type file))))
      (with-current-buffer buffer
        (add-hook 'post-command-hook #'dired-preview--clean-up-window nil :local))
      (add-to-list 'dired-preview--buffers buffer)
      buffer)))

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
    (:width (if-let ((window-width (floor (window-total-width) 2))
                     ((> window-width fill-column)))
                window-width
              fill-column))
    (:height (floor (window-height) 2))))

(defun dired-preview-display-action-side ()
  "Pick a side window that is appropriate for the given frame."
  (if-let (split-width-threshold
           (width (window-body-width))
           ((>= width (window-body-height)))
           ((>= width split-width-threshold)))
      `(:side right :dimension window-width :size ,(dired-preview-get-window-size :width))
    `(:side bottom :dimension window-height :size ,(dired-preview-get-window-size :height))))

(defun dired-preview-display-action-alist-dwim ()
  "Reference function for `dired-preview-display-action-alist'.
Return a `display-buffer' action alist, as described in the
aforementioned user option."
  (let ((properties (dired-preview-display-action-side)))
    `((display-buffer-in-side-window)
      (side . ,(plist-get properties :side))
      (,(plist-get properties :dimension) . ,(plist-get properties :size)))))

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
  (dired-preview--kill-buffers)
  (dired-preview--kill-large-buffers)
  (dired-preview--kill-placeholder-buffers))

(defun dired-preview--close-previews-outside-dired ()
  "Call `dired-preview--close-previews' if the current buffer is not in Dired mode."
  (unless (eq major-mode 'dired-mode)
    (dired-preview--close-previews)
    (remove-hook 'window-state-change-hook #'dired-preview--close-previews-outside-dired)
    (put 'dired-preview-start 'function-executed nil)))

(defun dired-preview--display-buffer (buffer)
  "Call `display-buffer' for BUFFER.
Only do it with the current major mode is Dired."
  (let ((action-alist (cond
                       ((functionp dired-preview-display-action-alist)
                        (funcall dired-preview-display-action-alist))
                       (dired-preview-display-action-alist)
                       (t
                        (dired-preview-display-action-alist-dwim)))))
    (display-buffer buffer action-alist)))

(defun dired-preview-display-file (file)
  "Display preview of FILE if appropriate."
  (dired-preview--delete-windows)
  (when-let ((buffer (dired-preview--get-preview-buffer file)))
    (dired-preview--display-buffer buffer)
    (when-let ((window (get-buffer-window buffer)))
      (dired-preview--set-window-parameters window t))))

(defun dired-preview--preview-p (file)
  "Return non-nil if FILE can be previewed."
  (and file
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

(defun dired-preview-trigger (&optional no-delay)
  "Trigger display of file at point after `dired-preview-trigger-commands'.
With optional NO-DELAY do not start a timer.  Otherwise produce
the preview with `dired-preview-delay' of idleness."
  (add-hook 'window-state-change-hook #'dired-preview--close-previews-outside-dired)
  (dired-preview--cancel-timer)
  (let* ((file (dired-file-name-at-point))
         (preview (dired-preview--preview-p file)))
    (cond
     ((and preview (memq this-command dired-preview-trigger-commands))
      (if no-delay
          (dired-preview-display-file file)
        (setq dired-preview--timer
              (run-with-idle-timer dired-preview-delay nil #'dired-preview-display-file file))))
     ((and file preview)
      (dired-preview-start file))
     ((and (not preview)
           (memq this-command dired-preview-trigger-commands))
      (dired-preview--delete-windows)))
    (dired-preview--close-previews-outside-dired)))

(defun dired-preview-disable-preview ()
  "Disable Dired preview."
  (unless (eq major-mode 'dired-mode)
    (user-error "Can only use `dired-preview' in Dired"))
  (remove-hook 'post-command-hook #'dired-preview-trigger :local)
  (dired-preview--close-previews)
  (put 'dired-preview-start 'function-executed nil))

(defun dired-preview-enable-preview ()
  "Enable Dired preview."
  (unless (eq major-mode 'dired-mode)
    (user-error "Can only use `dired-preview' in Dired"))
  (add-hook 'post-command-hook #'dired-preview-trigger nil :local)
  (dired-preview-trigger :no-delay))

(defvar dired-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'dired-preview-hexl-toggle)
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
