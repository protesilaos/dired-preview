;;; dired-preview.el --- Automatically preview file at point in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/dired-preview
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.1.1
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
;; Files matching the `dired-preview-ignored-extensions-regexp' are not
;; previewed.  The default value of that user option includes multimedia,
;; PDFs, and EPUBs.
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
  (require 'subr-x))

(declare-function hexl-mode "hexl")
(declare-function hexl-mode-exit "hexl" (&optional arg))

(defgroup dired-preview nil
  "Automatically preview file at point in Dired."
  :group 'dired)

(defcustom dired-preview-ignored-extensions-regexp
  (concat "\\."
          "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
          "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
          "\\|iso\\|epub\\|pdf\\)")
  "Regular expression of file type extensions to not preview.
When the value is nil, do not ignore any file: preview
everything."
  :group 'dired-preview
  :type '(choice (const :tag "Do not ignore any file (preview everything)" nil)
                 (string :tag "Ignore files matching regular expression")))

(defcustom dired-preview-max-size (expt 2 20)
  "Files larger than this byte limit are not previewed."
  :group 'dired-preview
  :type 'natnum)

(defcustom dired-preview-delay 0.7
  "Time in seconds to wait before previewing."
  :group 'dired-preview
  :type 'number)

(defcustom dired-preview-chunk-size 10240
  "Size in bytes to read from large files."
  :group 'dired-preview
  :type 'natnum)

(defcustom dired-preview-binary-as-hexl t
  "Whether non-text (binary) files should be previewed in `hexl-mode'.

Irrespective of this option, you can switch between raw/hexl
views at any time using `dired-preview-hexl-toggle'."
  :group 'dired-preview
  :type 'boolean)

(defvar dired-preview--buffers nil
  "List with buffers of previewed files.")

(defvar dired-preview--large-files-alist nil
  "Alist mapping previewed large files to buffer names.")

(defun dired-preview--get-buffers ()
  "Return buffers that show previews."
  (seq-filter #'buffer-live-p dired-preview--buffers))

(defun dired-preview--window-parameter-p (window)
  "Return non-nil if WINDOW has `dired-preview-window' parameter."
  (window-parameter window 'dired-preview-window))

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
         (if (>= (dired-preview--get-buffer-cumulative-size)
                 dired-preview--buffers-threshold)
             (when (and (buffer-local-value 'delayed-mode-hooks buffer)
                        (not (eq buffer (current-buffer))))
               (ignore-errors (kill-buffer-if-not-modified buffer))
               (setq buffers (delq buffer buffers)))
           (throw 'stop t)))
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
  (when-let (((stringp dired-preview-ignored-extensions-regexp))
             (ext (file-name-extension file)))
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

(defun dired-preview--run-mode-hooks ()
  "Run mode hooks in current buffer, if `delayed-mode-hooks' is non-nil."
  (cond
   ((window-parameter (selected-window) 'dired-preview-window)
    (dired-preview--delete-windows))
   ((and delay-mode-hooks (current-buffer))
    (dired-preview--set-window-parameters (selected-window) nil)
    (apply #'run-hooks (delete-dups delayed-mode-hooks))
    (kill-local-variable 'delayed-mode-hooks)
    (remove-hook 'post-command-hook #'dired-preview--run-mode-hooks :local))))

(defun dired-preview--dispatch-file (file)
  "Decide how to preview FILE.

Return the preview buffer."
  (cond
   ((dired-preview--file-large-p file)
    (dired-preview--find-large-file file))
   (t
    (dired-preview--find-file-no-select file))))

(defun dired-preview--find-large-file (file)
  "Read part of FILE with appropriate settings.

The size of the leading chunk is specified by
`dired-preview-chunk-size'."
  (let ((inhibit-message t)
        (enable-dir-local-variables nil)
        (enable-local-variables :safe)
        (non-essential t)
        (delay-mode-hooks t))
    (if-let* ((buffer (or (get-file-buffer file)
                       (find-buffer-visiting file)
                       (alist-get file dired-preview--large-files-alist
                                  nil nil #'equal))))
        buffer ; Buffer is already being visited, we can reuse it
      (with-current-buffer (create-file-buffer file)
        ;; We create a buffer with a partial preview
        (buffer-disable-undo)
        (insert-file-contents file nil 1 dired-preview-chunk-size 'replace)
        (when (and (eq buffer-file-coding-system 'no-conversion)
                   dired-preview-binary-as-hexl)
          (hexl-mode))
        (dired-preview--add-truncation-message)
        (read-only-mode t)
        ;; Because this buffer is not marked as visiting FILE, we need to keep
        ;; track of it ourselves.
        (setf (alist-get file dired-preview--large-files-alist
                         nil nil 'equal)
              (current-buffer))))))

(defun dired-preview--add-truncation-message ()
  "Add a message indicating that the previewed file is truncated."
  (let ((end-ov (make-overlay (1- (point-max)) (point-max))))
    (overlay-put
     end-ov 'display
     (propertize "\n--PREVIEW TRUNCATED--" 'face 'shadow))))

(defun dired-preview-hexl-toggle ()
  "Toggle preview between text and `hexl-mode'."
  (interactive)
  (dolist (win (dired-preview--get-windows))
    (with-selected-window win
      (if (eq major-mode 'hexl-mode)
          (hexl-mode-exit)
        (hexl-mode))
      (dired-preview--add-truncation-message))))

(defun dired-preview--find-file-no-select (file)
  "Call `find-file-noselect' on FILE with appropriate settings."
  ;; NOTE: I learnt about `non-essential' and `delay-mode-hooks' from
  ;; Daniel Mendler's `consult' package, which imnplements a preview
  ;; functionality as well (more sophisticated than mine):
  ;; <https://github.com/minad/consult>.
  (let ((inhibit-message t)
        (enable-dir-local-variables nil)
        (enable-local-variables :safe)
        (non-essential t)
        (delay-mode-hooks t))
    (find-file-noselect file :nowarn)))

(defun dired-preview--add-to-previews (file)
  "Add FILE to `dired-preview--buffers', if not already in a buffer.
Always return FILE buffer."
  (let ((buffer (find-buffer-visiting file)))
    (if (buffer-live-p buffer)
        buffer
      (setq buffer (dired-preview--dispatch-file file)))
    (with-current-buffer buffer
      (add-hook 'post-command-hook #'dired-preview--run-mode-hooks nil :local))
    (add-to-list 'dired-preview--buffers buffer)
    buffer))

(defun dired-preview--get-preview-buffer (file)
  "Return buffer to preview FILE in."
  (dired-preview--add-to-previews file))

(defvar dired-preview-trigger-commands
  '(dired-next-line dired-previous-line dired-mark dired-unmark dired-unmark-backward dired-del-marker dired-goto-file dired-find-file)
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
  (dired-preview--kill-large-buffers))

(defun dired-preview--close-previews-outside-dired ()
  "Call `dired-preview--close-previews' if BUFFER is not in Dired mode."
  (unless (eq major-mode 'dired-mode)
    (dired-preview--close-previews)
    (remove-hook 'window-state-change-hook #'dired-preview--close-previews-outside-dired)
    (put 'dired-preview-start 'function-executed nil)))

(defun dired-preview--display-buffer (buffer)
  "Call `display-buffer' for BUFFER.
Only do it with the current major mode is Dired."
  (display-buffer buffer))

(defun dired-preview-display-file (file)
  "Display preview of FILE if appropriate."
  (dired-preview--delete-windows)
  (when-let ((buffer (dired-preview--get-preview-buffer file)))
    (dired-preview--display-buffer buffer)
    (when-let ((window (get-buffer-window buffer)))
      (dired-preview--set-window-parameters window t))))

(defun dired-preview--preview-p (file)
  "Return non-nil if FILE can be previewed."
  (and (file-regular-p file)
       (not (file-directory-p file))
       (not (dired-preview--file-displayed-p file))
       (not (dired-preview--file-ignored-p file))))

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
  (if-let ((file (dired-file-name-at-point))
           ((dired-preview--preview-p file))
           ((memq this-command dired-preview-trigger-commands)))
      (if no-delay
          (dired-preview-display-file file)
        (setq dired-preview--timer
              (run-with-idle-timer
               dired-preview-delay
               nil
               #'dired-preview-display-file
               file)))
    (if (and file (dired-preview--preview-p file))
	(dired-preview-start file)
      (if (not (memq this-command dired-preview-trigger-commands))
	  nil
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
