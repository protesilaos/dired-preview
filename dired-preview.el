;;; dired-preview.el --- Automatically preview file at point in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/dired-preview
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.0.0
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
;; Enable the `dired-preview-mode' in the current Dired buffer and
;; then perform the regular up/down motions.  Those will trigger the
;; preview.
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
;;   without affecting the performance of Emacs.  Though `dired-preview'
;;   is designed to have no external dependencies, so such an ambition
;;   may not be realisable (e.g. produce a thumbnail out of a video).  ]
;;
;; To set up `dired-preview-mode' in every Dired buffer, set it up thus:
;;
;;     (add-hook 'dired-mode-hook #'dired-preview-mode)
;;
;; I took inspiration for `dired-preview' from the now unmaintained
;; `peep-dired' package by Adam Sokolnicki: <https://github.com/asok/peep-dired>.
;; My original plan was to volunteer to maintain `peep-dired` but I
;; decided to write it my own way: it was easier for me, plus I can
;; implement only what I consider necessary without upsetting existing
;; users.

;;; Code:

(require 'dired)

(defgroup dired-preview nil
  "Automatically preview file at point in Dired."
  :group 'dired)

(defcustom dired-preview-ignored-extensions-regexp
  (concat "\\."
          "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
          "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
          "\\|iso\\|epub\\|pdf\\)")
  "Regular expression of file type extensions to not preview."
  :group 'dired-preview
  :type 'string)

(defcustom dired-preview-max-size (expt 2 20)
  "Files larger than this byte limit are not previewed."
  :group 'dired-preview
  :type 'integer)

(defcustom dired-preview-buffer-name "*dired-preview*"
  "Name of preview buffer.
Used by the default value of `dired-preview-display-action-alist'."
  :group 'dired-preview
  :type 'string)

(defcustom dired-preview-display-action-alist
  `((display-buffer-in-side-window)
    (side . right)
    (slot . -1)
    (window-width . 0.3)
    (dedicated . t)
    (body-function . dired-preview-set-up-preview-window)
    (window-parameters . ((no-other-window . t)
                          (mode-line-format . ("%e"
                                               mode-line-front-space
                                               ,dired-preview-buffer-name)))))
  "The `display-buffer' action for the preview.
This is the same data that is passed to `display-buffer-alist'.
Read Info node `(elisp) Displaying Buffers'.  As such, it is
meant for experienced users.

To ensure best results, the `body-function' in the alist must be
set to `dired-preview-set-up-preview-window', as shown in this
user option's default value."
  :group 'dired-preview
  :type 'alist)

(defcustom dired-preview-delay 0.7
  "Time in seconds to wait before previewing."
  :group 'dired-preview
  :type 'float)

(defvar dired-preview--buffers nil
  "List with buffers of previewed files.")

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

(defun dired-preview--run-mode-hooks ()
  "Run mode hooks in current buffer, if `delayed-mode-hooks' is non-nil.
See `dired-preview--find-file-no-select' for how hooks are
delayed and `dired-preview-set-up-preview-window' for how this function
is used."
  (when (and delay-mode-hooks (current-buffer))
    (run-mode-hooks delayed-mode-hooks)
    (set-window-parameter (selected-window) 'dired-preview-window nil)
    (remove-hook 'post-command-hook #'dired-preview--run-mode-hooks :local)))

(defun dired-preview--add-to-previews (file)
  "Add FILE to `dired-preview--buffers', if not already in a buffer.
Always return FILE buffer."
  (let ((buffer (find-buffer-visiting file)))
    (if (buffer-live-p buffer)
        buffer
      (setq buffer (dired-preview--find-file-no-select file))
      (add-to-list 'dired-preview--buffers buffer)
      buffer)))

(defun dired-preview--get-buffers ()
  "Return buffers that show previews."
  (seq-filter
   (lambda (buffer)
     (when (and (bufferp buffer)
                (buffer-live-p buffer))
       buffer))
   dired-preview--buffers))

(defun dired-preview--get-windows ()
  "Return windows that show previews."
  (seq-filter
   (lambda (window)
     (when (window-parameter window 'dired-preview-window)
       window))
   (window-list)))

(defun dired-preview--file-ignored-p (file)
  "Return non-nil if FILE extension is among the ignored extensions.
See user option `dired-preview-ignored-extensions-regexp'."
  (when-let ((ext (file-name-extension file)))
    (string-match-p ext dired-preview-ignored-extensions-regexp)))

(defun dired-preview--file-large-p (file)
  "Return non-nil if FILE exceeds `dired-preview-max-size'."
  (> (file-attribute-size (file-attributes file)) dired-preview-max-size))

(defun dired-preview--file-displayed-p (file)
  "Return non-nil if FILE is already displayed in a window."
  (when-let* ((buffer (get-file-buffer file))
              (window (get-buffer-window buffer)))
    (window-live-p window)))

(defun dired-preview--preview-p (file)
  "Return non-nil if FILE can be previewed."
  (and (file-exists-p file)
       (not (file-directory-p file))
       (not (dired-preview--file-displayed-p file))
       (not (dired-preview--file-ignored-p file))
       (not (dired-preview--file-large-p file))))

(defun dired-preview--return-preview-buffer (file)
  "Return buffer to preview FILE in.
Determine the propriety of this action by checking that FILE
conforms with `dired-preview--preview-p'."
  (when (dired-preview--preview-p file)
    (dired-preview--add-to-previews file)))

(defun dired-preview--delete-windows ()
  "Delete preview windows."
  (mapc
   (lambda (window)
     (delete-window window))
   (dired-preview--get-windows)))

(defun dired-preview--kill-buffers ()
  "Kill preview buffers."
  (mapc
   (lambda (buffer)
     (when (and (not (eq buffer (current-buffer)))
                (window-parameter (get-buffer-window buffer) 'dired-preview-window)
                delayed-mode-hooks)
       (ignore-errors
         (kill-buffer-if-not-modified buffer))))
   (dired-preview--get-buffers))
  (setq dired-preview--buffers nil))

(defvar dired-preview--timer nil
  "Most recent timer object to display a preview.")

(defun dired-preview--close-previews ()
  "Kill preview buffers and delete their windows."
  (dired-preview--delete-windows)
  (dired-preview--kill-buffers))

(defun dired-preview--close-previews-outside-dired ()
  "Call `dired-preview--close-previews' if BUFFER is not in Dired mode."
  (when (not (eq major-mode 'dired-mode))
    (dired-preview--close-previews)))

(defun dired-preview-set-up-preview-window (window &rest _)
  "Set WINDOW `:preview' parameter.
Use this as the `body-function' in the user option
`dired-preview-display-action-alist'."
  (set-window-parameter window 'dired-preview-window :preview)
  (with-current-buffer (window-buffer window)
    (add-hook 'post-command-hook #'dired-preview--close-previews-outside-dired nil :local)
    (add-hook 'post-command-hook #'dired-preview--run-mode-hooks nil :local)))

(defun dired-preview--display-buffer (buffer)
  "Call `display-buffer' for BUFFER.
Only do it with the current major mode is Dired."
  ;; We check for `dired-mode' because we want to avoid the scenario
  ;; where the user switches to another buffer/window/frame before the
  ;; timer elapses.
  (when (eq major-mode 'dired-mode)
    (display-buffer
     buffer
     dired-preview-display-action-alist)))

(defun dired-preview--cancel-timer ()
  "Cancel `dired-preview--timer' if it is a timer object."
  (when (timerp dired-preview--timer)
    (cancel-timer dired-preview--timer)))

(defun dired-preview--display-buffer-with-delay (buffer)
  "Display BUFFER with `dired-preview-delay'."
  (dired-preview--cancel-timer)
  (setq dired-preview--timer
        (run-with-timer dired-preview-delay nil
                        #'dired-preview--display-buffer buffer)))

(defun dired-preview-display-file (&rest _)
  "Display preview of `dired-file-name-at-point' if appropriate.
Return buffer object of displayed buffer.  Ignore any arguments.

Use this as advice after relevant Dired commands (see
`dired-preview-enable-preview', `dired-preview-disable-preview')."
  (if-let* ((file (dired-file-name-at-point))
            (buffer (dired-preview--return-preview-buffer file)))
        (dired-preview--display-buffer-with-delay buffer)
    (dired-preview--close-previews)))

(defun dired-preview-disable-preview ()
  "Disable preview."
  (unless (eq major-mode 'dired-mode)
    (error "Can only use `dired-preview' in Dired"))
  (dolist (command '(dired-next-line dired-previous-line dired-mark dired-goto-file))
    (advice-remove command #'dired-preview-display-file))
  (dired-preview--close-previews))

(defun dired-preview-enable-preview ()
  "Enable preview and store window configuration."
  (unless (eq major-mode 'dired-mode)
    (error "Can only use `dired-preview' in Dired"))
  (dolist (command '(dired-next-line dired-previous-line dired-mark dired-goto-file))
    (advice-add command :after #'dired-preview-display-file))
  (add-hook 'post-command-hook #'dired-preview--close-previews-outside-dired nil :local)
  (dired-preview-display-file))

;;;###autoload
(define-minor-mode dired-preview-mode
  "Buffer-local mode to preview file at point in Dired."
  :global nil
  (if dired-preview-mode
      (dired-preview-enable-preview)
    (dired-preview-disable-preview)))

(provide 'dired-preview)
;;; dired-preview.el ends here
