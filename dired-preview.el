;;; dired-preview.el --- Automatically preview file at point in Dired -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
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

(defcustom dired-preview-display-action-alist-function
  #'dired-preview-display-action-alist-dwim
  "Function to return the `display-buffer' action for the preview.
This is the same data that is passed to `display-buffer-alist'.
Read Info node `(elisp) Displaying Buffers'.  As such, it is
meant for experienced users.  See the reference function
`dired-preview-display-action-alist-dwim' for the implementation
details."
  :group 'dired-preview
  :type 'function)

(defcustom dired-preview-delay 0.7
  "Time in seconds to wait before previewing."
  :group 'dired-preview
  :type 'float)

(defvar dired-preview--buffers nil
  "List with buffers of previewed files.")

(defun dired-preview--get-buffers ()
  "Return buffers that show previews."
  (seq-filter
   (lambda (buffer)
     (when (and (bufferp buffer)
                (buffer-live-p buffer))
       buffer))
   dired-preview--buffers))

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
         (if (and (>= (dired-preview--get-buffer-cumulative-size)
                      dired-preview--buffers-threshold))
             (when (and (buffer-local-value 'delayed-mode-hooks buffer)
                        (not (eq buffer (current-buffer))))
               (ignore-errors (kill-buffer-if-not-modified buffer))
               (setq buffers (delq buffer buffers)))
           (throw 'stop t)))
       buffers))
    (setq dired-preview--buffers (delq nil (nreverse buffers)))))

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
      (setq buffer (dired-preview--find-file-no-select file)))
    (with-current-buffer buffer
      (add-hook 'post-command-hook #'dired-preview--run-mode-hooks nil :local))
    (add-to-list 'dired-preview--buffers buffer)
    buffer))

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
  (if-let* ((width (window-body-width))
            ((>= width (window-body-height)))
            ((>= width split-width-threshold)))
      `(:side right :dimension window-width :size ,(dired-preview-get-window-size :width))
    `(:side bottom :dimension window-height :size ,(dired-preview-get-window-size :height))))

(defun dired-preview-display-action-alist-dwim ()
  "Reference function for `dired-preview-display-action-alist-function'.
Return a `display-buffer' action alist, as described in the
aforementioned user option."
  (let ((properties (dired-preview-display-action-side)))
    `((display-buffer-in-side-window)
      (side . ,(plist-get properties :side))
      (,(plist-get properties :dimension) . ,(plist-get properties :size)))))

(defvar dired-preview-trigger-commands
  '(dired-next-line dired-previous-line dired-mark dired-goto-file)
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
  (dired-preview--kill-buffers))

(defun dired-preview--close-previews-outside-dired ()
  "Call `dired-preview--close-previews' if BUFFER is not in Dired mode."
  (unless (eq major-mode 'dired-mode)
    (dired-preview--close-previews)
    (remove-hook 'window-state-change-hook #'dired-preview--close-previews-outside-dired)))

(defun dired-preview--display-buffer (buffer)
  "Call `display-buffer' for BUFFER.
Only do it with the current major mode is Dired."
  (display-buffer
   buffer
   (funcall (or dired-preview-display-action-alist-function
                #'dired-preview-display-action-alist-dwim))))

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
       (not (dired-preview--file-ignored-p file))
       (not (dired-preview--file-large-p file))))

(defun dired-preview-trigger (&optional no-delay)
  "Trigger display of file at point after `dired-preview-trigger-commands'.
With optional NO-DELAY do not start a timer.  Otherwise produce
the preview with `dired-preview-delay' of idleness."
  (add-hook 'window-state-change-hook #'dired-preview--close-previews-outside-dired)
  (dired-preview--cancel-timer)
  (if-let* ((file (dired-file-name-at-point))
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
    (dired-preview--close-previews-outside-dired)))

(defun dired-preview-disable-preview ()
  "Disable Dired preview."
  (unless (eq major-mode 'dired-mode)
    (error "Can only use `dired-preview' in Dired"))
  (remove-hook 'post-command-hook #'dired-preview-trigger :local)
  (dired-preview--close-previews))

(defun dired-preview-enable-preview ()
  "Enable Dired preview."
  (unless (eq major-mode 'dired-mode)
    (error "Can only use `dired-preview' in Dired"))
  (add-hook 'post-command-hook #'dired-preview-trigger nil :local)
  (dired-preview-trigger :no-delay))

;;;###autoload
(define-minor-mode dired-preview-mode
  "Buffer-local mode to preview file at point in Dired."
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
