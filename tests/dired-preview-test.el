;;; dired-preview-test.el --- Unit tests for Dired preview -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

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

;; Tests for Dired preview.  Note that we are using Shorthands in this
;; file, so the "dpt-" prefix really is "denote-preview-test-".
;; Evaluate the following to learn more:
;;
;;    (info "(elisp) Shorthands")

;;; Code:

(require 'ert)
(require 'dired-preview)

(ert-deftest dpt--dired-preview--file-ignored-p ()
  "Test that `dired-preview--file-ignored-p' returns non-nil for ignored file names."
  (let ((dired-preview-ignored-extensions-regexp
         (custom--standard-value 'dired-preview-ignored-extensions-regexp)))
    (dolist (ext '("mkv" "webm" "mp4" "mp3" "ogg" "m4a" "flac" "wav"
                   "gz" "zst" "tar" "xz" "rar" "zip" "iso" "epub" "pdf"))
      (should (dired-preview--file-ignored-p (format "example.%s" ext)))
      (should-not (dired-preview--file-ignored-p (format "example.%s_" ext)))))
  (let ((dired-preview-ignored-extensions-regexp "\\.DS_Store\\'"))
    (should (dired-preview--file-ignored-p ".DS_Store"))))

(ert-deftest dpt--dired-preview--infer-type ()
  "Test that `dired-preview--infer-type' infers the correct file type."
  (let ((dired-preview-ignored-extensions-regexp
         (custom--standard-value 'dired-preview-ignored-extensions-regexp)))
    (dolist (ext '("mkv" "webm" "mp4" "mp3" "ogg" "m4a" "flac" "wav"
                   "gz" "zst" "tar" "xz" "rar" "zip" "iso" "epub" "pdf"))
      (should (eq 'ignore
                  (car (dired-preview--infer-type
                        (format "example.%s" ext)))))
      (should-not (eq 'ignore
                      (car (dired-preview--infer-type
                            (format "example.%s_" ext)))))))
  (let ((dired-preview-ignored-extensions-regexp "\\.DS_Store\\'"))
    (should (eq 'ignore
                (car (dired-preview--infer-type ".DS_Store"))))))

(provide 'dired-preview-test)
;;; dired-preview-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("dpt" . "dired-preview-test-"))
;; End:
