;;; dired-preview-test.el --- Unit tests for Dired preview -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for Dired preview.

;;; Code:

(require 'ert)
(require 'dired-preview)

(ert-deftest dired-preview-test--dired-preview--file-ignored-p ()
  "Test that `dired-preview--file-ignored-p' returns non-nil for ignored file names."
  (let ((dired-preview-ignored-extensions-regexp
         (custom--standard-value 'dired-preview-ignored-extensions-regexp)))
    (dolist (ext '("mkv" "webm" "mp4" "mp3" "ogg" "m4a" "flac" "wav"
                   "gz" "zst" "tar" "xz" "rar" "zip" "iso" "epub" "pdf"))
      (should (dired-preview--file-ignored-p (format "example.%s" ext)))
      (should-not (dired-preview--file-ignored-p (format "example.%s_" ext)))))
  (let ((dired-preview-ignored-extensions-regexp "\\.DS_Store\\'"))
    (should (dired-preview--file-ignored-p ".DS_Store"))))

(ert-deftest dired-preview-test--dired-preview--infer-type ()
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
