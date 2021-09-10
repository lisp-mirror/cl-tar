;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-simple-extract)

(define-condition unsupported-entry-error (extraction-entry-error)
  ())

(define-condition unsupported-fifo-entry-error (unsupported-entry-error)
  ())

(define-condition unsupported-symbolic-link-entry-error (unsupported-entry-error)
  ())

(define-condition unsupported-hard-link-entry-error (unsupported-entry-error)
  ())

(define-condition unsupported-block-device-entry-error (unsupported-entry-error)
  ())

(define-condition unsupported-character-device-entry-error (unsupported-entry-error)
  ())

(defun dereference-link (&optional c)
  (invoke-restart (find-restart 'dereference-link c)))
