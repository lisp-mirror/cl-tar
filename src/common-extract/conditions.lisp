;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-common-extract)

(define-condition extraction-error (error)
  ())

(define-condition extraction-entry-error (extraction-error)
  ((entry
    :initarg :entry
    :reader extraction-entry-error-entry)))

(define-condition entry-name-contains-device-error (extraction-entry-error)
  ())

(define-condition entry-name-contains-..-error (extraction-entry-error)
  ())

(define-condition entry-name-is-absolute-error (extraction-entry-error)
  ())

(defun skip-entry (&optional c)
  (invoke-restart (find-restart 'skip-entry c)))

(defun relativize-entry-name (&optional c)
  (invoke-restart (find-restart 'relativize-entry-name c)))

(defun treat-..-as-back (&optional c)
  (invoke-restart (find-restart 'treat-..-as-back c)))
