;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)

(define-condition destination-exists (extraction-entry-error file-error)
  ((mtime
    :initarg :mtime
    :reader destination-exists-mtime)))

(define-condition extraction-through-symbolic-link-error (extraction-entry-error)
  ((target
    :initarg :target)
   (pathname
    :reader extraction-through-symbolic-link-error-pathname
    :initarg :pathname)))

(define-condition file-exists-in-place-of-directory (extraction-entry-error)
  ((pathname
    :initarg :pathname)))

(define-condition destination-is-symbolic-link-error (extraction-error)
  ((target
    :initarg :target)))

(defun follow-symbolic-link (&optional c)
  (invoke-restart (find-restart 'follow-symbolic-link c)))

(defun replace-symbolic-link (&optional c)
  (invoke-restart (find-restart 'replace-symbolic-link c)))
