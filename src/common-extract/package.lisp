;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(uiop:define-package #:tar-common-extract
    (:use :cl)
  ;; Conditions
  (:export
   #:extraction-error

   #:extraction-entry-error
   #:extraction-entry-error-entry

   #:entry-name-contains-device-error

   #:entry-name-contains-..-error

   #:entry-name-is-absolute-error

   #:unsupported-entry-error
   #:unsupported-character-device-entry-error
   #:unsupported-block-device-entry-error
   #:unsupported-fifo-entry-error
   #:unsupported-symbolic-link-entry-error
   #:unsupported-hard-link-entry-error

   #:dereference-link

   #:skip-entry
   #:relativize-entry-name
   #:treat-..-as-back)

  ;; Utils
  (:export
   #:pathname-parent-directory-pathname)

  ;; pathname computation
  (:export
   #:compute-extraction-pathname))
