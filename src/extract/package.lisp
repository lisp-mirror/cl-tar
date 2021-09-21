;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(uiop:define-package #:tar-extract
    (:use :cl
          #:tar-common-extract)
  ;; From TAR-COMMON-EXTRACT
  (:export
   #:extraction-error

   #:extraction-entry-error
   #:extraction-entry-error-entry

   #:entry-name-contains-device-error

   #:entry-name-contains-..-error

   #:entry-name-is-absolute-error

   #:skip-entry
   #:relativize-entry-name
   #:treat-..-as-back)

  ;; conditions
  (:export
   #:extraction-through-symbolic-link-error)

  (:export
   #:extract-archive))
