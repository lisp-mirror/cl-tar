;;;; Documentation
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(40ants-doc:defsection @manual (:title "Tar Manual"
                                :export nil)
  #.(uiop:read-file-string (asdf:system-relative-pathname :tar-file "README.md")))
