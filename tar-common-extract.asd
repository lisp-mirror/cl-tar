;;; -*- mode: lisp -*-

(asdf:defsystem #:tar-common-extract
  :version "0.1.0"
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Common utilities for tar-simple-extract and tar-extract."
  :license "MIT"
  :depends-on ("split-sequence" "tar" "uiop")
  :pathname "src/common-extract"
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "utils" :depends-on ("package"))
               (:file "pathname-computation" :depends-on ("package"))))
