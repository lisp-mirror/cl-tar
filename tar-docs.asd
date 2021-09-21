;;; -*- mode: lisp -*-

(asdf:defsystem #:tar-docs
  :version "0.0.1"
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Documentation system for tar."
  :license "MIT"
  :depends-on ("tar" "40ants-doc" "tar-simple-extract" "tar-extract")
  :pathname "src/"
  :components ((:file "docs")))
