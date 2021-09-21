;;; -*- mode: lisp -*-

(asdf:defsystem #:tar
  :version "0.0.1"
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A high level interface for tar archives"
  :license "MIT"
  :depends-on ("alexandria" "babel" "local-time" "split-sequence" "tar-file" "uiop" "40ants-doc")
  :pathname "src"
  :in-order-to ((test-op (load-op "tar/test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-test)))
                      (error "Tests failed")))
  :components ((:file "archive" :depends-on ("package"))
               (:file "conditions" :depends-on ("package"))
               (:file "entry" :depends-on ("package" "utils"))
               (:file "gnu-archive" :depends-on ("package" "archive" "utils" "validation"))
               (:file "package")
               (:file "pax-archive" :depends-on ("package" "archive" "utils" "validation"))
               (:file "ustar-archive" :depends-on ("package" "archive" "utils" "validation"))
               (:file "utils" :depends-on ("package"))
               (:file "v7-archive" :depends-on ("package" "archive" "validation"))
               (:file "validation" :depends-on ("package" "entry"))))

(defsystem #:tar/test
  :pathname "test"
  :components ((:file "package")
               (:file "read" :depends-on ("package" "shared"))
               (:file "shared" :depends-on ("package"))
               (:file "write" :depends-on ("package" "shared")))
  :depends-on (#:tar #:parachute))
