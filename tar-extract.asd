;;; -*- mode: lisp -*-

(asdf:defsystem #:tar-extract
  :version "0.0.1"
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A system to extract tar archives to the filesystem."
  :license "MIT"
  :depends-on ("alexandria" "babel" "cffi" "file-attributes" "local-time" "osicat" "split-sequence" "tar" "uiop" "40ants-doc" "tar-common-extract")
  :pathname "src/extract"
  :in-order-to ((test-op (load-op "tar-extract/test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-extract-test)))
                      (error "Tests failed")))
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "fd-stream" :depends-on ("package"))
               (:file "extract" :depends-on ("package" "conditions"))))

(defsystem #:tar-extract/test
  :pathname "test/extract"
  :components ((:file "package")
               (:file "extract" :depends-on ("package"))
               (:file "stress" :depends-on ("package")))
  :depends-on (#:tar-extract #:parachute))
