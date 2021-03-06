;;; -*- mode: lisp -*-

(asdf:defsystem #:tar-simple-extract
  :version "0.1.1"
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A system to extract tar archives to the filesystem using only portable CL constructs."
  :license "MIT"
  :depends-on ("tar" "tar-common-extract" "uiop" "local-time")
  :pathname "src/simple-extract"
  :in-order-to ((test-op (load-op "tar-simple-extract/test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-simple-extract-test)))
                      (error "Tests failed")))
  :components ((:file "package")
               (:file "extract" :depends-on ("package"))))

(defsystem #:tar-simple-extract/test
  :pathname "test/simple-extract"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "v7" :depends-on ("package" "utils"))
               (:file "stress" :depends-on ("package" "utils")))
  :depends-on ("tar-simple-extract" "parachute" "osicat"))
