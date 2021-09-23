;;; -*- mode: lisp -*-

(asdf:defsystem #:tar-extract
  :version "0.1.1"
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A system to extract tar archives to the filesystem."
  :license "MIT"
  :depends-on ("alexandria" "local-time" "osicat" "tar" "uiop" "tar-common-extract")
  :pathname "src/extract"
  :in-order-to ((test-op (load-op "tar-extract/test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-extract-test)))
                      (error "Tests failed")))
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "features" :depends-on ("package"))
               (:file "utils" :depends-on ("package" "features"))
               (:file "fd-stream" :depends-on ("package" "utils" "features"))
               (:file "extract" :depends-on ("package" "conditions" "utils" "features"))))

(defsystem #:tar-extract/test
  :pathname "test/extract"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "extract" :depends-on ("package" "utils"))
               (:file "stress" :depends-on ("package" "utils")))
  :depends-on ("tar-extract" "parachute"))
