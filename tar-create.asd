;;; -*- mode: lisp -*-

(asdf:defsystem #:tar-create
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A system to create tar archives from the filesystem."
  :license "MIT"
  :depends-on ("tar" "uiop" "local-time" "osicat")
  :pathname "src/create/"
  :in-order-to ((test-op (load-op "tar-create/test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-create-test)))
                      (error "Tests failed")))
  :components ((:file "package")
               (:file "create" :depends-on ("package"))
               (:file "posix" :depends-on ("create") :if-feature (:not :windows))
               (:file "windows" :depends-on ("create") :if-feature :windows)))

(defsystem #:tar-create/test
  :pathname "test/create"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "v7" :depends-on ("package" "utils"))
               (:file "ustar" :depends-on ("package" "utils"))
               (:file "pax" :depends-on ("package" "utils"))
               ;;(:file "gnu" :depends-on ("package" "utils"))
               )
  :depends-on ("tar-create" "parachute" "osicat"))
