;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)

(defun call-with-fd (thunk fd)
  (unwind-protect
       (funcall thunk fd)
    (nix:close fd)))

(defmacro with-fd ((fd &optional (default fd))
                   &body body)
  `(call-with-fd (lambda (,fd) ,@body) ,default))
