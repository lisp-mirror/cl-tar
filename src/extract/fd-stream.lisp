;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)


(define-condition fd-stream-error (stream-error)
  ())

(define-condition simple-fd-stream-error (fd-stream-error simple-error)
  ())

(defclass fd-stream (trivial-gray-streams:trivial-gray-stream-mixin
                     trivial-gray-streams:fundamental-binary-stream)
  ((fd
    :initarg :fd
    :reader fd
    :documentation
    "The underlying file descriptor.")
   (close-hooks
    :initform nil
    :accessor close-hooks)))

(defclass fd-output-stream (fd-stream
                            trivial-gray-streams:fundamental-binary-output-stream)
  ()
  (:documentation
   "A FD-STREAM used for output."))

(defmethod trivial-gray-streams:stream-write-byte ((stream fd-output-stream) byte)
  (cffi:with-foreign-array (buf (make-array 1 :element-type '(unsigned-byte 8)
                                              :initial-element byte)
                                '(unsigned-byte 8))
    (nix:write (fd stream) buf 1)
    byte))

(defmethod trivial-gray-streams:stream-write-sequence ((stream fd-output-stream)
                                                       sequence start end
                                                       &key &allow-other-keys)
  (let ((num-bytes (- (or end (length sequence)) start)))
    (cffi:with-foreign-array (buf (subseq sequence start end) `(:array :char ,num-bytes))
      (nix:write (fd stream) buf num-bytes)))
  sequence)

(defmethod stream-element-type ((stream fd-stream))
  '(unsigned-byte 8))

(defmethod close ((stream fd-stream) &key abort)
  (declare (ignore abort))
  (mapc #'funcall (close-hooks stream))
  (nix:close (fd stream)))

(defun get-directory-handle (directory-pn start-handle)
  (let ((directory-components (pathname-directory directory-pn))
        (start-handle start-handle)
        (close-handle-p nil))
    (when (eql :absolute (pop directory-components))
      (setf start-handle (nix:open "/" (logior nix:o-rdonly
                                               nix:o-nofollow))
            close-handle-p t))
    (dolist (component directory-components)
      ;; Try making the directory if it does not exist.
      (let ((last-handle start-handle))
        (unwind-protect
             (progn
               (handler-case
                   (nix:mkdirat start-handle component (logior nix:s-irwxg
                                                               nix:s-irwxo
                                                               nix:s-irwxu))
                 (nix:eexist ()))
               (handler-bind
                   ((nix:eloop
                      (lambda (c)
                        (declare (ignore c))
                        (restart-case
                            (error 'directory-is-symbolic-link-error)
                          (continue ()
                            (setf start-handle (nix:openat start-handle component nix:o-rdonly
                                                           0)))))))
                 (setf start-handle (nix:openat start-handle component (logior nix:o-rdonly
                                                                               nix:o-nofollow)
                                                0))))
          (if close-handle-p
              (nix:close last-handle)
              (setf close-handle-p t)))))
    start-handle))

(defun open-directory-handle-1 (start-handle directory-list)
  (when (null directory-list)
    (return-from open-directory-handle-1 start-handle))
  (unwind-protect
       (progn
         (handler-case
             (nix:mkdirat start-handle (first directory-list) nix:s-irwxu)
           (nix:eexist ()))
         (handler-bind
             ((nix:eloop
                (lambda (c)
                  (declare (ignore c))
                  (let ((target (uiop:parse-unix-namestring (nix:readlinkat start-handle
                                                                            (first directory-list))
                                                            :dot-dot :back)))
                    (restart-case
                        (error 'directory-is-symbolic-link-error
                               :target target)
                      (follow-symbolic-link ()
                        (open-directory-handle start-handle target))
                      (replace-symbolic-link ()
                        (nix:unlinkat start-handle (first directory-list) 0)
                        (open-directory-handle-1 start-handle directory-list)))))))
           (let ((flags (logior nix:o-rdonly nix:o-nofollow)))
             (open-directory-handle-1 (nix:openat start-handle (first directory-list) flags 0)
                                      (rest directory-list)))))))

(defun open-directory-handle (start-handle directory-pn)
  (let ((directory-components (pathname-directory directory-pn)))
    (if (null directory-components)
        start-handle
        (ecase (pop directory-components)
          (:absolute
           (open-directory-handle-1 (nix:open "/" nix:o-rdonly) directory-components))
          (:relative
           (open-directory-handle-1 start-handle directory-components))))))

(defun openat-random (dir-handle pathname mode)
  (loop
    :for random := (random 1000000)
    :for name := (concatenate 'string "." (file-namestring pathname)
                              "." (princ-to-string random))
    :for stream := (handler-case
                       (openat dir-handle pathname mode
                               :if-exists nil)
                     (destination-is-symbolic-link-error () nil))
    :when stream
      :return (values stream name)))

(defun openat (dir-handle pathname mode
               &rest args
               &key if-exists
                 follow-file-symbolic-links)
  (let ((parent-dir-handle (open-directory-handle dir-handle
                                                  (uiop:pathname-directory-pathname pathname)))
        (flags (logior nix:o-wronly
                       nix:o-creat))
        (name (file-namestring pathname)))
    (unless follow-file-symbolic-links
      (setf flags (logior flags nix:o-nofollow)))
    (case if-exists
      (:supersede
       (setf flags (logior flags nix:o-trunc)))
      (t
       (setf flags (logior flags nix:o-excl))))

    (if (or (null name)
            (equal name ""))
        (make-instance 'fd-stream :fd parent-dir-handle)
        (unwind-protect
             (handler-case
                 (make-instance 'fd-output-stream :fd (nix:openat parent-dir-handle name flags mode))
               (nix:eexist ()
                 (restart-case
                     (error 'destination-exists)
                   (supersede-file ()
                     (apply #'openat parent-dir-handle name mode :if-exists :supersede args))
                   (rename-and-replace-file ()
                     (multiple-value-bind (stream tmp-name) (openat-random parent-dir-handle name mode)
                       (push (lambda ()
                               (nix:renameat parent-dir-handle tmp-name parent-dir-handle name))
                             (close-hooks stream))
                       stream))))
               (nix:eloop ()
                 (let ((target (uiop:parse-unix-namestring (nix:readlinkat parent-dir-handle name)
                                                           :dot-dot :back)))
                   (restart-case
                       (error 'destination-is-symbolic-link-error
                              :target target)
                     (follow-symbolic-link ()
                       (apply #'openat parent-dir-handle target mode :follow-file-symbolic-links t
                              args))
                     (replace-symbolic-link ()
                       (multiple-value-bind (stream tmp-name) (openat-random parent-dir-handle name mode)
                         (push (lambda ()
                                 (nix:renameat parent-dir-handle tmp-name parent-dir-handle name))
                               (close-hooks stream))
                         stream))))))
          (unless (eql dir-handle parent-dir-handle)
            (nix:close parent-dir-handle))))))
