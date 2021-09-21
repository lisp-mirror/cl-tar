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
                        (return-from open-directory-handle-1
                          (open-directory-handle start-handle (uiop:ensure-directory-pathname target))))
                      (replace-symbolic-link ()
                        (nix:unlinkat start-handle (first directory-list) 0)
                        (return-from open-directory-handle-1
                          (open-directory-handle-1 start-handle directory-list))))))))
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
                       (openat dir-handle name mode)
                     (destination-is-symbolic-link-error () nil))
    :when stream
      :return (values stream name)))

(defun openat (dir-handle pathname mode)
  (let ((dirfd (open-directory-handle dir-handle
                                      (uiop:pathname-directory-pathname pathname)))
        flags
        (name (file-namestring pathname))
        stat)
    (if (or (null name)
            (equal name ""))
        (make-instance 'fd-stream :fd dirfd)
        (unwind-protect
             (tagbody
              :retry
                (setf flags (logior nix:o-wronly
                                    nix:o-creat
                                    nix:o-nofollow))
                (handler-case
                    (setf stat (nix:fstatat dirfd name nix:at-symlink-nofollow))
                  ;; If the file doesn't seem to exist, add O_EXCL to our flags and
                  ;; try to open it. The O_EXCL ensures we get an error if the file
                  ;; is created between the stat and open calls
                  (nix:enoent ()
                    (setf flags (logior flags nix:o-excl))
                    (go :open)))
                (cond
                  ;; The file exists and is a symlink.
                  ((nix:s-islnk (nix:stat-mode stat))
                   (let (target)
                     ;; Try reading where it points to, so we can ask the user what
                     ;; to do.
                     (handler-case
                         (setf target (uiop:parse-unix-namestring
                                       (nix:readlinkat dirfd name)
                                       :dot-dot :back))
                       ;; The link got deleted between the stat and readlink
                       ;; calls. Just retry from scratch.
                       (nix:einval () (go :retry)))
                     (restart-case
                         (error 'destination-is-symbolic-link-error
                                :target target)
                       ;; Follow the symlink! We resolve the symlink destination
                       ;; ourselves. This is because our API tells the user where
                       ;; the symlink points and POSIX has no way to say "follow the
                       ;; symlink, but only if it points to X still" (well, Linux
                       ;; sort of does, but not Darwin nor BSD (pass a file
                       ;; descriptior to readlinkat, not a dirfd))
                       (follow-symbolic-link ()
                         (return-from openat
                           (openat dirfd target mode)))
                       ;; Replace the symbolic link! Create a temporary file, rename
                       ;; it on top of the symlink, and then return a stream to the
                       ;; new file. This ensures that the link is atomically
                       ;; replaced.
                       (replace-symbolic-link ()
                         (multiple-value-bind (stream tmp-name)
                             (openat-random dirfd name mode)
                           (nix:renameat dirfd tmp-name dirfd name)
                           (return-from openat stream))))))
                  ;; File exists, but is not a symlink. Ask the user what to do.
                  (t
                   (restart-case
                       (error 'destination-exists
                              :mtime (local-time:unix-to-timestamp (nix:stat-mtime stat)
                                                                   :nsec (nix:stat-mtime-nsec stat))
                              :pathname pathname)
                     ;; User wants us to overwrite it. So add O_TRUNC to the flags
                     ;; and get going.
                     (supersede-file ()
                       (setf flags (logior flags nix:o-trunc))
                       (go :open))
                     ;; User wants us to rename and replace the file. This keeps
                     ;; processes that already have the file open happier. Take the
                     ;; same approach as replacing a symlink, make a new file and
                     ;; rename it.
                     (rename-and-replace-file ()
                       (multiple-value-bind (stream tmp-name) (openat-random dirfd name mode)
                         (nix:renameat dirfd tmp-name dirfd name)
                         (return-from openat stream))))))
              :open
                ;; Try opening the file!
                (handler-case
                    (return-from openat
                      (make-instance 'fd-output-stream :fd (nix:openat dirfd name flags mode)))
                  ;; Someone snuck in and created a file between the stat and open!
                  (nix:eexist () (go :retry))
                  ;; Someone snuck in and made a symlink on us!
                  (nix:eloop () (go :retry))))
          (unless (eql dir-handle dirfd)
            (nix:close dirfd))))))
