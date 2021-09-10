;;;; tar archives
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defclass archive ()
  ((opened-stream
    :initarg :opened-stream
    :reader archive-opened-stream)
   (file
    :initarg :file
    :reader archive-file)
   (direction
    :initarg :direction
    :reader archive-direction)))

(defgeneric read-entry (archive))

(defmethod read-entry ((archive archive))
  (let ((raw-entry (tar-file:read-entry (archive-file archive))))
    (unless (null raw-entry)
      (convert-from-physical-entry archive raw-entry))))

(defgeneric convert-from-physical-entry (archive physical-entry &rest overrides))

(defgeneric archive-supports-property-p (archive property))

(defmethod archive-supports-property-p ((archive archive) property)
  (not (null (member property '(name mode uid gid mtime size devmajor devminor linkname)))))

(defgeneric archive-type-to-tar-file-type (type)
  (:method ((type (eql :auto)))
    type)
  (:method ((type (eql 'v7-archive)))
    'tar-file:v7-tar-file)
  (:method ((type (eql 'ustar-archive)))
    'tar-file:ustar-tar-file)
  (:method ((type (eql 'pax-archive)))
    'tar-file:ustar-tar-file)
  (:method ((type (eql 'gnu-archive)))
    'tar-file:gnu-tar-file))

(defgeneric tar-file-to-archive-type (tar-file)
  (:method ((tar-file tar-file:v7-tar-file))
    'v7-archive)
  (:method ((tar-file tar-file:ustar-tar-file))
    "We don't know if this will ever have PAX headers, so upgrade to PAX-ARCHIVE."
    'pax-archive)
  (:method ((tar-file tar-file:gnu-tar-file))
    'gnu-archive))

(defun open-archive (stream-or-path
                     &key
                       (type :auto)
                       (direction :input)
                       (if-exists nil if-exists-supplied-p)
                       (if-does-not-exist nil if-does-not-exist-supplied-p)
                       (blocking-factor 20)
                       (header-encoding tar-file:*default-header-encoding*))
  (assert (member direction '(:input :output)))
  (let* ((stream (if (streamp stream-or-path)
                     stream-or-path
                     (apply #'open
                            stream-or-path
                            :direction direction
                            :element-type '(unsigned-byte 8)
                            (append
                             (when if-exists-supplied-p
                               (list :if-exists if-exists))
                             (when if-does-not-exist-supplied-p
                               (list :if-does-not-exist if-does-not-exist))))))
         (tar-file-type (archive-type-to-tar-file-type type))
         (tar-file (tar-file:open-tar-file stream
                                           :type tar-file-type
                                           :direction direction
                                           :blocking-factor blocking-factor
                                           :header-encoding header-encoding))
         (archive-type (if (eql type :auto)
                           (tar-file-to-archive-type tar-file)
                           type)))
    (make-instance archive-type :file tar-file
                                :direction direction
                                :opened-stream (unless (streamp stream-or-path)
                                                 stream))))
(defgeneric close-archive (archive))

(defmethod close-archive ((archive archive))
  (when (eql (archive-direction archive) :output)
    (tar-file:finalize-tar-file (archive-file archive)))
  (tar-file:close-tar-file (archive-file archive))
  (unless (null (archive-opened-stream archive))
    (close (archive-opened-stream archive))))

(defun call-with-open-archive (thunk stream-or-path
                               &rest args
                               &key
                                 type direction
                                 if-exists if-does-not-exist
                                 blocking-factor header-encoding)
  (declare (ignore type direction if-exists if-does-not-exist blocking-factor header-encoding))
  (let ((archive (apply #'open-archive stream-or-path args)))
    (unwind-protect (funcall thunk archive)
      (close-archive archive))))

(defmacro with-open-archive ((archive-var stream-or-path
                              &rest args
                              &key
                                type direction
                                if-exists if-does-not-exist
                                blocking-factor header-encoding)
                             &body body)
  (declare (ignore type direction if-exists if-does-not-exist blocking-factor header-encoding))
  `(call-with-open-archive
    (lambda (,archive-var) ,@body)
    ,stream-or-path
    ,@args))

(defmacro do-entries ((entry archive &optional result)
                      &body body)
  "Iterate over the entries in ARCHIVE. For each entry, ENTRY is bound to an
ENTRY representing the entry. RESULT is returned."
  (let ((archive-var (gensym)))
    `(let ((,archive-var ,archive))
       (do ((,entry (read-entry ,archive-var)
                    (read-entry ,archive-var)))
           ((null ,entry) ,result)
         ,@body))))
