;;;; tar file entries
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defmacro define-entry-property (name type)
  `(progn
     (defgeneric ,name (entry)
       (:documentation
        ,(format nil "The ~A of ENTRY. Returns a ~A."
                 (string-downcase (string name))
                 type)))
     (defgeneric (setf ,name) (value entry)
       (:documentation
        ,(format nil "Set the ~A of ENTRY. VALUE must be a ~A."
                 (string-downcase (string name))
                 type)))))

(defmacro define-optional-entry-property (name type)
  `(progn
     (defgeneric ,name (entry)
       (:documentation
        ,(format nil "The ~A of ENTRY. Returns a ~A."
                 (string-downcase (string name))
                 type))
       (:method :around (entry)
         (if (archive-supports-property-p (archive entry) ',name)
             (call-next-method)
             (restart-case
                 (error 'unsupported-property :name ',name)
               (use-value (value)
                 :interactive (lambda ()
                                (format *query-io* "Enter a new value (unevaluated): ")
                                (read *query-io*))
                 value)
               (ignore-unsupported-property ()
                 nil)))))
     (defgeneric (setf ,name) (value entry)
       (:documentation
        ,(format nil "Set the ~A of ENTRY. VALUE must be a ~A."
                 (string-downcase (string name))
                 type))
       (:method :around (value entry)
         (if (archive-supports-property-p (archive entry) ',name)
             (call-next-method)
             (restart-case
                 (error 'unsupported-property :name ',name)
               (ignore-unsupported-property ()
                 value)))))))

(define-entry-property name string)
(define-entry-property mode list)
(define-entry-property uid (integer 0))
(define-entry-property gid (integer 0))
(define-entry-property mtime local-time:timestamp)
(define-entry-property size (integer 0))
(define-entry-property devmajor (integer 0))
(define-entry-property devminor (integer 0))
(define-entry-property linkname (integer 0))

(define-optional-entry-property uname string)
(define-optional-entry-property gname string)
(define-entry-property atime local-time:timestamp)
(define-entry-property ctime local-time:timestamp)

(defclass entry ()
  ((archive
    :initarg :archive
    :accessor archive)
   (name
    :initarg :name
    :type string
    :accessor name)
   (mode
    :initarg :mode
    :type list
    :accessor mode)
   (uid
    :initarg :uid
    :type (integer 0)
    :accessor uid)
   (gid
    :initarg :gid
    :type (integer 0)
    :accessor gid)
   (uname
    :initarg :uname
    :type string
    :accessor uname)
   (gname
    :initarg :gname
    :type string
    :accessor gname)
   (mtime
    :initarg :mtime
    :type local-time:timestamp
    :accessor mtime)
   (atime
    :initarg :atime
    :type local-time:timestamp
    :accessor atime)
   (ctime
    :initarg :ctime
    :type local-time:timestamp
    :accessor ctime))
  (:documentation
   "The base class of all entry types. Each ENTRY must contain a NAME, MODE,
UID, GID, and MTIME. Other common properties are UNAME, GNAME, ATIME, and
CTIME."))

(defclass has-data-mixin ()
  ((physical-entry
    :initarg :physical-entry
    :reader physical-entry)))

(defclass file-entry (entry has-data-mixin)
  ((size
    :initarg :size
    :type integer
    :accessor size)))

(defclass directory-entry (entry)
  ((size
    :initarg :size
    :type integer
    :accessor size)))

(defclass symbolic-link-entry (entry)
  ((linkname
    :initarg :linkname
    :type string
    :accessor linkname)))

(defclass hard-link-entry (entry)
  ((linkname
    :initarg :linkname
    :type string
    :accessor linkname)))

(defclass fifo-entry (entry)
  ())

(defclass device-entry (entry)
  ((devmajor
    :initarg :devmajor
    :type integer
    :accessor devmajor)
   (devminor
    :initarg :devminor
    :type integer
    :accessor devminor)))

(defclass block-device-entry (device-entry)
  ())

(defclass character-device-entry (device-entry)
  ())

(defclass unknown-entry (entry has-data-mixin)
  ((size
    :initarg :size
    :type integer
    :accessor size)))

(defgeneric make-entry-stream (entry))

(defmethod make-entry-stream ((entry has-data-mixin))
  (tar-file:make-entry-stream (physical-entry entry)))
