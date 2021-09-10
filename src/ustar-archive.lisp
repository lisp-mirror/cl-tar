;;;; ustar archives
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defclass ustar-archive (archive)
  ())

(defmethod archive-supports-property-p ((archive ustar-archive) property)
  (or (not (null (member property '(uname gname))))
      (call-next-method)))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:file-entry)
                                        &rest overrides)
  (apply #'make-instance 'file-entry
         :archive archive
         :physical-entry physical-entry
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :size (tar-file:size physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:directory-entry)
                                        &rest overrides)
  (apply #'make-instance 'directory-entry
         :archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :size (tar-file:size physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:symbolic-link-entry)
                                        &rest overrides)
  (apply #'make-instance 'symbolic-link-entry
         :archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :linkname (tar-file:linkname physical-entry)))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:hard-link-entry)
                                        &rest overrides)
  (apply #'make-instance 'hard-link-entry
         :archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :linkname (tar-file:linkname physical-entry)))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:character-device-entry)
                                        &rest overrides)
  (apply #'make-instance 'character-device-entry
         :archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :devmajor (tar-file:devmajor physical-entry)
                  :devminor (tar-file:devminor physical-entry)))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:block-device-entry)
                                        &rest overrides)
  (apply #'make-instance 'block-device-entry
         :archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :devmajor (tar-file:devmajor physical-entry)
                  :devminor (tar-file:devminor physical-entry)))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:fifo-entry)
                                        &rest overrides)
  (apply #'make-instance 'fifo-entry
         :archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))
