;;;; v7 archives
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defclass v7-archive (archive)
  ())

(defmethod convert-from-physical-entry ((archive v7-archive)
                                        (physical-entry tar-file:file-entry)
                                        &rest overrides)
  (declare (ignore overrides))
  (make-instance 'file-entry
                 :archive archive
                 :physical-entry physical-entry
                 :name (tar-file:name physical-entry)
                 :size (tar-file:size physical-entry)
                 :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                 :uid (tar-file:uid physical-entry)
                 :gid (tar-file:gid physical-entry)
                 :mode (mode-to-permissions (tar-file:mode physical-entry))))

(defmethod convert-from-physical-entry ((archive v7-archive)
                                        (physical-entry tar-file:directory-entry)
                                        &rest overrides)
  (declare (ignore overrides))
  (make-instance 'directory-entry
                 :archive archive
                 :name (tar-file:name physical-entry)
                 :size (tar-file:size physical-entry)
                 :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                 :uid (tar-file:uid physical-entry)
                 :gid (tar-file:gid physical-entry)
                 :mode (mode-to-permissions (tar-file:mode physical-entry))))

(defmethod convert-from-physical-entry ((archive v7-archive)
                                        (physical-entry tar-file:symbolic-link-entry)
                                        &rest overrides)
  (declare (ignore overrides))
  (make-instance 'symbolic-link-entry
                 :archive archive
                 :name (tar-file:name physical-entry)
                 :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                 :uid (tar-file:uid physical-entry)
                 :gid (tar-file:gid physical-entry)
                 :mode (mode-to-permissions (tar-file:mode physical-entry))
                 :linkname (tar-file:linkname physical-entry)))

(defmethod convert-from-physical-entry ((archive v7-archive)
                                        (physical-entry tar-file:hard-link-entry)
                                        &rest overrides)
  (declare (ignore overrides))
  (make-instance 'hard-link-entry
                 :archive archive
                 :name (tar-file:name physical-entry)
                 :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                 :uid (tar-file:uid physical-entry)
                 :gid (tar-file:gid physical-entry)
                 :mode (mode-to-permissions (tar-file:mode physical-entry))
                 :linkname (tar-file:linkname physical-entry)))
