;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)

(defvar *deferred-links*)

(defvar *deferred-directories*)

(defvar *destination-dir-fd*)

(defun extract-archive (archive
                        &key
                          (directory *default-pathname-defaults*)

                          (absolute-pathnames :error)
                          (device-pathnames :error)
                          (dot-dot :error)
                          (strip-components 0)

                          (if-exists :error)
                          (if-newer-exists :error)
                          (if-symbolic-link-exists :error)
                          (if-directory-symbolic-link-exists :error)

                          keep-directory-metadata
                          touch
                          (no-same-owner (or #+:windows t
                                             (not (zerop (nix:getuid)))))
                          numeric-uid
                          mask

                          (symbolic-links :dereference)
                          (hard-links :dereference)
                          (character-devices :skip)
                          (block-devices :skip)
                          (fifos :skip)

                          (filter (constantly t)))
  "Extract all entries in ARCHIVE to DIRECTORY.

DIRECTORY defaults to (UIOP:ENSURE-DIRECTORY-PATHNAME *DEFAULT-PATHNAME-DEFAULTS*).

The following options configure how the final pathname of each entry is
computed:

ABSOLUTE-PATHANMES controls what happens when an entry is discovered that has
an absolute pathname. It defaults to :ERROR. The possible values are:

+ :ALLOW: Allow the pathname as is.
+ :SKIP: Silently skip the entry.
+ :RELATIVIZE: Strip the leading / and treat it as a relative pathname.
+ :ERROR: Signal an ENTRY-NAME-IS-ABSOLUTE-ERROR, with the restarts CONTINUE,
  SKIP-ENTRY, and RELATIVIZE-ENTRY-NAME active.

DEVICE-PATHNAMES controls what happens when an entry is discovered that has a
non-NIL device. It defaults to :ERROR. The possible values are:

+ :ALLOW: Allow the pathname as is.
+ :SKIP: Silently skip the entry.
+ :RELATIVIZE: Strip the device.
+ :ERROR: Signal an ENTRY-NAME-CONTAINS-DEVICE-ERROR, with the restarts
  CONTINUE, SKIP-ENTRY, and RELATIVIZE-ENTRY-NAME active.

DOT-DOT controls what happens when an entry is discovered that has a name
containing .. in a directory component. It defaults to :ERROR. The possible
values are:

+ :BACK: Allow the pathname as is, treating .. as :BACK.
+ :SKIP: Silently skip the entry.
+ :ERROR: Signal an ENTRY-NAME-CONTAINS-..-ERROR, with the restarts
  TREAT-..-AS-BACK and SKIP-ENTRY active.

STRIP-COMPONENTS is an integer specifying how many directory and file
components to strip. Defaults to 0.



The following options configure what happens to files that already exist on the
filesystem.

IF-DIRECTORY-SYMBOLIC-LINK-EXISTS controls what happens to existing directories
that are symlinks. This includes any symlink on the destination path of the
entry. Defaults to :ERROR. The possible values are:

+ :ERROR : Signal an error
+ :SUPERSEDE : replace the symlink with a new, empty directory.
+ :FOLLOW : keep the existing symlink.

IF-SYMBOLIC-LINK-EXISTS controls what happesn to existing files that are
symlinks. Defaults to :ERROR. The possible values are:

+ NIL, :SKIP : Skip the entry.
+ :FOLLOW : Follow the symlink and write the contents of the entry, respecting
  IF-NEWER-EXISTS and IF-EXISTS.
+ :SUPERSEDE : Replace the symlink.


IF-NEWER-EXISTS controls what happens to files that already exist within
DIRECTORY if extracting ARCHIVE would overwrite them and the existing file has
a more recent mtime. It defaults to :ERROR. The possible values are:

+ NIL, :SKIP, :KEEP : existing files are skipped
+ :SUPERSEDE, :RENAME-AND-DELETE, :ERROR : Same behavior as OPEN.

IF-EXISTS controls what happens to files that already exist within
DIRECTORY. It defaults to :ERROR. The possible values are:

+ NIL, :SKIP, :KEEP : existing files are skipped
+ :SUPERSEDE, :RENAME-AND-DELETE, :ERROR : Same behavior as OPEN.


The following options configure how metadata is extracted.

If KEEP-DIRECTORY-METADATA is non-NIL, then metadata for existing directories
is kept.

If TOUCH is non-NIL, file mtimes will not be set on extraction.

If NO-SAME-OWNER is non-NIL, then the owner and group of extracted entries will
not be set. Defaults to T for non-root users.

If NUMERIC-UID is non-NIL, UIDs and GIDs are preferred over UNAMEs and GNAMEs.

MASK is a list of permissions to remove from all entries.

The following options configure how certain types of entries are extracted.

SYMBOLIC-LINKS controls how symbolic links are extracted from ARCHIVE. It
defaults to :DEREFERENCE. The possible values are:

+ :DEREFERENCE: any symlink entries are instead written as normal files with
  the contents of the file they point to.
+ :SKIP: Skip the symlink.
+ :ERROR: Signal an error.

HARD-LINKS controls how hard links are extracted from ARCHIVE. It defaults to
:DEREFERENCE. The possible values are:

+ :DEREFERENCE: any hard link entries are instead written as normal files with
  the contents of the file they point to.
+ :SKIP: Skip the hard link.
+ :ERROR: Signal an error.

CHARACTER-DEVICES controls how character devices are extracted from ARCHIVE. It
defaults to :SKIP. The possible values are:

+ :SKIP: Skip the entry.
+ :ERROR: Signal an error.

BLOCK-DEVICES controls how block devices are extracted from ARCHIVE. It
defaults to :SKIP. The possible values are:

+ :SKIP: Skip the entry.
+ :ERROR: Signal an error.

FIFOS controls how FIFOs are extracted from ARCHIVE. It defaults to :SKIP. The
possible values are:

+ :SKIP: Skip the entry.
+ :ERROR: Signal an error.

The following option controls what entries are extracted.

FILTER defaults to (CONSTANTLY T). Must be a function designator that takes two
arguments (the entry and the pathname were it will be extracted) and returns
non-NIL if the entry should be extracted."
  (let ((*default-pathname-defaults* (uiop:ensure-directory-pathname directory))
        (*deferred-links* nil)
        (*deferred-directories* nil)
        *destination-dir-fd*)
    (ensure-directories-exist *default-pathname-defaults*)
    (setf *destination-dir-fd* (nix:open *default-pathname-defaults*
                                         (logior nix:o-rdonly
                                                 nix:o-directory)))
    (handler-bind
        ((entry-name-contains-device-error
           (lambda (c)
             (case device-pathnames
               (:allow (continue c))
               (:skip (skip-entry c))
               (:relativize (relativize-entry-name c)))))
         (entry-name-contains-..-error
           (lambda (c)
             (case dot-dot
               (:back (treat-..-as-back c))
               (:skip (skip-entry c)))))
         (entry-name-is-absolute-error
           (lambda (c)
             (case absolute-pathnames
               (:allow (continue c))
               (:skip (skip-entry c))
               (:relativize (relativize-entry-name c)))))

         (unsupported-symbolic-link-entry-error
           (lambda (c)
             (case symbolic-links
               (:skip (skip-entry c))
               (:dereference (dereference-link c)))))
         (unsupported-hard-link-entry-error
           (lambda (c)
             (case hard-links
               (:skip (skip-entry c))
               (:dereference (dereference-link c)))))
         (unsupported-fifo-entry-error
           (lambda (c)
             (case fifos
               (:skip (skip-entry c)))))
         (unsupported-block-device-entry-error
           (lambda (c)
             (case block-devices
               (:skip (skip-entry c)))))
         (unsupported-character-device-entry-error
           (lambda (c)
             (case character-devices
               (:skip (skip-entry c)))))

         (extraction-through-symbolic-link-error
           (lambda (c)
             (if (uiop:directory-pathname-p (extraction-through-symbolic-link-error-pathname c))
                 (case if-directory-symbolic-link-exists
                   ((nil :skip) (skip-entry c))
                   (:follow (follow-symbolic-link c))
                   (:supersede (replace-symbolic-link c)))
                 (case if-symbolic-link-exists
                   ((nil :skip) (skip-entry c))
                   (:follow (follow-symbolic-link c))
                   (:supersede (replace-symbolic-link c))))))

         (destination-exists
           (lambda (c)
             (let ((entry-mtime (tar:mtime (extraction-entry-error-entry c)))
                   (existing-mtime (destination-exists-mtime c)))
               (if (local-time:timestamp< entry-mtime existing-mtime)
                   (case if-newer-exists
                     ((nil :skip :keep) (skip-entry c))
                     (:supersede (invoke-restart 'supersede-file))
                     (:rename-and-delete (invoke-restart 'rename-and-replace-file)))
                   (case if-exists
                     ((nil :skip :keep) (skip-entry c))
                     (:supersede (invoke-restart 'supersede-file))
                     (:rename-and-delete (invoke-restart 'rename-and-replace-file))))))))
      (tar:do-entries (entry archive)
        (let ((*current-entry* entry))
          (restart-case
              (let ((pn (compute-extraction-pathname entry (tar:name entry) strip-components)))
                (when (funcall filter entry pn)
                  (tar:with-ignored-unsupported-properties ()
                    (extract-entry entry pn
                                   :mask mask
                                   :numeric-uid numeric-uid
                                   :no-same-owner no-same-owner
                                   :touch touch))))
            (skip-entry ()))))
      (dolist (pair *deferred-directories*)
        (handle-deferred-directory (car pair) (cdr pair)
                                   :mask mask
                                   :numeric-uid numeric-uid
                                   :no-same-owner no-same-owner
                                   :touch touch))
      (values))))

(defgeneric extract-entry (entry pn &key))

(defun safe-getpwnam (uname)
  (unless (null uname)
    (nix:getpwnam uname)))

(defun safe-getgrnam (gname)
  (unless (null gname)
    (nix:getgrnam gname)))

(defmethod extract-entry ((entry tar:file-entry) pn
                          &key touch no-same-owner numeric-uid mask
                            if-destination-symbolic-link)
  ;; First, get a handle on the directory.
  (with-open-stream (stream (openat *destination-dir-fd* pn
                                    (logior nix:s-irusr
                                            nix:s-iwusr)))
    (uiop:copy-stream-to-stream (tar:make-entry-stream entry) stream
                                :element-type '(unsigned-byte 8))
    (handler-case
        (progn
          ;; Set atime and mtime
          (unless touch
            (let ((atime (and (slot-boundp entry 'tar:atime) (tar:atime entry)))
                  (mtime (tar:mtime entry))
                  atime-sec atime-nsec
                  mtime-sec mtime-nsec)
              (if (null atime)
                  (setf atime-sec 0
                        atime-nsec nix:utime-omit)
                  (setf atime-sec (local-time:timestamp-to-unix atime)
                        atime-nsec (local-time:nsec-of atime)))
              (if (null mtime)
                  (setf mtime-sec 0
                        mtime-nsec nix:utime-omit)
                  (setf mtime-sec (local-time:timestamp-to-unix mtime)
                        mtime-nsec (local-time:nsec-of mtime)))
              (nix:futimens (fd stream) atime-sec atime-nsec mtime-sec mtime-nsec)))
          ;; Set permissions
          (nix:fchmod (fd stream) (tar::permissions-to-mode (set-difference (tar:mode entry)
                                                                            mask)))
          ;; Set owner
          (unless no-same-owner
            (let ((owner (if numeric-uid
                             (tar:uid entry)
                             (or (nth-value 2 (safe-getpwnam (tar:uname entry)))
                                 (tar:uid entry))))
                  (group (if numeric-uid
                             (tar:gid entry)
                             (or (nth-value 2 (safe-getgrnam (tar:gname entry)))
                                 (tar:gid entry)))))
              (nix:fchown (fd stream) owner group)))))))

(defun handle-deferred-directory (entry pn
                                  &key
                                    touch no-same-owner numeric-uid mask
                                    if-destination-symbolic-link)
  (with-fd (fd (fd (openat *destination-dir-fd* pn
                           (logior nix:s-irusr
                                   nix:s-iwusr
                                   nix:s-ixusr))))
    (handler-case
        (progn
          ;; Set atime and mtime
          (unless touch
            (let ((atime (and (slot-boundp entry 'tar:atime) (tar:atime entry)))
                  (mtime (tar:mtime entry))
                  atime-sec atime-nsec
                  mtime-sec mtime-nsec)
              (if (null atime)
                  (setf atime-sec 0
                        atime-nsec nix:utime-omit)
                  (setf atime-sec (local-time:timestamp-to-unix atime)
                        atime-nsec (local-time:nsec-of atime)))
              (if (null mtime)
                  (setf mtime-sec 0
                        mtime-nsec nix:utime-omit)
                  (setf mtime-sec (local-time:timestamp-to-unix mtime)
                        mtime-nsec (local-time:nsec-of mtime)))
              (nix:futimens fd atime-sec atime-nsec mtime-sec mtime-nsec)))
          ;; Set permissions
          (nix:fchmod fd (tar::permissions-to-mode (set-difference (tar:mode entry)
                                                                            mask)))
          ;; Set owner
          (unless no-same-owner
            (let ((owner (if numeric-uid
                             (tar:uid entry)
                             (or (nth-value 2 (safe-getpwnam (tar:uname entry)))
                                 (tar:uid entry))))
                  (group (if numeric-uid
                             (tar:gid entry)
                             (or (nth-value 2 (safe-getgrnam (tar:gname entry)))
                                 (tar:gid entry)))))
              (nix:fchown fd owner group)))))))

(defmethod extract-entry ((entry tar:directory-entry) pn &key &allow-other-keys)
  ;; First, get a handle on the parent
  (with-fd (dirfd (fd (openat *destination-dir-fd* pn
                              (logior nix:s-irusr
                                      nix:s-iwusr
                                      nix:s-ixusr))))
    (declare (ignore dirfd))
    ;; Enqueue this to later set its properties.
    (push (cons entry pn) *deferred-directories*)))

(defmethod extract-entry ((entry tar:fifo-entry) pn &key mask &allow-other-keys)
  (let ((dir-fd (fd (openat *destination-dir-fd* (uiop:pathname-directory-pathname pn) nix:s-irwxu))))
    (with-fd (dir-fd)
      (nix:mkfifoat dir-fd (file-namestring pn) (tar::permissions-to-mode
                                                 (set-difference (tar:mode entry)
                                                                 mask))))))

(defmethod extract-entry ((entry tar:symbolic-link-entry) pn &key &allow-other-keys)
  (let ((dir-fd (fd (openat *destination-dir-fd* (uiop:pathname-directory-pathname pn) nix:s-irwxu))))
    (with-fd (dir-fd)
      (nix:symlinkat (tar:linkname entry) dir-fd (file-namestring pn)))))

(defmethod extract-entry ((entry tar:hard-link-entry) pn &key &allow-other-keys)
  (let* ((dir-fd (fd (openat *destination-dir-fd* (uiop:pathname-directory-pathname pn) nix:s-irwxu)))
         (destination-pn (uiop:parse-unix-namestring (tar:linkname entry)
                                                     :dot-dot :back))
         (destination-dir-fd (fd (openat *destination-dir-fd*
                                         (uiop:pathname-directory-pathname destination-pn)
                                         nix:s-irwxu))))
    (unwind-protect
         (nix:linkat destination-dir-fd (file-namestring destination-pn)
                     dir-fd (file-namestring pn) 0)
      (unless (eql dir-fd *destination-dir-fd*)
        (nix:close dir-fd))
      (unless (or (eql destination-dir-fd *destination-dir-fd*)
                  (eql destination-dir-fd dir-fd))
        (nix:close destination-dir-fd)))))
