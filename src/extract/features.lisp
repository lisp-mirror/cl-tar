;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)

#+(and (not windows) (not darwin))
(progn
  (pushnew :tar-extract-use-mkfifoat *features*))

#-windows
(progn
  (pushnew :tar-extract-use-openat *features*)
  (pushnew :tar-extract-use-utimens *features*))
