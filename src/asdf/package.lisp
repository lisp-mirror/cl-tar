;;;; Package
;;;;
;;;; This software is part of cl-tar. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:tar-cli-asdf
    (:use #:cl)
  (:local-nicknames (#:ops #:asdf-release-ops))
  (:export #:tar-cli-system))

(in-package #:tar-cli-asdf)
