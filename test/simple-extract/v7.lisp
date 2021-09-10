(in-package #:tar-simple-extract-test)

(para:define-test extract-v7
  (let ((temp-dir (uiop:ensure-directory-pathname
                   (osicat-posix:mkdtemp
                    (namestring (merge-pathnames "cl-tar-"
                                                 (uiop:temporary-directory)))))))
    (unwind-protect
         (tar:with-open-archive (a (asdf:system-relative-pathname
                                    :tar "test/v7.tar"))
           (tar-simple-extract:simple-extract-archive a :directory temp-dir)
           (para:is equal
                    "Hello, world!
"
                    (uiop:read-file-string (merge-pathnames "a.txt" temp-dir)))
           (para:is equal
                    "Hello, world!
"
                    (uiop:read-file-string (merge-pathnames "a-symlink.txt" temp-dir)))
           (para:is equal
                    "Hello, world!
"
                    (uiop:read-file-string (merge-pathnames "a-hardlink.txt" temp-dir))))
      (uiop:delete-directory-tree temp-dir :validate t))))
