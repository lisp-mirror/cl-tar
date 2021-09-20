(in-package #:tar-test)

(para:define-test write-ustar
  (uiop:with-temporary-file (:stream s :pathname pn
                             :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:ustar-archive)
      (write-a.txt a)
      (write-a-symlink.txt a)
      (write-a-hardlink.txt a)
      (write-fifo a)
      (write-sda1 a)
      (write-tty0 a))
    :close-stream
    (files-equal pn (asdf:system-relative-pathname
                     :tar "test/ustar.tar"))))

(para:define-test write-pax
  (uiop:with-temporary-file (:stream s :pathname pn
                             :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (write-a.txt a)
      (write-a-symlink.txt a)
      (write-a-hardlink.txt a)
      (write-fifo a)
      (write-sda1 a)
      (write-tty0 a))
    :close-stream
    (files-equal pn (asdf:system-relative-pathname
                     :tar "test/pax.tar"))))

(para:define-test write-v7
  (uiop:with-temporary-file (:stream s :pathname pn
                             :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type :v7)
      (write-a.txt a)
      (write-a-symlink.txt a)
      (write-a-hardlink.txt a))
    :close-stream
    (files-equal pn (asdf:system-relative-pathname
                     :tar "test/v7.tar"))))
