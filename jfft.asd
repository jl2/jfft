;;;; jfft.asd
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:jfft
  :description "Animated visualization of fast fourier transforms."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC"
  :depends-on (#:png
               #:alexandria
               #:work-queue
               #:anim-utils)
  :serial t
  :components ((:file "package")
               (:file "jfft")))

