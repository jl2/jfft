;;;; radial-julia.asd
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:jfft
  :description "Describe radial-julia here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:png
               #:alexandria
               #:work-queue
               #:anim-utils)
  :serial t
  :components ((:file "package")
               (:file "jfft")))

