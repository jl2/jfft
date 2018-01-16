;;;; package.lisp
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage #:jfft
  (:use #:cl #:alexandria #:anim-utils)
  (:export
   #:make-fft-julia-sets
   #:fft-julia-sets))

