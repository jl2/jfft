;;;; package.lisp
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:jfft
  (:nicknames #:jf)
  (:use #:cl #:alexandria #:anim-utils)
  (:export
   #:make-julia-set
   #:make-julia-set-animation
   #:fft-julia-sets
   ))

