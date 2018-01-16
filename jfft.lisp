;;;; radial-julia.lisp
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:jfft)

(declaim (optimize (speed 3) (safety 1) (compilation-speed 0) (debug 1)))

(declaim (ftype (function 
                 (fixnum fixnum double-float double-float ) double-float)
                j-map-val))

(declaim (ftype (function
                 (fixnum (simple-array (unsigned-byte 8)) (simple-array (complex double-float)) fixnum fixnum fixnum fixnum double-float double-float double-float double-float fixnum))
                draw-fft-julia-line))

(declaim (inline j-map-val set-pixel-ong black-and-white draw-fft-julia-line smooth-colors))

(defun j-map-val (x width xmin xmax)
  "Map a value from the range 0,width to the range xmin,xmax"
  (declare (type fixnum x width))
  (declare (type double-float xmin xmax))
  (the double-float (+ xmin (* (- xmax xmin) (/ (coerce x 'double-float) (coerce width 'double-float) 1.0d0)))))

(defun set-pixel-png (img x y r g b)
  "Set a pixel in im at location x,y to color (r,g,b)"
  (declare (type fixnum x y r g b))
  (declare (type (simple-array (unsigned-byte 8)) img))
  (setf (aref img x y 0) r)
  (setf (aref img x y 1) g)
  (setf (aref img x y 2) b))

(defun black-and-white  (iters iterations i j width height)
  (declare (ignorable iters iterations i j width height))
  (declare (type fixnum iters iterations i j width height))
  (let ((val (* 255 (mod iters 2))))
    (values val val val)))

(defun smooth-colors (iters iterations i j width height)
  (declare (ignorable iters iterations i j width height))
  (declare (type fixnum iters iterations i j width height))
  (let ((tval (/ (coerce iters 'double-float) (coerce iterations 'double-float) 1.0)))
    (values (truncate (* 255 (- 1 tval)))
            (truncate (+ (* 127 (sin (* 8 pi tval))) 127))
            (truncate (+ (* 127 (cos (* pi tval))) 127)))))

(defun new-colors (iters iterations i j width height)
  (declare (ignorable iters iterations i j width height))
  (declare (type fixnum iters iterations i j width height))
  (let ((rem (mod iters 5))
        (tval (/ (coerce iters 'double-float) (coerce iterations 'double-float) 1.0)))
    (if (= iters iterations)
        (values 0 0 0)
        (values (truncate (* 36 rem))
                (truncate (* 13 rem))
                (truncate (+ (* 127 (cos (* pi tval))) 127))))))

(defun draw-fft-julia-line (i png mapping cs x-count y-count width height real-min real-max imag-min imag-max iterations)
  (declare
   (type fixnum i width height iterations)
   (type double-float real-min real-max imag-min imag-max)
   (type (simple-array (unsigned-byte 8)) png)
   (type (simple-array (complex double-float)) cs))
  (let* (
         (imag-c-idx (truncate (* y-count (/ i height))))
         (sub-image-height (truncate (/ height y-count)))
         (imag-value (j-map-val (mod i sub-image-height) sub-image-height real-min real-max))
         )

    (dotimes (j width)
      
      (declare (type fixnum j)
               (type double-float imag-value))
      
      (let* ((real-c-idx (truncate (* x-count (/ j width))))
             (this-c (aref cs (aref mapping (+ (* x-count imag-c-idx) real-c-idx))))
             (max-iterations (if (= #C(0.0 0.0) this-c)
                                 (/ iterations 3)
                                 iterations))
             (sub-image-width (truncate (/ width x-count)))
             (iters
              (do* ((real-value (j-map-val (mod j sub-image-width) sub-image-width imag-min imag-max))
                    (z          (complex real-value imag-value)
                                (+ (* z z) this-c))
                    (iter       0
                                (incf iter)))
                   ((or (>= iter max-iterations)
                        (> (abs z) 4.0)) iter)
                (declare (type fixnum iter)
                         (type (complex double-float) z)
                         (type double-float real-value)
                         (dynamic-extent iter)))))
        (declare (type fixnum iters))
        (multiple-value-call #'set-pixel-png png i j (new-colors iters max-iterations i j width height))))))


(defun make-fft-julia (&key
                         (cs)
                         (file-name)
                         (width 800) (height 800)
                         (x-count 6)
                         (y-count 5)
                         (mapping #(20 21 22 23 24 25
                                    19  6  7  8  9 26
                                    18  5  0  1 10 27
                                    17  4  3  2 11 28
                                    16 15 14 13 12 29))
                         (real-min -2.0) (real-max 2.0)
                         (imag-min -2.0) (imag-max 2.0)
                         (iterations 120)
                         (thread-count 4))
  "Generate a Mandelbrot Set fractal and save to the file name given.  The portion of the set drawn is given by xmin,xmax and ymin,ymax."
  (declare (type fixnum width height iterations thread-count)
           (type string file-name)
           (type (simple-array (complex double-float)) cs)
           (type double-float real-min real-max imag-min imag-max))

  (ensure-directories-exist file-name)
  (let* ((img (png:make-image height width 3 8))
         (wq (wq:create-work-queue (rcurry #'draw-fft-julia-line
                                           img
                                           mapping
                                           cs
                                           x-count y-count
                                           width height
                                           real-min real-max
                                           imag-min imag-max
                                           iterations)
                                   thread-count)))

    (dotimes (i height)
      (declare (type fixnum i))
      (wq:add-job wq i))

    (wq:destroy-work-queue wq)

    (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
      (png:encode img output))))

(defun fft-julia-sets (&key
                         (mp3-file-name)
                         (output-directory "/Users/jeremiahlarocco/images/fractals/julia-animation/")
                         (width 800)
                         (height 800)

                         (iterations 120)

                         (fps 30)
                         (thread-count 4)
                         (fft-window-size 128)

                         (x-count 6)
                         (y-count 5)
                         (mapping #(20 21 22 23 24 25
                                    19  6  7  8  9 26
                                    18  5  0  1 10 27
                                    17  4  3  2 11 28
                                    16 15 14 13 12 29))

                         (max-frames nil)
                         (max-duration nil)
                         (real-min -2.0) (real-max 2.0)
                         (imag-min -2.0) (imag-max 2.0)
                         )
  
  (declare (type fixnum width height frame-count iterations thread-count)
           (type simple-string output-directory mp3-file-name)
           (type double-float real-min real-max imag-min imag-max)
           )
  (let* (
         (real-dir-name (ensure-directories-exist
                         (if (char=  #\/ (aref output-directory (- (length output-directory) 1)))
                             output-directory
                             (concatenate 'string output-directory "/"))))
         (description-file-name (format nil "~adescription.lisp" real-dir-name))

         (the-mp3 (read-mp3-file mp3-file-name))
         (real-dir 1.0)
         (imag-dir 1.0)
         (song-duration (mp3-file-duration-in-seconds the-mp3))
         (total-frames (if max-frames
                           max-frames
                           (if max-duration
                               (* fps max-duration)
                               (ceiling (* song-duration fps))))))
    (declare 
     (type simple-string real-dir-name description-file-name))

    (format t "Creating animation with ~a frames...~%" total-frames)

    (with-open-file (outf description-file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (declare (type stream outf))
      (format outf "(list ~%")
      (dotimes (frame total-frames)
        (declare (type fixnum frame))
        (let* ((output-file-name (format nil "~aframe~8,'0d.png" real-dir-name frame))
               (win-center (ceiling (max 0 (- (* 44100 (interpolate 0.0 song-duration frame total-frames))
                                              (round (/ fft-window-size 2))))))
               
               (left-fft-data (bordeaux-fft:windowed-fft (mp3-file-left-channel the-mp3) win-center fft-window-size))
               (right-fft-data (bordeaux-fft:windowed-fft (mp3-file-right-channel the-mp3) win-center fft-window-size)))

          (make-fft-julia :file-name output-file-name
                          :width width :height height
                          :cs left-fft-data
                          :x-count x-count
                          :y-count y-count
                          :mapping mapping
                          :real-min real-min
                          :real-max real-max
                          :real-min imag-min
                          :imag-max imag-max

                          :iterations iterations
                          :thread-count thread-count)))
      (format outf ")~%"))))
