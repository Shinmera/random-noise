(defpackage #:org.shirakumo.random-noise.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:noise #:org.shirakumo.random-noise))
  (:export
   #:generate-image))
(in-package #:org.shirakumo.random-noise.test)

(defun f->u8 (x)
  (max 0 (min 255 (floor (* 128 (1+ x))))))

(defun generate-image (file generator &key (resolution 128) (if-exists :supersede)
                                           (frequency 4f0) (xxhash 1))
  (let ((pixels (map '(simple-array (unsigned-byte 8) (*)) #'f->u8
                     (noise:sample/2d (1- resolution) frequency xxhash generator))))
    (zpng:write-png (make-instance 'zpng:png :image-data pixels :color-type :grayscale :width resolution :height resolution) file
                    :if-exists if-exists)
    file))

(defun generate-diff-image (file generator &key (resolution 128) (if-exists :supersede)
                                                (frequency 4f0) (xxhash 1))
  (multiple-value-bind (v dx dy) (noise:sample/2d (1- resolution) frequency xxhash generator)
    (let ((pixels (make-array (* 128 128 3) :element-type '(unsigned-byte 8) :initial-element 0)))
      (loop for i from 0 below (length v)
            do (setf (aref pixels (+ (* 3 i) 0)) (f->u8 (* 0.1 (aref dx i))))
               (setf (aref pixels (+ (* 3 i) 1)) (f->u8 (* 0.1 (aref dy i)))))
      (zpng:write-png (make-instance 'zpng:png :image-data pixels :color-type :truecolor :width resolution :height resolution) file
                      :if-exists if-exists)
      file)))

(defun generate-images (dir &rest args)
  (dolist (gen '(noise:chebyshev
                 noise:perlin
                 noise:simplex
                 noise:smooth-worley
                 noise:value
                 noise:worley))
    (apply #'generate-image (make-pathname :name (string-downcase gen) :type "png" :defaults dir) gen args)
    (apply #'generate-diff-image (make-pathname :name (format NIL "~(~a-diff~)" gen) :type "png" :defaults dir) gen args))
  (apply #'generate-image (make-pathname :name "fractal" :type "png" :defaults dir)
         (lambda (p f x) (noise:fractal p f x #'noise:perlin))
         args)
  (apply #'generate-diff-image (make-pathname :name "fractal-diff" :type "png" :defaults dir)
         (lambda (p f x) (noise:fractal p f x #'noise:perlin))
         args))

(define-test random-noise)
