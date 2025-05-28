(defpackage #:org.shirakumo.random-noise.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:noise #:org.shirakumo.random-noise))
  (:export
   #:generate-image))
(in-package #:org.shirakumo.random-noise.test)

(defun generate-image (file generator &key (resolution 128) (if-exists :supersede)
                                           (frequency 4f0) (xxhash 1))
  (let ((pixels (map '(simple-array (unsigned-byte 8) (*))
                     (lambda (x) (max 0 (min 255 (floor (* 128 (1+ x))))))
                     (noise:sample/2d (1- resolution) frequency xxhash generator))))
    (zpng:write-png (make-instance 'zpng:png :image-data pixels :color-type :grayscale :width resolution :height resolution) file
                    :if-exists if-exists)
    file))

(defun generate-images (dir &rest args)
  (dolist (gen '(noise:chebyshev
                 noise:perlin
                 noise:simplex
                 noise:smooth-worley
                 noise:value
                 noise:worley))
    (apply #'generate-image (make-pathname :name (string-downcase gen) :type "png" :defaults dir) gen args))
  (apply #'generate-image (make-pathname :name "fractal" :type "png" :defaults dir)
         (lambda (p f x) (noise:fractal p f x #'noise:perlin))
         args))

(define-test random-noise)
