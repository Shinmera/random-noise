(defpackage #:org.shirakumo.random-noise.test
  (:use #:cl #:parachute #:org.shirakumo.fraf.math)
  (:local-nicknames
   (#:noise #:org.shirakumo.random-noise)))
(in-package #:org.shirakumo.random-noise.test)

(define-test random-noise)

(defun generate-pixels (noise-function &key (size 128) (frequency 1.0) (octaves 1) (lacunarity 1f0) (persistence 1f0))
  (let ((array (make-array (* size size) :element-type '(unsigned-byte 8)))
        (args (list :octaves octaves :lacunarity lacunarity :persistence persistence)))
    (dotimes (y size array)
      (dotimes (x size)
        (let ((sample (apply noise-function (vec x y) frequency args)))
          (setf (aref array (+ x (* y size))) (floor (* 128 (1+ sample)))))))))

(defun generate-image (file noise-function &rest args &key (size 128) (if-exists :supersede))
  (remf args :if-exists)
  (let ((pixels (apply #'generate-pixels noise-function args)))
    (zpng:write-png (make-instance 'zpng:png :image-data pixels :color-type :grayscale :width size :height size) file
                    :if-exists if-exists)
    file))
