(in-package #:org.shirakumo.random-noise)

(defun distance/2d/chebyshev (x y)
  (cond ((< (abs y) (abs x))
         (sample (abs x)
                 (if (< x 0) +1f0 -1f0)
                 0f0))
        (T
         (sample (abs y)
                 0f0
                 (if (< y 0) +1f0 -1f0)))))

(defun distance/3d/chebyshev (x y z)
  (cond ((< (abs y) (abs x))
         (sample (abs x)
                 (if (< x 0) +1f0 -1f0)
                 0f0
                 0f0))
        ((< (abs z) (abs y))
         (sample (abs y)
                 0f0
                 (if (< y 0) +1f0 -1f0)
                 0f0))
        (T
         (sample (abs z)
                 0f0
                 0f0
                 (if (< z 0) +1f0 -1f0)))))

(define-voronoi-method (:chebyshev 1)
  :init init/worley
  :update update/1d/worley
  :distance distance/1d/worley)

(define-voronoi-method (:chebyshev 2)
  :init init/worley
  :update update/2d/worley
  :distance distance/2d/chebyshev)

(define-voronoi-method (:chebyshev 3)
  :init init/worley
  :update update/3d/worley
  :distance distance/3d/chebyshev)

(defun chebyshev/1d (position frequency xxhash &optional (lattice #'normal-lattice) (function :f2-f1))
  (voronoi/1d position frequency xxhash lattice (voronoi-method :chebyshev 1) function))

(defun chebyshev/2d (position frequency xxhash &optional (lattice #'normal-lattice) (function :f2-f1))
  (voronoi/2d position frequency xxhash lattice (voronoi-method :chebyshev 2) function))

(defun chebyshev/3d (position frequency xxhash &optional (lattice #'normal-lattice) (function :f2-f1))
  (voronoi/3d position frequency xxhash lattice (voronoi-method :chebyshev 3) function))
