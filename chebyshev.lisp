(in-package #:org.shirakumo.random-noise)

(define-sample-function distance/2d/chebyshev ((x single-float) (y single-float))
  (cond ((< (abs y) (abs x))
         (sample (abs x)
                 (if (< x 0) +1f0 -1f0)
                 0f0))
        (T
         (sample (abs y)
                 0f0
                 (if (< y 0) +1f0 -1f0)))))

(define-sample-function distance/3d/chebyshev ((x single-float) (y single-float) (z single-float))
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

(define-sample-function chebyshev/1d ((position (point 1)) (frequency single-float) (xxhash xxhash) &optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/1d position frequency xxhash lattice (voronoi-method :chebyshev 1) function))

(define-sample-function chebyshev/2d ((position (point 2)) (frequency single-float) (xxhash xxhash) &optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/2d position frequency xxhash lattice (voronoi-method :chebyshev 2) function))

(define-sample-function chebyshev/3d ((position (point 3)) (frequency single-float) (xxhash xxhash) &optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/3d position frequency xxhash lattice (voronoi-method :chebyshev 3) function))
