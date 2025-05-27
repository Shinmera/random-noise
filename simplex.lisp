(in-package #:org.shirakumo.random-noise)

(define-sample-function simplex-gradient/1d ((xxhash xxhash) (x single-float))
  (s/ * (line xxhash x)
      (/ 32f0 27f0)))

(define-sample-function simplex-gradient/2d ((xxhash xxhash) (x single-float) (y single-float))
  (s/ * (circle xxhash x y)
      (/ 5.832f0 (sqrt 2f0))))

(define-sample-function simplex-gradient/3d ((xxhash xxhash) (x single-float) (y single-float) (z single-float))
  (s/ * (sphere xxhash x y z)
      (/ 1024f0 (* 125f0 (sqrt 3f0)))))

(define-noise-function simplex 1 (&optional (lattice #'normal-lattice function))
  (lattice/1d position frequency xxhash lattice #'simplex-gradient/1d))

(define-noise-function simplex 2 (&optional (lattice #'normal-lattice function))
  (lattice/2d position frequency xxhash lattice #'simplex-gradient/2d))

(define-noise-function simplex 3 (&optional (lattice #'normal-lattice function))
  (lattice/3d position frequency xxhash lattice #'simplex-gradient/3d))

(define-noise-toplevel simplex (&optional (lattice #'normal-lattice function)) lattice)
