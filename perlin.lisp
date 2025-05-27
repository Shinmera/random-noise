(in-package #:org.shirakumo.random-noise)

(define-sample-function perlin-gradient/1d ((xxhash xxhash) (x single-float))
  (line xxhash x))

(define-sample-function perlin-gradient/2d ((xxhash xxhash) (x single-float) (y single-float))
  (s/ * (square xxhash x y)
      (/ 2f0 0.53528f0)))

(define-sample-function perlin-gradient/3d ((xxhash xxhash) (x single-float) (y single-float) (z single-float))
  (s/ * (octahedron xxhash x y z)
      (/ 1f0 0.56290f0)))

(define-noise-function perlin 1 (&optional (lattice #'normal-lattice function))
  (lattice/1d position frequency xxhash lattice #'perlin-gradient/1d))

(define-noise-function perlin 2 (&optional (lattice #'normal-lattice function))
  (lattice/2d position frequency xxhash lattice #'perlin-gradient/2d))

(define-noise-function perlin 3 (&optional (lattice #'normal-lattice function))
  (lattice/3d position frequency xxhash lattice #'perlin-gradient/3d))
