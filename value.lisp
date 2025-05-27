(in-package #:org.shirakumo.random-noise)

(define-sample-function value-gradient/1d ((xxhash xxhash) (x single-float))
  (declare (ignore x))
  (sample (1- (* 2 (xxhash-float xxhash)))))

(define-sample-function value-gradient/2d ((xxhash xxhash) (x single-float) (y single-float))
  (declare (ignore x y))
  (sample (1- (* 2 (xxhash-float xxhash)))))

(define-sample-function value-gradient/3d ((xxhash xxhash) (x single-float) (y single-float) (z single-float))
  (declare (ignore x y z))
  (sample (1- (* 2 (xxhash-float xxhash)))))

(define-noise-function value 1 (&optional (lattice #'normal-lattice function))
  (lattice/1d position frequency xxhash lattice #'value-gradient/1d))

(define-noise-function value 2 (&optional (lattice #'normal-lattice function))
  (lattice/2d position frequency xxhash lattice #'value-gradient/2d))

(define-noise-function value 3 (&optional (lattice #'normal-lattice function))
  (lattice/3d position frequency xxhash lattice #'value-gradient/3d))

(define-noise-toplevel value (&optional (lattice #'normal-lattice function)) lattice)
