(in-package #:org.shirakumo.random-noise)

(declaim (inline square-vectors octahedron-vectors line square circle octahedron sphere))
(defun square-vectors (xxhash)
  (declare (type xxhash xxhash))
  (let* ((c0 (1- (* 2 (xxhash-float xxhash))))
         (c1 (- 0.5f0 (abs c0)))
         (c0 (- c0 (floor (+ 0.5 c0)))))
    (values c0 c1)))

(defun octahedron-vectors (xxhash)
  (declare (type xxhash xxhash))
  (let* ((c0 (1- (* 2 (xxhash-float xxhash 0))))
         (c1 (1- (* 2 (xxhash-float xxhash 3))))
         (c2 (- 1 (abs c0) (abs c1)))
         (offset (max 0f0 (- c2)))
         (c0 (+ c0 (if (< c0 0) offset (- offset))))
         (c1 (+ c1 (if (< c1 0) offset (- offset)))))
    (values c0 c1 c2)))

(define-sample-function line ((xxhash xxhash) (x single-float))
  (let ((l (* (1+ (xxhash-float xxhash))
              (if (= 0 (ldb (byte 1 8) (xxhash-int xxhash)))
                  +1f0 -1f0))))
    (sample (* l x) l)))

(define-sample-function square ((xxhash xxhash) (x single-float) (y single-float))
  (multiple-value-bind (c0 c1) (square-vectors xxhash)
    (sample (+ (* c0 x) (* c1 y))
            c0
            c1)))

(define-sample-function circle ((xxhash xxhash) (x single-float) (y single-float))
  (multiple-value-bind (c0 c1) (square-vectors xxhash)
    (let ((r (/ (sqrt (+ (* c0 c0) (* c1 c1))))))
      (sample (* r (+ (* c0 x) (* c1 y)))
              (* r c0)
              (* r c1)))))

(define-sample-function octahedron ((xxhash xxhash) (x single-float) (y single-float) (z single-float))
  (multiple-value-bind (c0 c1 c2) (octahedron-vectors xxhash)
    (sample (+ (* c0 x) (* c1 y) (* c2 z))
            c0
            c1
            c2)))

(define-sample-function sphere ((xxhash xxhash) (x single-float) (y single-float) (z single-float))
  (multiple-value-bind (c0 c1 c2) (octahedron-vectors xxhash)
    (let ((r (/ (sqrt (+ (* c0 c0) (* c1 c1) (* c2 c2))))))
      (sample (* r (+ (* c0 x) (* c1 y) (* c2 z)))
              (* r c0)
              (* r c1)
              (* r c2)))))

(define-sample-function value-gradient/1d ((xxhash xxhash) (x single-float))
  (declare (ignore x))
  (sample (1- (* 2 (xxhash-float xxhash)))))

(define-sample-function value-gradient/2d ((xxhash xxhash) (x single-float) (y single-float))
  (declare (ignore x y))
  (sample (1- (* 2 (xxhash-float xxhash)))))

(define-sample-function value-gradient/3d ((xxhash xxhash) (x single-float) (y single-float) (z single-float))
  (declare (ignore x y z))
  (sample (1- (* 2 (xxhash-float xxhash)))))

(define-sample-function perlin-gradient/1d ((xxhash xxhash) (x single-float))
  (line xxhash x))

(define-sample-function perlin-gradient/2d ((xxhash xxhash) (x single-float) (y single-float))
  (s/ * (square xxhash x y)
      (/ 2f0 0.53528f0)))

(define-sample-function perlin-gradient/3d ((xxhash xxhash) (x single-float) (y single-float) (z single-float))
  (s/ * (octahedron xxhash x y z)
      (/ 1f0 0.56290f0)))

(define-sample-function simplex-gradient/1d ((xxhash xxhash) (x single-float))
  (s/ * (line xxhash x)
      (/ 32f0 27f0)))

(define-sample-function simplex-gradient/2d ((xxhash xxhash) (x single-float) (y single-float))
  (s/ * (circle xxhash x y)
      (/ 5.832f0 (sqrt 2f0))))

(define-sample-function simplex-gradient/3d ((xxhash xxhash) (x single-float) (y single-float) (z single-float))
  (s/ * (sphere xxhash x y z)
      (/ 1024f0 (* 125f0 (sqrt 3f0)))))
