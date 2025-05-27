(in-package #:org.shirakumo.random-noise)

(defconstant SMOOTH-LSE 10f0)
(defconstant SMOOTH-POLY 0.25f0)

(defun init/smooth-worley (data)
  (setf (bv data) 2f0)
  data)

(define-sample-function distance/1d/smooth-worley ((x single-float))
  (sample (abs x) (if (< x 0) +1f0 -1f0)))

(define-sample-function distance/2d/smooth-worley ((x single-float) (y single-float))
  (let ((v (sqrt (+ (* x x) (* y y)))))
    (sample v (- (/ x v)) (- (/ y v)))))

(define-sample-function distance/3d/smooth-worley ((x single-float) (y single-float) (z single-float))
  (let ((v (sqrt (+ (* x x) (* y y) (* z z)))))
    (sample v (- (/ x v)) (- (/ y v)) (- (/ z v)))))

(defun finalize/1d/smooth-worley (data)
  (setf (adx data) (/ (adx data) (av data)))
  (setf (av data) (/ (log (av data)) (- SMOOTH-LSE)))
  (cond ((< 0 (av data))
         (with-sample a (smoothstep (av data) (adx data) (ady data) (adz data))
           (setf (av data) a)
           (setf (adx data) adx)))
        (T
         (setf (av data) 0f0)
         (setf (adx data) 0f0)))
  data)

(defun finalize/2d/smooth-worley (data)
  (setf (adx data) (/ (adx data) (av data)))
  (setf (ady data) (/ (ady data) (av data)))
  (setf (av data) (/ (log (av data)) (- SMOOTH-LSE)))
  (cond ((and (< 0 (av data)) (< (av data) 1))
         (with-sample a (smoothstep (av data) (adx data) (ady data))
           (setf (av data) a)
           (setf (adx data) adx)
           (setf (ady data) ady)))
        (T
         (setf (av data) 1f0)
         (setf (adx data) 0f0)
         (setf (ady data) 0f0)))
  data)

(defun finalize/3d/smooth-worley (data)
  (setf (adx data) (/ (adx data) (av data)))
  (setf (ady data) (/ (ady data) (av data)))
  (setf (adz data) (/ (adz data) (av data)))
  (setf (av data) (/ (log (av data)) (- SMOOTH-LSE)))
  (cond ((and (< 0 (av data)) (< (av data) 1))
         (with-sample a (smoothstep (av data) (adx data) (ady data) (adz data))
           (setf (av data) a)
           (setf (adx data) adx)
           (setf (ady data) ady)
           (setf (adz data) adz)))
        (T
         (setf (av data) 1f0)
         (setf (adx data) 0f0)
         (setf (ady data) 0f0)
         (setf (adz data) 0f0)))
  (cond ((and (< 0 (bv data)) (< (bv data) 1))
         (with-sample b (smoothstep (bv data) (bdx data) (bdy data) (bdz data))
           (setf (bv data) b)
           (setf (bdx data) bdx)
           (setf (bdy data) bdy)
           (setf (bdz data) bdz)))
        (T
         (setf (bv data) 1f0)
         (setf (bdx data) 0f0)
         (setf (bdy data) 0f0)
         (setf (bdz data) 0f0)))
  data)

(defun update/1d/smooth-worley (data v dx)
  (let ((e (exp (* -1 SMOOTH-LSE v))))
    (incf (av data) e)
    (incf (adx data) (* e dx))
    (let* ((h (- 1 (/ (abs (- (bv data) v)) SMOOTH-POLY)))
           (hdx (- (bdx data) dx))
           (ds (< (- (bv data) v) 0))
           (smooth (< 0 h))
           (h (* 0.25f0 SMOOTH-POLY h h)))
      (when (< v (bv data))
        (setf (bv data) v)
        (setf (bdx data) dx))
      (when smooth
        (decf (bv data) h)
        (decf (bdx data) (* hdx 0.5 h (if ds +1f0 -1f0))))))
  data)

(defun update/2d/smooth-worley (data v dx dy)
  (let ((e (exp (* -1 SMOOTH-LSE v))))
    (incf (av data) e)
    (incf (adx data) (* e dx))
    (incf (ady data) (* e dy))
    (let* ((h (- 1 (/ (abs (- (bv data) v)) SMOOTH-POLY)))
           (hdx (- (bdx data) dx))
           (hdy (- (bdy data) dy))
           (ds (< (- (bv data) v) 0))
           (smooth (< 0 h))
           (h (* 0.25f0 SMOOTH-POLY h h)))
      (when (< v (bv data))
        (setf (bv data) v)
        (setf (bdx data) dx)
        (setf (bdy data) dy))
      (when smooth
        (decf (bv data) h)
        (decf (bdx data) (* hdx 0.5 h (if ds +1f0 -1f0)))
        (decf (bdy data) (* hdy 0.5 h (if ds +1f0 -1f0))))))
  data)

(defun update/3d/smooth-worley (data v dx dy dz)
  (let ((e (exp (* -1 SMOOTH-LSE v))))
    (incf (av data) e)
    (incf (adx data) (* e dx))
    (incf (ady data) (* e dy))
    (incf (adz data) (* e dz))
    (let* ((h (- 1 (/ (abs (- (bv data) v)) SMOOTH-POLY)))
           (hdx (- (bdx data) dx))
           (hdy (- (bdy data) dy))
           (hdz (- (bdz data) dz))
           (ds (< (- (bv data) v) 0))
           (smooth (< 0 h))
           (h (* 0.25f0 SMOOTH-POLY h h)))
      (when (< v (bv data))
        (setf (bv data) v)
        (setf (bdx data) dx)
        (setf (bdy data) dy)
        (setf (bdz data) dz))
      (when smooth
        (decf (bv data) h)
        (decf (bdx data) (* hdx 0.5 h (if ds +1f0 -1f0)))
        (decf (bdy data) (* hdy 0.5 h (if ds +1f0 -1f0)))
        (decf (bdz data) (* hdz 0.5 h (if ds +1f0 -1f0))))))
  data)

(define-voronoi-method (:smooth-worley 1)
  :init init/smooth-worley
  :update update/1d/smooth-worley
  :distance distance/1d/smooth-worley
  :finalize finalize/1d/smooth-worley)

(define-voronoi-method (:smooth-worley 2)
  :init init/smooth-worley
  :update update/2d/smooth-worley
  :distance distance/2d/smooth-worley
  :finalize finalize/2d/smooth-worley)

(define-voronoi-method (:smooth-worley 3)
  :init init/smooth-worley
  :update update/3d/smooth-worley
  :distance distance/3d/smooth-worley
  :finalize finalize/3d/smooth-worley)

(define-noise-function smooth-worley 1 (&optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/1d position frequency xxhash lattice (voronoi-method :smooth-worley 1) function))

(define-noise-function smooth-worley 2 (&optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/2d position frequency xxhash lattice (voronoi-method :smooth-worley 2) function))

(define-noise-function smooth-worley 3 (&optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/3d position frequency xxhash lattice (voronoi-method :smooth-worley 3) function))

(define-noise-toplevel smooth-worley (&optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  lattice function)
