(in-package #:org.shirakumo.random-noise)

(defun init/worley (data)
  (setf (av data) 2f0)
  (setf (bv data) 2f0)
  data)

(define-sample-function distance/1d/worley ((x single-float))
  (sample (abs x) (if (< x 0) +1f0 -1f0)))

(define-sample-function distance/2d/worley ((x single-float) (y single-float))
  (sample (+ (* x x) (* y y))
          x y 0f0))

(define-sample-function distance/3d/worley ((x single-float) (y single-float) (z single-float))
  (sample (+ (* x x) (* y y) (* z z)) x y z))

(defun finalize/1d/worley (data)
  data)

(defun finalize/2d/worley (data)
  (cond ((< (av data) 1f0)
         (setf (av data) (sqrt (av data)))
         (setf (adx data) (- (/ (adx data) (av data))))
         (setf (ady data) (- (/ (ady data) (av data)))))
        (T
         (setf (av data) 1f0)
         (setf (adx data) 0f0)
         (setf (ady data) 0f0)))
  (cond ((< (bv data) 1f0)
         (setf (bv data) (sqrt (bv data)))
         (setf (bdx data) (- (/ (bdx data) (bv data))))
         (setf (bdy data) (- (/ (bdy data) (bv data)))))
        (T
         (setf (bv data) 1f0)
         (setf (bdx data) 0f0)
         (setf (bdy data) 0f0)))
  data)

(defun finalize/3d/worley (data)
  (cond ((< (av data) 1f0)
         (setf (av data) (sqrt (av data)))
         (setf (adx data) (- (/ (adx data) (av data))))
         (setf (ady data) (- (/ (ady data) (av data))))
         (setf (adz data) (- (/ (adz data) (av data)))))
        (T
         (setf (av data) 1f0)
         (setf (adx data) 0f0)
         (setf (ady data) 0f0)
         (setf (adz data) 0f0)))
  (cond ((< (bv data) 1f0)
         (setf (bv data) (sqrt (bv data)))
         (setf (bdx data) (- (/ (bdx data) (bv data))))
         (setf (bdy data) (- (/ (bdy data) (bv data))))
         (setf (bdz data) (- (/ (bdz data) (bv data)))))
        (T
         (setf (bv data) 1f0)
         (setf (bdx data) 0f0)
         (setf (bdy data) 0f0)
         (setf (bdz data) 0f0)))
  data)

(defun update/1d/worley (data v dx)
  (cond ((< v (av data))
         (setf (bv data) (av data))
         (setf (bdx data) (adx data)))
        ((< v (bv data))
         (setf (bv data) v)
         (setf (bdx data) dx)))
  (when (< v (av data))
    (setf (av data) v)
    (setf (adx data) dx))
  data)

(defun update/2d/worley (data v dx dy)
  (cond ((< v (av data))
         (setf (bv data) (av data))
         (setf (bdx data) (adx data))
         (setf (bdy data) (ady data)))
        ((< v (bv data))
         (setf (bv data) v)
         (setf (bdx data) dx)
         (setf (bdy data) dy)))
  (when (< v (av data))
    (setf (av data) v)
    (setf (adx data) dx)
    (setf (ady data) dy))
  data)

(defun update/3d/worley (data v dx dy dz)
  (cond ((< v (av data))
         (setf (bv data) (av data))
         (setf (bdx data) (adx data))
         (setf (bdy data) (ady data))
         (setf (bdz data) (adz data)))
        ((< v (bv data))
         (setf (bv data) v)
         (setf (bdx data) dx)
         (setf (bdy data) dy)
         (setf (bdz data) dz)))
  (when (< v (av data))
    (setf (av data) v)
    (setf (adx data) dx)
    (setf (ady data) dy)
    (setf (adz data) dz))
  data)

(define-voronoi-method (:worley 1)
  :init init/worley
  :update update/1d/worley
  :distance distance/1d/worley
  :finalize finalize/1d/worley)

(define-voronoi-method (:worley 2)
  :init init/worley
  :update update/2d/worley
  :distance distance/2d/worley
  :finalize finalize/2d/worley)

(define-voronoi-method (:worley 3)
  :init init/worley
  :update update/3d/worley
  :distance distance/3d/worley
  :finalize finalize/3d/worley)

(define-noise-function worley 1 (&optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/1d position frequency xxhash lattice (voronoi-method :worley 1) function))

(define-noise-function worley 2 (&optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/2d position frequency xxhash lattice (voronoi-method :worley 2) function))

(define-noise-function worley 3 (&optional (lattice #'normal-lattice function) (function :f2-f1 keyword))
  (voronoi/3d position frequency xxhash lattice (voronoi-method :worley 3) function))
