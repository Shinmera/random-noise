(in-package #:org.shirakumo.random-noise)

(declaim (inline make-voronoi-data))
(defstruct (voronoi-data
            (:constructor make-voronoi-data ())
            (:conc-name NIL)
            (:copier NIL)
            (:predicate NIL))
  (av 0f0 :type single-float)
  (adx 0f0 :type single-float)
  (ady 0f0 :type single-float)
  (adz 0f0 :type single-float)
  (bv 0f0 :type single-float)
  (bdx 0f0 :type single-float)
  (bdy 0f0 :type single-float)
  (bdz 0f0 :type single-float))

(defvar *voronoi-methods* ())

(defstruct (voronoi-method
            (:constructor make-voronoi-method (init update distance finalize))
            (:conc-name voronoi-)
            (:copier NIL)
            (:predicate NIL))
  (init NIL :type function)
  (update NIL :type function)
  (distance NIL :type function)
  (finalize NIL :type function))

(defun voronoi-method (type arity)
  (aref (cdr (or (assoc type *voronoi-methods*)
                 (error "No such voronoi method ~s" type)))
        (1- arity)))

(defun (setf voronoi-method) (method type arity)
  (setf (aref (cdr (or (assoc type *voronoi-methods*)
                       (let ((cons (cons type (make-array 3))))
                         (push cons *voronoi-methods*)
                         cons)))
              (1- arity))
        method))

(defmacro define-voronoi-method ((type arity) &body args)
  (destructuring-bind (&key init update distance (finalize 'identity)) args
    `(setf (voronoi-method ',type ,arity)
           (make-voronoi-method #',init #',update #',distance #',finalize))))

(defmacro with-voronoi-method (method &body body)
  `(let* ((method (the voronoi-method ,method))
          (init (voronoi-init method))
          (update (voronoi-update method))
          (distance (voronoi-distance method))
          (finalize (voronoi-finalize method)))
     ,@body))

(define-noise-function voronoi 1 ((lattice function) (method voronoi-method) &optional (function :f2-f1 keyword))
  (with-voronoi-method method
    (let ((x (make-lattice))
          (data (make-voronoi-data))
          (validator (lattice-validator lattice)))
      (declare (dynamic-extent x data))
      (funcall lattice position frequency x)
      (funcall init data)
      (loop for u from -1 to +1
            for h = (xxhash-eat xxhash (funcall validator (+ u (p0 x)) frequency))
            do (with-sample (v dx) (funcall distance (+ (xxhash-float h) u (- (g0 x))))
                 (funcall update data v dx)))
      (funcall finalize data)
      (ecase function
        (:f1
         (sample (av data)
                 (* frequency (adx data))))
        (:f2
         (sample (bv data)
                 (* frequency (bdx data))))
        (:f2-f1
         (sample (- (bv data) (av data))
                 (* frequency (- (bdx data) (adx data)))))))))

(define-noise-function voronoi 2 ((lattice function) (method voronoi-method) &optional (function :f2-f1 keyword))
  (with-voronoi-method method
    (let ((x (make-lattice)) (y (make-lattice))
          (data (make-voronoi-data))
          (validator (lattice-validator lattice)))
      (declare (dynamic-extent x y data))
      (funcall lattice position frequency x)
      (funcall lattice position frequency y)
      (funcall init data)
      (loop for u from -1 to +1
            for hx = (xxhash-eat xxhash (funcall validator (+ u (p0 x)) frequency))
            for x-offset = (- u (g0 x))
            do (loop for v from -1 to +1
                     for h = (xxhash-eat hx (funcall validator (+ v (p0 y)) frequency))
                     for y-offset = (- v (g0 y))
                     do (with-sample (v dx dy) (funcall distance
                                                        (+ (xxhash-float h 0) x-offset)
                                                        (+ (xxhash-float h 1) y-offset))
                          (funcall update data v dx dy))
                        (with-sample (v dx dy) (funcall distance
                                                        (+ (xxhash-float h 2) x-offset)
                                                        (+ (xxhash-float h 3) y-offset))
                          (funcall update data v dx dy))))
      (funcall finalize data)
      (ecase function
        (:f1
         (sample (av data)
                 (* frequency (adx data))
                 (* frequency (ady data))))
        (:f2
         (sample (bv data)
                 (* frequency (bdx data))
                 (* frequency (bdy data))))
        (:f2-f1
         (sample (- (bv data) (av data))
                 (* frequency (- (bdx data) (adx data)))
                 (* frequency (- (bdy data) (ady data)))))))))

(define-noise-function voronoi 3 ((lattice function) (method voronoi-method) &optional (function :f2-f1 keyword))
  (with-voronoi-method method
    (let ((x (make-lattice)) (y (make-lattice)) (z (make-lattice))
          (data (make-voronoi-data))
          (validator (lattice-validator lattice)))
      (declare (dynamic-extent x y z data))
      (funcall lattice position frequency x)
      (funcall lattice position frequency y)
      (funcall lattice position frequency z)
      (funcall init data)
      (loop for u from -1 to +1
            for hx = (xxhash-eat xxhash (funcall validator (+ u (p0 x)) frequency))
            for x-offset = (- u (g0 x))
            do (loop for v from -1 to +1
                     for hy = (xxhash-eat hx (funcall validator (+ v (p0 y)) frequency))
                     for y-offset = (- v (g0 y))
                     do (loop for w from -1 to +1
                              for h = (xxhash-eat hy (funcall validator (+ w (p0 z)) frequency))
                              for z-offset = (- w (g0 z))
                              do (with-sample (v dx dy) (funcall distance
                                                                 (+ (xxhash-float* h 5  0) x-offset)
                                                                 (+ (xxhash-float* h 5  5) y-offset)
                                                                 (+ (xxhash-float* h 5 10) z-offset))
                                   (funcall update data v dx dy))
                                 (with-sample (v dx dy) (funcall distance
                                                                 (+ (xxhash-float* h 5 15) x-offset)
                                                                 (+ (xxhash-float* h 5 20) y-offset)
                                                                 (+ (xxhash-float* h 5 25) z-offset))
                                   (funcall update data v dx dy)))))
      (funcall finalize data)
      (ecase function
        (:f1
         (sample (av data)
                 (* frequency (adx data))
                 (* frequency (ady data))
                 (* frequency (adz data))))
        (:f2
         (sample (bv data)
                 (* frequency (bdx data))
                 (* frequency (bdy data))
                 (* frequency (bdz data))))
        (:f2-f1
         (sample (- (bv data) (av data))
                 (* frequency (- (bdx data) (adx data)))
                 (* frequency (- (bdy data) (ady data)))
                 (* frequency (- (bdz data) (adz data)))))))))

(define-noise-toplevel voronoi ((lattice function) (method voronoi-method) &optional (function :f2-f1 keyword))
  lattice method function)
