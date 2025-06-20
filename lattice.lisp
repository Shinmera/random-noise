(in-package #:org.shirakumo.random-noise)

(declaim (inline lattice p0 p1 g0 g1 tt dt))
(defstruct (lattice
            (:conc-name NIL)
            (:constructor make-lattice (&optional (p0 0) (p1 0) (g0 0f0) (g1 0f0) (tt 0f0) (dt 0f0)))
            (:copier NIL)
            (:predicate NIL))
  (p0 0 :type (unsigned-byte 32))
  (p1 0 :type (unsigned-byte 32))
  (g0 0f0 :type single-float)
  (g1 0f0 :type single-float)
  (tt 0f0 :type single-float)
  (dt 0f0 :type single-float))

(defun normal-lattice (coordinate frequency &optional (lattice (make-lattice)))
  (declare (type lattice lattice))
  (declare (type single-float coordinate frequency))
  (let* ((coordinate (* coordinate frequency))
         (point (the (unsigned-byte 32) (abs (floor coordinate))))
         (tt (- coordinate point)))
    (setf (p0 lattice) point)
    (setf (p1 lattice) (+ point 1))
    (setf (g0 lattice) (- coordinate point))
    (setf (g1 lattice) (- (g0 lattice) 1))
    (setf (tt lattice) (* tt tt tt (+ (* tt (- (* tt 6) 15)) 10)))
    (setf (dt lattice) (* tt tt (+ (* tt (- (* tt 30) 60)) 30)))
    lattice))

(defun normal-lattice/validate (point frequency)
  (declare (ignore frequency))
  point)

(defun tiling-lattice (coordinate frequency &optional (lattice (make-lattice)))
  (declare (type lattice lattice))
  (declare (type single-float coordinate frequency))
  (let* ((coordinate (* coordinate frequency))
         (point (the (unsigned-byte 32) (floor coordinate)))
         (tt (- coordinate point))
         (p0 (- point (truncate (* frequency (ceiling (/ point frequency)))))))
    (setf (g0 lattice) (- coordinate point))
    (setf (g1 lattice) (- (g0 lattice) 1))

    (setf (p0 lattice) (if (< p0 0) (truncate (+ p0 frequency)) p0))
    (setf (p1 lattice) (+ (p0 lattice) 1))
    (setf (p1 lattice) (if (= (p1 lattice) frequency) 0 (p1 lattice)))
    
    (setf (tt lattice) (* tt tt tt (+ (* tt (- (* tt 6) 15)) 10)))
    (setf (dt lattice) (* tt tt (+ (* tt (- (* tt 30) 60)) 30)))
    lattice))

(defun tiling-lattice/validate (point frequency)
  (cond ((= -1 point)
         (1- frequency))
        ((= point frequency)
         0)
        (T
         point)))

(declaim (inline lattice-validator))
(defun lattice-validator (lattice)
  (cond ((eq lattice #'normal-lattice) #'normal-lattice/validate)
        ((eq lattice #'tiling-lattice) #'tiling-lattice/validate)
        (T (error "Unknown lattice type."))))

(define-noise-function lattice 1 ((lattice function) (gradient function))
  (let ((span (make-lattice)))
    (declare (dynamic-extent span))
    (funcall lattice position frequency span)
    (with-sample a (funcall gradient (xxhash-eat xxhash (p0 span)) (g0 span))
      (with-sample b (funcall gradient (xxhash-eat xxhash (p1 span)) (g1 span))
        (sample (lerp a b (tt span))
                (* frequency (+ (lerp adx bdx (tt span)) (* (dt span) (- b a)))))))))

(define-noise-function lattice 2 ((lattice function) (gradient function))
  (let ((x (make-lattice)) (y (make-lattice)))
    (declare (dynamic-extent x y))
    (funcall lattice (aref position 0) frequency x)
    (funcall lattice (aref position 1) frequency y)
    (let ((h0 (xxhash-eat xxhash (p0 x)))
          (h1 (xxhash-eat xxhash (p1 x))))
      (with-samples ((a (funcall gradient (xxhash-eat h0 (p0 y)) (g0 x) (g0 y)))
                     (b (funcall gradient (xxhash-eat h0 (p1 y)) (g0 x) (g1 y)))
                     (c (funcall gradient (xxhash-eat h1 (p0 y)) (g1 x) (g0 y)))
                     (d (funcall gradient (xxhash-eat h1 (p1 y)) (g1 x) (g1 y))))
        (sample (lerp (lerp a b (tt y)) (lerp c d (tt y)) (tt x))
                (* frequency (+ (lerp (lerp adx bdx (tt y)) (lerp cdx ddx (tt y)) (tt x))
                                (* (- (lerp c d (tt y)) (lerp a b (tt y))) (dt x))))
                (* frequency (lerp (+ (lerp ady bdy (tt y)) (* (- b a) (dt y)))
                                   (+ (lerp cdy ddy (tt y)) (* (- d c) (dt y)))
                                   (tt x))))))))

(define-noise-function lattice 3 ((lattice function) (gradient function))
  (let ((x (make-lattice)) (y (make-lattice)) (z (make-lattice)))
    (declare (dynamic-extent x y z))
    (funcall lattice (aref position 0) frequency x)
    (funcall lattice (aref position 1) frequency y)
    (funcall lattice (aref position 2) frequency z)
    (let* ((h0 (xxhash-eat xxhash (p0 x)))
           (h1 (xxhash-eat xxhash (p1 x)))
           (h00 (xxhash-eat h0 (p0 y)))
           (h01 (xxhash-eat h0 (p1 y)))
           (h10 (xxhash-eat h1 (p0 y)))
           (h11 (xxhash-eat h1 (p1 y))))
      (with-samples ((a (funcall gradient (xxhash-eat h00 (p0 z)) (g0 x) (g0 y) (g0 z)))
                     (b (funcall gradient (xxhash-eat h00 (p1 z)) (g0 x) (g0 y) (g1 z)))
                     (c (funcall gradient (xxhash-eat h01 (p0 z)) (g0 x) (g1 y) (g0 z)))
                     (d (funcall gradient (xxhash-eat h01 (p1 z)) (g0 x) (g1 y) (g1 z)))
                     (e (funcall gradient (xxhash-eat h10 (p1 z)) (g1 x) (g0 y) (g0 z)))
                     (f (funcall gradient (xxhash-eat h10 (p1 z)) (g1 x) (g0 y) (g1 z)))
                     (g (funcall gradient (xxhash-eat h11 (p1 z)) (g1 x) (g1 y) (g0 z)))
                     (h (funcall gradient (xxhash-eat h11 (p1 z)) (g1 x) (g1 y) (g1 z))))
        (sample (lerp (lerp (lerp a b (tt z)) (lerp c d (tt z)) (tt y))
                      (lerp (lerp e f (tt z)) (lerp g h (tt z)) (tt y))
                      (tt x))
                (* frequency
                   (+ (lerp (lerp (lerp adx bdx (tt z)) (lerp cdx ddx (tt z)) (tt y))
                            (lerp (lerp edx fdx (tt z)) (lerp gdx hdx (tt z)) (tt y))
                            (tt x))
                      (* (- (lerp (lerp e f (tt z)) (lerp g h (tt z)) (tt y))
                            (lerp (lerp a b (tt z)) (lerp c d (tt z)) (tt y)))
                         (dt x))))
                (* frequency
                   (lerp (lerp (+ (lerp adz bdz (tt z)) (* (- b a) (dt z)))
                               (+ (lerp cdz ddz (tt z)) (* (- d c) (dt z)))
                               (tt y))
                         (lerp (+ (lerp edz fdz (tt z)) (* (- f e) (dt z)))
                               (+ (lerp gdz hdz (tt z)) (* (- h g) (dt z)))
                               (tt y))
                         (tt x))))))))

(define-noise-toplevel lattice ((lattice function) (gradient function))
  lattice gradient)
