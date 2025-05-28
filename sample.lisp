(in-package #:org.shirakumo.random-noise)

(deftype point (&optional size)
  (etypecase size
    ((eql *) '(or single-float (simple-array single-float (*))))
    ((eql 1) 'single-float)
    ((integer 2) `(simple-array single-float (,size)))))

(deftype sample ()
  '(values single-float single-float single-float single-float &optional))

(declaim (inline sample))
(declaim (ftype (function (single-float &optional single-float single-float single-float) sample) sample))
(defun sample (v &optional (dx 0f0) (dy 0f0) (dz 0f0))
  (values v dx dy dz))

(defmacro with-sample (spec s &body body)
  (flet ((s (a b)
           (intern (format NIL "~a~a" a b))))
    (destructuring-bind (&optional (v 'a) (dx (s v :dx)) (dy (s v :dy)) (dz (s v :dz)))
        (if (listp spec) spec (list spec))
      `(multiple-value-bind (,v ,dx ,dy ,dz) ,s
         (declare (type single-float ,v ,dx ,dy ,dz))
         (declare (ignorable ,dx ,dy ,dz))
         ,@body))))

(defmacro with-samples (specs &body body)
  (if specs
      `(with-sample ,(first (first specs)) ,(second (first specs))
         (with-samples ,(rest specs)
           ,@body))
      `(progn ,@body)))

(declaim (inline lerp))
(defun lerp (a b x)
  (declare (type single-float a b x))
  (+ (* a (- 1 x))
     (* b x)))

(defmacro s/ (op a b)
  `(with-sample a ,a
     (let ((b ,b))
       (sample (,op a b)
               (,op adx b)
               (,op ady b)
               (,op adz b)))))

(defmacro define-sample-function (name args &body body)
  `(progn
     (declaim (ftype (function ,(loop with kind = '&required
                                      for arg in args
                                      collect (if (listp arg)
                                                  (ecase kind
                                                    (&required (second arg))
                                                    (&optional (third arg))
                                                    (&key (list (intern (string (first arg)) "KEYWORD") (third arg))))
                                                  (setf kind arg)))
                               sample)
                     ,name))
     (defun ,name ,(loop for arg in args collect (cond ((not (listp arg)) arg)
                                                       ((cddr arg) (butlast arg))
                                                       (T (first arg))))
       (declare (optimize speed (safety 1)))
       ,@(loop for arg in args
               when (listp arg)
               collect `(declare (type ,(car (last arg)) ,(first arg))))
       ,@body)))

(defmacro define-noise-function (kind arity args &body body)
  (let ((name (intern (format NIL "~a/~d~a" kind arity :d))))
    `(define-sample-function ,name ((position (point ,arity)) (frequency single-float) (xxhash xxhash) ,@args)
       ,@body)))

(defmacro define-noise-toplevel (kind args &rest call-args)
  `(progn
     (declaim (inline ,kind))
     (define-sample-function ,kind ((position point) (frequency single-float) (xxhash xxhash) ,@args)
       (etypecase position
         ,@(loop for arity from 1 to 3
                 collect `((point ,arity)
                           (,(intern (format NIL "~a/~d~a" kind arity :d))
                            position frequency xxhash ,@call-args)))))))

(declaim (inline smoothstep))
(define-sample-function smoothstep ((v single-float) &optional (dx 0f0 single-float) (dy 0f0 single-float) (dz 0f0 single-float))
  (let ((d (* 6 v (- 1 v))))
    (sample (* v v (- 3 (* 2 v)))
            (* dx d)
            (* dy d)
            (* dz d))))

(defmacro smoothstep! (sampler)
  `(with-sample a ,sampler
     (smoothstep a adx ady adz)))

(declaim (inline turbulence))
(define-sample-function turbulence ((v single-float) &optional (dx 0f0 single-float) (dy 0f0 single-float) (dz 0f0 single-float))
  (sample (abs v)
          (if (<= 0 v) dx (- dx))
          (if (<= 0 v) dy (- dy))
          (if (<= 0 v) dz (- dz))))

(defmacro turbulence! (sampler)
  `(with-sample a ,sampler
     (turbulence a adx ady adz)))

(defun sample/1d (resolution frequency xxhash function &rest args)
  (let ((v (make-array (expt (1+ resolution) 1) :element-type 'single-float))
        (dx (make-array (expt (1+ resolution) 1) :element-type 'single-float))
        (i 0))
    (loop for p from 0f0 to 1f0 by (/ resolution)
          do (with-sample s (apply function p frequency xxhash args)
               (setf (aref v i) s)
               (setf (aref dx i) sdx))
             (incf i))
    (values v dx)))

(defun sample/2d (resolution frequency xxhash function &rest args)
  (let ((v (make-array (expt (1+ resolution) 2) :element-type 'single-float))
        (dx (make-array (expt (1+ resolution) 2) :element-type 'single-float))
        (dy (make-array (expt (1+ resolution) 2) :element-type 'single-float))
        (p (make-array 2 :element-type 'single-float))
        (i 0))
    (loop for py from 0f0 to 1f0 by (/ resolution)
          do (setf (aref p 1) py)
             (loop for px from 0f0 to 1f0 by (/ resolution)
                   do (setf (aref p 0) px)
                      (with-sample s (apply function p frequency xxhash args)
                        (setf (aref v i) s)
                        (setf (aref dx i) sdx)
                        (setf (aref dy i) sdy))
                      (incf i)))
    (values v dx dy)))

(defun sample/3d (resolution frequency xxhash function &rest args)
  (let ((v (make-array (expt (1+ resolution) 3) :element-type 'single-float))
        (dx (make-array (expt (1+ resolution) 3) :element-type 'single-float))
        (dy (make-array (expt (1+ resolution) 3) :element-type 'single-float))
        (dz (make-array (expt (1+ resolution) 3) :element-type 'single-float))
        (p (make-array 3 :element-type 'single-float))
        (i 0))
    (loop for pz from 0f0 to 1f0 by (/ resolution)
          do (setf (aref p 2) pz)
             (loop for py from 0f0 to 1f0 by (/ resolution)
                   do (setf (aref p 1) py)
                      (loop for px from 0f0 to 1f0 by (/ resolution)
                            do (setf (aref p 0) px)
                               (with-sample s (apply function p frequency xxhash args)
                                 (setf (aref v i) s)
                                 (setf (aref dx i) sdx)
                                 (setf (aref dy i) sdy)
                                 (setf (aref dz i) sdz))
                               (incf i))))
    (values v dx dy dz)))

(define-noise-function curl 2 ((generator function))
  (with-samples ((s (funcall generator position frequency xxhash)))
    (sample s
            sdy
            (- sdx))))

(define-noise-function curl 3 ((generator function) &optional (offset 100f0 single-float))
  (let ((x (aref position 0))
        (y (aref position 1))
        (z (aref position 2)))
    (flet ((p (x y z)
             (setf (aref position 0) x)
             (setf (aref position 1) y)
             (setf (aref position 2) z)
             position))
      (with-samples ((sx (funcall generator (p z y (+ x offset)) frequency xxhash))
                     (sy (funcall generator (p (+ x offset) z (+ y offset)) frequency xxhash))
                     (sz (funcall generator (p y (+ x offset) z) frequency xxhash)))
        (sample (/ (+ sx sy sz) 3)
                (- szdx sydy)
                (- sxdx szdy)
                (- sydx sxdy))))))
