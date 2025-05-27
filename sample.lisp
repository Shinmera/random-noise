(in-package #:org.shirakumo.random-noise)

(deftype point (&optional size)
  (etypecase size
    ((eql *) '(or single-float (simple-array single-float (*))))
    ((eql 1) 'single-float)
    ((integer 2) `(simple-array single-float (,size)))))

(declaim (inline sample))
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

(declaim (inline smoothstep))
(defun smoothstep (v &optional (dx 0f0) (dy 0f0) (dz 0f0))
  (declare (type single-float v dx dy dz))
  (let ((d (* 6 v (- 1 v))))
    (sample (* v v (- 3 (* 2 v)))
            (* dx d)
            (* dy d)
            (* dz d))))

(declaim (inline curl))
(defun curl (v &optional (dx 0f0) (dy 0f0) (dz 0f0))
  (declare (type single-float v dx dy dz))
  (sample (abs v)
          (if (<= 0 v) dx (- dx))
          (if (<= 0 v) dy (- dy))
          (if (<= 0 v) dz (- dz))))

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
                               (values single-float single-float single-float single-float &optional))
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
