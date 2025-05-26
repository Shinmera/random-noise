(in-package #:org.shirakumo.random-noise)

(declaim (inline smooth smooth-dt))
(defun smooth (x)
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun smooth-dt (x)
  (* 30 x x (1+ (* x (- x 2)))))

(declaim (type (simple-array (unsigned-byte 8) (512)) +noise+))
(#-sbcl defvar #+sbcl sb-ext:defglobal +noise+
        (coerce '(151 160 137  91  90  15 131  13 201  95  96  53 194 233   7 225 
		          140  36 103  30  69 142   8  99  37 240  21  10  23 190   6 148 
		          247 120 234  75   0  26 197  62  94 252 219 203 117  35  11  32 
		          57 177  33  88 237 149  56  87 174  20 125 136 171 168  68 175 
		          74 165  71 134 139  48  27 166  77 146 158 231  83 111 229 122 
		          60 211 133 230 220 105  92  41  55  46 245  40 244 102 143  54 
		          65  25  63 161   1 216  80  73 209  76 132 187 208  89  18 169 
		          200 196 135 130 116 188 159  86 164 100 109 198 173 186   3  64 
		          52 217 226 250 124 123   5 202  38 147 118 126 255  82  85 212 
		          207 206  59 227  47  16  58  17 182 189  28  42 223 183 170 213 
		          119 248 152   2  44 154 163  70 221 153 101 155 167  43 172   9 
		          129  22  39 253  19  98 108 110  79 113 224 232 178 185 112 104 
		          218 246  97 228 251  34 242 193 238 210 144  12 191 179 162 241 
		          81  51 145 235 249  14 239 107  49 192 214  31 181 199 106 157 
		          184  84 204 176 115 121  50  45 127   4 150 254 138 236 205  93 
		          222 114  67  29  24  72 243 141 128 195  78  66 215  61 156 180 

		          151 160 137  91  90  15 131  13 201  95  96  53 194 233   7 225 
		          140  36 103  30  69 142   8  99  37 240  21  10  23 190   6 148 
		          247 120 234  75   0  26 197  62  94 252 219 203 117  35  11  32 
		          57 177  33  88 237 149  56  87 174  20 125 136 171 168  68 175 
		          74 165  71 134 139  48  27 166  77 146 158 231  83 111 229 122 
		          60 211 133 230 220 105  92  41  55  46 245  40 244 102 143  54 
		          65  25  63 161   1 216  80  73 209  76 132 187 208  89  18 169 
		          200 196 135 130 116 188 159  86 164 100 109 198 173 186   3  64 
		          52 217 226 250 124 123   5 202  38 147 118 126 255  82  85 212 
		          207 206  59 227  47  16  58  17 182 189  28  42 223 183 170 213 
		          119 248 152   2  44 154 163  70 221 153 101 155 167  43 172   9 
		          129  22  39 253  19  98 108 110  79 113 224 232 178 185 112 104 
		          218 246  97 228 251  34 242 193 238 210 144  12 191 179 162 241 
		          81  51 145 235 249  14 239 107  49 192 214  31 181 199 106 157 
		          184  84 204 176 115 121  50  45 127   4 150 254 138 236 205  93 
		          222 114  67  29  24  72 243 141 128 195  78  66 215  61 156 180)
                '(simple-array (unsigned-byte 8) (*))))

(declaim (inline noise))
(defun noise (x)
  (declare (type (integer 0 511) x))
  (declare (optimize speed (safety 0)))
  (aref +noise+ x))

(deftype noise-function ()
  '(function (vec3 single-float &optional vec3) (values single-float vec3 &optional)))

(defmacro define-noise-function (name (point deriv) &body body)
  (let ((freq (gensym "FREQUENCY"))
        (ppoint (gensym "POINT")))
    `(progn
       (declaim (ftype noise-function ,name))
       (defun ,name (,ppoint ,freq &optional (,deriv (vec3)))
         (declare (type vec3 ,ppoint ,deriv))
         (declare (type single-float ,freq))
         (declare (optimize speed (safety 1)))
         (let ((,point (vec3)))
           (declare (dynamic-extent ,point))
           (!v* ,point ,ppoint ,freq)
           (values (let* ((ix0 (the fixnum (floor (vx point))))
                          (iy0 (the fixnum (floor (vy point))))
                          (iz0 (the fixnum (floor (vz point))))
                          (tx0 (- (vx point) ix0))
                          (ty0 (- (vy point) iy0))
                          (tz0 (- (vz point) iz0))
                          (tx1 (- tx0 1))
                          (ty1 (- ty0 1))
                          (tz1 (- tz0 1))
                          (ix0 (logand ix0 255))
                          (iy0 (logand iy0 255))
                          (iz0 (logand iz0 255))
                          (ix1 (1+ ix0))
                          (iy1 (1+ iy0))
                          (iz1 (1+ iz0)))
                     (declare (ignorable iy0 iz0 ty0 tz0 tx1 ty1 tz1 iy0 iz0 iy1 iz1))
                     ,@body)
                   (nv* ,deriv ,freq)))))))

(defun accumulate (fun point frequency octaves lacunarity persistence &optional (deriv (vec3)))
  (declare (type vec3 point deriv))
  (declare (type noise-function fun))
  (declare (type single-float frequency lacunarity persistence))
  (declare (type (unsigned-byte 8) octaves))
  (declare (optimize speed (safety 1)))
  (let ((tmpderiv (vec3))
        (amplitude 1f0)
        (range 1f0)
        (sum (funcall fun point frequency deriv)))
    (declare (type single-float amplitude range sum))
    (loop for i from 1 below octaves
          do (setf frequency (* frequency lacunarity))
             (setf amplitude (* amplitude persistence))
             (setf range (+ range amplitude))
             (setf sum (+ sum (* amplitude (funcall fun point frequency tmpderiv))))
             (nv+ deriv tmpderiv))
    (setf range (/ range))
    (values (* sum range) (nv* deriv range))))

(defmacro define-noise-entry-function (name &body cases)
  `(progn
     (declaim (inline ,name))
     (defun ,name (point frequency &key (derivative (vec3)) (octaves 1) (lacunarity 1f0) (persistence 1f0))
       (declare (optimize speed))
       (let ((lacunarity (float lacunarity 0f0))
             (persistence (float persistence 0f0))
             (frequency (float frequency 0f0)))
         (etypecase point
           (vec3
            (accumulate #',(second (assoc 'vec3 cases)) point frequency octaves lacunarity persistence derivative))
           (vec2 
            (let ((point (vec3 (vx point) (vy point) 0f0)))
              (declare (dynamic-extent point))
              (accumulate #',(second (assoc 'vec2 cases)) point frequency octaves lacunarity persistence derivative)))
           (float
            (let ((point (vec3 point 0f0 0f0)))
              (declare (dynamic-extent point))
              (accumulate #',(second (assoc 'float cases)) point frequency octaves lacunarity persistence derivative))))))))

(defmacro with-lut (name table &body body)
  `(flet ((,name (x)
            (declare (optimize speed (safety 0)))
            (aref (load-time-value
                   (make-array ,(length table)
                               :initial-contents
                               (list ,@table)))
                  (logand x ,(1- (length table))))))
     (declare (inline ,name))
     ,@body))
