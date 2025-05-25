(in-package #:org.shirakumo.random-noise)

(define-noise-function simplex-noise/1d (point deriv)
  )

(define-noise-function simplex-noise/2d (point deriv)
  )

(define-noise-function simplex-noise/3d (point deriv)
  )

(define-noise-entry-function simplex-noise
  (vec3 simplex-noise/3d)
  (vec2 simplex-noise/2d)
  (float simplex-noise/1d))
