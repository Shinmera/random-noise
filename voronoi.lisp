(in-package #:org.shirakumo.random-noise)

(define-noise-function voronoi-noise/1d (point deriv)
  )

(define-noise-function voronoi-noise/2d (point deriv)
  )

(define-noise-function voronoi-noise/3d (point deriv)
  )

(define-noise-entry-function voronoi-noise
  (vec3 voronoi-noise/3d)
  (vec2 voronoi-noise/2d)
  (float voronoi-noise/1d))
