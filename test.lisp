(defpackage #:org.shirakumo.random-noise.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:noise #:org.shirakumo.random-noise)))
(in-package #:org.shirakumo.random-noise.test)

(define-test random-noise)
