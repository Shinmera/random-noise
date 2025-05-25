(asdf:defsystem random-noise
  :version "0.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Functions to generate various types of noise such as Perlin or Value noise"
  :homepage "https://Shinmera.github.io/random-noise/"
  :bug-tracker "https://github.com/Shinmera/random-noise/issues"
  :source-control (:git "https://github.com/Shinmera/random-noise.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "value")
               (:file "perlin")
               (:file "simplex")
               (:file "voronoi")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-math)
  :in-order-to ((asdf:test-op (asdf:test-op :random-noise/test))))

(asdf:defsystem random-noise/test
  :version "0.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Tests for the random-noise system"
  :homepage "https://Shinmera.github.io/random-noise/"
  :bug-tracker "https://github.com/Shinmera/random-noise/issues"
  :source-control (:git "https://github.com/Shinmera/random-noise.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:random-noise :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.random-noise.test)))
