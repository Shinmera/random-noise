(asdf:load-system :staple-markless)
(defmethod staple:subsystems ((s (eql (asdf:find-system :random-noise)))) ())
(defmethod staple:images ((s (eql (asdf:find-system :random-noise)))) ())
