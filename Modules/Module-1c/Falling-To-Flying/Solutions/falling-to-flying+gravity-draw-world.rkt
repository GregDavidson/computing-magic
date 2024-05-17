;; Return a new scene with the world (the ball)
;; centered horizontally and at its correct y height.
(define (draw-world world)
  (place-image (ball-shape world)
               HALF-SCENE-WIDTH
               ;; Our y represents height above ground
               ;; so it goes from SCENE-HEIGHT down to 0.
               ;; place-image wants to know where to put
               ;; the center of the ball relative to the
               ;; upper-left corner of the scene.
               (round (- SCENE-HEIGHT (ball-y world)))
               EMPTY-SCENE ) )
