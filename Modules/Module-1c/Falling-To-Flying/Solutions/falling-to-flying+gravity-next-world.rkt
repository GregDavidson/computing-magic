(define (next-world world)
  (let* ( ;; when emerging (at the top) the ball maintains the emerging-dy
          ;; otherwise it accelerates due to gravity
          [new-dy (if (emerging? world) EMERGING-DY (+ (ball-dy world) GRAVITY))]
          ;; a bouncing ball (at the bottom) will return to the top of the scene
          [new-y (if (bouncing? world) SCENE-HEIGHT (+ (ball-y world) new-dy))] )
    (make-ball (ball-shape world) ; not changing the shape, but we could!
               new-y new-dy ) ) )
