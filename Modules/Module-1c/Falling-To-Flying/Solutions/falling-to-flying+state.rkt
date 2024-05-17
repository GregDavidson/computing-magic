#lang racket
;; Start with Falling, move towards Flying!

(require 2htdp/image) ; for drawing, image manipulation
(require 2htdp/universe) ; animation and game framework

;; Establish the Scene

;; Issues with the 2htdp/image library:
;; Horizontal (x) coordinates begin with 0 on the left and increase to the right
;; Vertical (y) coordinates begin with 0 at the top and increase downwards
;; Images are placed relative to their "keyholes".
;; Keyholes default to the center of each image.
;; Scenes are like images but without keyholes.

(define (half num-pixels) (quotient num-pixels 2))

(define SCENE-WIDTH 200) ; pixels
(define HALF-SCENE-WIDTH (half SCENE-WIDTH))
(define SCENE-HEIGHT 200) ; pixels
(define EMPTY-SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

;; Something to fall, why not a ball?

(struct ball (shape y dy)
  #:transparent ; allows us to see inside of a ball when we print it
  #:constructor-name make-ball ; changes constructor name from ball to make-ball
  )

;; The state of our "World" is a ball with state attributes

(define initial-world
  (make-ball (circle 20 "solid" "blue") ; shape
             SCENE-HEIGHT ; y location = height-above-ground
             -2 ; dy = drop rate
             ) )

(define HALF-BALL-HEIGHT (half (image-height (ball-shape initial-world))))

;; is the ball bouncing?
(define (bouncing? ball)
  (< (ball-y ball) HALF-BALL-HEIGHT) )

(define (next-world world)
  (let ( [y+dy (+ (ball-y world) (ball-dy world))] )
    (make-ball (ball-shape world) ; same shape
               (if (bouncing? world)
                   SCENE-HEIGHT ; drop it again
                   y+dy )
               (ball-dy world) ; same dy
               ) ) )

(define (draw-world world)
  (place-image (ball-shape world)
               HALF-SCENE-WIDTH
               (- SCENE-HEIGHT (ball-y world))
               EMPTY-SCENE) )

(big-bang initial-world
  [on-tick next-world 1/30] ; update world 30 "ticks" a second
  [to-draw draw-world SCENE-WIDTH SCENE-HEIGHT] ; draw world in canvas
  )
