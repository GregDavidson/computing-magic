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

(define SCENE-WIDTH 200) ; pixels
(define HALF-SCENE-WIDTH (quotient SCENE-WIDTH 2))
(define SCENE-HEIGHT 200) ; pixels
(define EMPTY-SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

;; Something to fall, why not a ball?

(define BALL (circle 20 "solid" "blue"))
(define DROP-RATE 2) ; pixels per "tick"

;; The state of our "World" is just the ball's height

(define world 0)

(define (next-world world)
  (remainder (+ world DROP-RATE) SCENE-HEIGHT) )

(define (draw-world world)
  (place-image BALL HALF-SCENE-WIDTH world EMPTY-SCENE) )

(big-bang world
  [on-tick next-world 1/30] ; update world 30 "ticks" a second
  [to-draw draw-world SCENE-WIDTH SCENE-HEIGHT] ; draw world in canvas
  )