#lang racket
;; Start with Falling, move towards Flying!

(require pict) ; for drawing, image manipulation
(require (only-in 2htdp/image
                  empty-scene place-image))
         ; different drawing, image manipulation
(require 2htdp/universe) ; animation and game framework

(define (place-pict-on-scene pict x y scene)
  (place-image (pict->bitmap pict) x y scene) )

;; Establish the Scene

;; Notes:
;; (a) Contrary to mathematics conventions, with place-image from 2htdp/image,
;; Horizontal (x) coordinates begin with 0 on the left and increase to the right
;; Vertical (y) coordinates begin with 0 at the top and increase downwards.
;; (2) Images created with 2htdp/image are placed relative to their "keyholes"
;; which default to the center of each image.

(define SCENE-WIDTH 200) ; pixels
(define HALF-SCENE-WIDTH (quotient SCENE-WIDTH 2))
(define SCENE-HEIGHT 200) ; pixels
(define EMPTY-SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

;; Something to fall, why not a ball?

(define BALL (disk 20 #:color "blue"))
(define DROP-RATE 2) ; pixels per "tick"

;; The state of our "World" is just the ball's height

(define world 0)

(define (next-world world)
  (remainder (+ world DROP-RATE) SCENE-HEIGHT) )

(define (draw-world world)
  (place-pict-on-scene BALL HALF-SCENE-WIDTH world EMPTY-SCENE) )

(big-bang world
  [on-tick next-world 1/30] ; update world 30 "ticks" a second
  [to-draw draw-world SCENE-WIDTH SCENE-HEIGHT] ; draw world in canvas
  )