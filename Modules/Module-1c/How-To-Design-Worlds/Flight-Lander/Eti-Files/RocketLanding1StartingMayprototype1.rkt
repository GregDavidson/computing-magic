#lang racket
;;this is a game in which you need to land a rocket without crashing.
;;realistic with gravity, thrust fuel and a nice flame

(require 2htdp/universe) ; for big-bang
(require 2htdp/image)

(define (half x) (quotient x 2))

;; Like place-image, but relative to the south (lower-middle)
;;  place the top image y pixels above the bottom of the bottom canvas
;; NOTE Images have keyholes- canvases don't
(define (place-image-above top-image y bottom-canvas)
  (let ([center-x (half (image-width bottom-canvas))]
        [center-y (-  (image-height bottom-canvas) y (half (image-height top-image) ))] )
       (place-image top-image center-x center-y bottom-canvas) ) )

(define WIDTH 600)
(define HEIGHT 800)
(define CANVAS (empty-scene WIDTH HEIGHT) )

(define ROCKET (bitmap "image/kevrocket1cropped.png"))
(define FLAME (bitmap "image/kevrocket1flame.png"))

(struct sprite (image1 image2 y dy ddy dry-weight fuel thrusting? on-tick on-key draw)
  #:transparent
  #:constructor-name make-sprite)

(define rocket (make-sprite ROCKET FLAME HEIGHT 0 -1 1  400 #f #f #f #f))

;;make each object update itself!!
(define (update-world world)
  (make-sprite (sprite-image1 world)
               (sprite-image2 world)
               (+ (sprite-y world)(sprite-dy world))
               (+(sprite-dy world)(sprite-ddy world))
               (sprite-ddy world)
               (sprite-dry-weight world)
               (sprite-fuel world)
               (sprite-thrusting? world)
               (sprite-on-tick world)
               (sprite-on-key world)
               (sprite-draw world) ) )
  
(define (update-world-on-key world a-key)
  world)

(define (game-over w)
  (<= (sprite-y w) 0) )

(define (draw-world world)
  (place-image-above (sprite-image1 world)
                     (sprite-y world)
                     CANVAS) )
  
;; create a world with
;; - the size of our screen
;; - 30 clock ticks per second
;; - initial "world-state" (a list of sprites)
(big-bang rocket
  (on-tick update-world 1/30)
  (to-draw draw-world WIDTH HEIGHT)
  (on-key update-world-on-key)
  (stop-when game-over ))