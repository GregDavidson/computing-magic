#lang racket

(require 2htdp/image)
(require 2htdp/universe) ; for big-bang

;; The 2htdp/image library positions images by their centers.
;; Thus we often need to adjust things by half
(define (half n) (/ n 2))

(define scene-width 400)
(define scene-height 400)

(define rocket-image (bitmap "rocket-with-flame.png"))

;; Our world is simply the position of the rocket,
;; expressed as a list of the rocket's center's
;; x (horizontal) and y (vertical) positions.
(define rocket-x first)
(define rocket-y second)
(define rocket-xy list)

(define initial-world (rocket-xy (half scene-width) (half (image-height rocket-image))))

(define (update-world world)
  (rocket-xy (rocket-x world) (+ 1 (rocket-y world))) )

(define (draw-world world)
  (place-image rocket-image (rocket-x world) (rocket-y world)
               (empty-scene scene-width scene-height) ) )

(define (landed? world) (>= (rocket-y world) (- scene-height (half (image-height rocket-image)))))

(big-bang initial-world
  (on-tick update-world 1/30) ; call update-world 30 times a second
  (to-draw draw-world         ; updating our scene with draw-world
           scene-width scene-height) ; in a window fitting the scene size
  (stop-when landed?) )
