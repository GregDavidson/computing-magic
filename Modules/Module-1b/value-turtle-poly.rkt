#lang racket
(require graphics/value-turtles)
(define t (turtles 300 300))
(define (poly num-sides side-length t)
  (define circle 360)
  (define angle (/ circle num-sides))
  (define (draw-sides num-sides t)
    (if (= 0 num-sides)
        t
        (draw-sides
         (- num-sides 1)
         (turn angle (draw side-length t)) ) ) )
  (draw-sides num-sides t) )

#;(poly 4 50 t)
#;(turtles-pict (poly 4 50 (set-pen-width t 4)))