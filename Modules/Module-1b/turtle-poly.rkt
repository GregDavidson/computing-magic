#lang racket
(require graphics/turtles)
(turtles #t)

(define (poly num-sides size)
; (define circle (* 2 pi))
  (define circle 360)
  (define angle (/ circle num-sides))
  (do ([n 0 (+ 1 n)])
    [(>= n num-sides) n]
    (draw size)
    (turn angle) ) )
