#lang racket/core
(require graphics/turtles)
(turtles #t)

(provide turtles τ circle move draw turn)

(define τ-radians (* 2 pi)) ; angle of a circle, in radians
(define τ-degrees 360) ; angle of a circle, in radians
(define τ τ-degrees) ; angle of a circle, in turtle package
(define (circle fraction) (* τ fraction))

