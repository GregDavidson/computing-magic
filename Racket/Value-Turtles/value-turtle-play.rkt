#lang racket

(require graphics/value-turtles
         threading )

;; threading makes it easy to pass the evolving turtle drawings
;; through the functions without syntactic nesting.  The turtle
;; drawing will be passed as the first argument or to where the
;; wildcard argument _ indicates.

;; ** Default Canvas

(define WIDTH 1000)
(define HEIGHT 1000)
(define TPIC (turtles WIDTH HEIGHT))

;; ** Angle Issues

;; Let's use radians!
(define CIRCLE #; 360 (* 2 pi))

;; normalize angle to between 0 and 2π
;; DO WE NEED THIS??
(define (normalize-angle angle)
  (cond [(negative? angle) (normalize-angle (+ angle CIRCLE))]
        [(>= angle CIRCLE) (normalize-angle (- angle CIRCLE))]
        [else angle] ) )

(define (left tpic angle) (turn/radians (normalize-angle angle) tpic))
(define (right tpic angle) (left tpic (- angle)))

;; turn the turtle the given fraction of a CIRCLE,
;; positive or negative!
(define (τ tpic fraction)
  (left tpic (/ CIRCLE fraction)) )

;; a nice special case!
(define (flip tpic) (τ tpic 2))

;; ** The rest of the primitives

;; tpic as first argument for easy threading!

(define (dra tpic length) (draw length tpic) )
(define (mov tpic length) (move length tpic) )

;; ** Fancy Drawing Functions

;; All of these functions should leave the turtle
;; location and orientation as it started out!

(define (regular-polygon tpic num-sides side-size)
  (let loop ( [n num-sides] [tpic tpic] )
    (if (zero? n)
        tpic
        (loop (sub1 n)
              (~> tpic
                  (dra side-size)
                  (τ num-sides) ) ) ) ) )

#; (regular-polygon TPIC 5 100)

(define (rectangle tpic width height)
  (~> tpic
      (dra width)
      (τ 4)
      (dra height)
      (τ 4)
      (dra width)
      (τ 4)
      (dra height)
      (τ 4) ) )

;; an isosceles triangle sitting on its base
(define (isosceles-triangle tpic base height)
  (let ( [base-angle (atan (* 2 (/ height base)))]
         [side (sqrt (+ (sqr height) (/ (sqr base) 4)))] )
    (~> tpic
        (dra base)
        (flip)
        (right base-angle)
        (dra side)
        (mov (- side))
        (left base-angle)
        (mov base)
        (flip)
        (left base-angle)
        (dra side)
        (mov (- side))
        (right base-angle) ) ) )

;; ** Things we might want to chain

;; The turtle position might not be left unchanged!

;; a universal threadable turtle conditional!
(define (turtle-if tpic turtle-thunk doit?)
  (if doit? (turtle-thunk tpic) tpic) )

;; an example: mv-if
(define (mv-if tpic length mov?)
  (turtle-if tpic (λ (tpic) (mov tpic length)) mov?) )

(define (shed tpic side #:right [right? #f])
    (~> tpic
      (regular-polygon 4 side)
      (τ 4)
      (mov side)
      (τ -4)
      (regular-polygon 3 side)
      (τ -4)
      (mov side)
      (τ 4)
      (mv-if side right?) ) )

#; (shed TPIC 60)

(define (house tpic width height #:right [right? #f])
  (~> tpic
      (rectangle width height)
      (τ 4)
      (mov height)
      (τ -4)
      (isosceles-triangle width (/ height 2))
      (τ -4)
      (mov height)
      (τ 4)
      (mv-if width right?) ) )

#; (house TPIC 180 40)

;; ** Use everything for a fancy drawing

;; The turtle position will not be left unchanged!
;; It will back up a bit, then advance left-to-right.

(let ( [side 60] [offset 40] )
  (~> TPIC
      (mov (- 0 side offset side))
      (shed side #:right #t)
      (mov offset)
      (house (* 3 side) side #:right #t) ) )
