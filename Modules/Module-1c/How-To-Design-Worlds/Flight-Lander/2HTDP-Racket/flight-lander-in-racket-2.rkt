#lang racket

;; * Flight Lander Game, Version (2htdp Lang Racket)

;; ** Synopsis

;; the plane moves left-to-right, wrapping
;; - the plane slowly descends
;; - up/down keys give extra vertical motion
;; - game ends when land or sink to bottom
;; - sinking is slow
;; - almost landed planes fall back into the water
;; - the language is now full Racket
;; - the foreground image is read from a file

;; inspired by Chapter 5 of the Tutorial
;;   How to Design Worlds https://world.cs.brown.edu/
;; brought up to date with the 2nd edition of
;;   How To Design Programs https://htdp.org/
;; using Lang Racket

;; ** Required Packages

(require 2htdp/universe) ; for big-bang
(require 2htdp/image)
(require rackunit) ; for checks
(require file/convertible) ; for image conversions
(require pict/convert)

;; ** The Data Structures

;; *** Type Checking

;; Make an expectation function which takes a value
;; which is supposed to conform to the expectation function.
;; If it conforms, return the value.  If it doesn't,
;; raise an error with the name and the value. 
(define (make-expect expectation? name)
  (λ (val) (if (expectation? val) val (error "Failed expectation ~a: ~a" name val))) )

;; A function which expects a non-negative integer, i.e. a "natural number".
(define the-natural
  (make-expect (λ (x) (and (integer? x) (>= x 0))) 'non-negative-integer) )

;; *** posn - Position Structure

;; Note: The guards allow x and y values which exceed width and height
;; in order to allow creating values which signal that an object
;; has gone off screen.

;; posn represents 2-D positions in the scene.
;; We can use either posn or make-posn to construct positions.
;; Our guard is a bit permissive as x and y should be strictly
;; less than WIDTH and HEIGHT.  But current program flow may
;; generate such values temporarily before wrapping or game-over.
;; The flow should be fixed to prevent this!!
;; Then the guard should become more strict!!
(struct	posn (x y)
  #:extra-constructor-name make-posn
  #:guard  (λ (x y name) ; check that our x and y values are valid
             (the-natural x)
             (the-natural y)
             (values x y) ) )

;; *** movable - foreground object structure

(define the-image (make-expect (λ (x) (image? x) 'image)) )
(define the-posn (make-expect (λ (x) (posn? x) 'posn)) )

(struct	movable (image locn)
  #:guard  (λ (image locn name) ; check that our components are valid
             (the-image locn)
             (the-posn locn)
             (values image locn) ) )

;; ** The Background

;; size of the total image
(define WIDTH 800)
(define HEIGHT 500)

;; the base (bottom of image) consists of water followed by land
(define BASE-HEIGHT 50)
(define BASE-POSN-Y (- HEIGHT BASE-HEIGHT))
(define WATER-WIDTH (* 5/8 WIDTH))

;; Using these constants, here’s an image to represent water:
(define WATER (rectangle WATER-WIDTH BASE-HEIGHT "solid" "blue"))
;; and another one to represent land:
(define LAND (rectangle (- WIDTH WATER-WIDTH) BASE-HEIGHT "solid" "brown"))

;; A lot of the 2htdp image functions work with the center
;; of images.  These functions help with that!

(define (half-width image) (/ (image-width image) 2))
(define (half-height image) (/ (image-height image) 2))

;; like place-image, but relative to the southwest (lower-left)
;; corner rather than the center of the image being placed
;; try: (sw-place-image PLANE 0 (image-height WATER) WATER)
(define (sw-place-image image x y scene)
  (let ( [center-x (+ x (half-width image))]
         [center-y (- y (half-height image))] )
    (place-image image center-x center-y scene) ) )

(define BACKGROUND
  (sw-place-image WATER
               0 HEIGHT
               (sw-place-image LAND
                            WATER-WIDTH HEIGHT
                            (empty-scene WIDTH HEIGHT) ) ) )

;; ** The Foreground Objects

;; *** The Plane

(define PLANE (bitmap "../Images/airplane-small-clipped-alpha.png"))
;; Start plane fully visible at upper left
(define PLANE-START (make-posn 0 (image-height PLANE)))
(define PLANE-MOVE-X 5) ; plane horizontal velocity component
(define PLANE-MOVE-Y 5) ; plane vertical velocity component
(define PLANE-SINK-Y 1) ; plane sink rate in water

;; By default, images are drawn relative to their centers.
;; We change this below so that we draw the plane
;; relative to x=center, y=bottom
(define PLANE-WIDTH (image-width PLANE))
(define PLANE-HEIGHT (image-height PLANE))
;; Inspect image to get correct value!!!
;; We're using 1/2 plane width in this prototype!!!
(define PLANE-WHEELS-X (half-width PLANE)) ; x distance from tail to wheels

;; for tests:
(define PLANE-SUNK (make-posn 0 HEIGHT))
(define PLANE-ALMOST-LANDED (make-posn (- WATER-WIDTH PLANE-WHEELS-X) BASE-POSN-Y)) ; falls back and sinks
(define PLANE-BARELY-LANDED (make-posn WATER-WIDTH BASE-POSN-Y)) ; wheels just barely on land
(define PLANE-FULLY-LANDED (make-posn (+ WATER-WIDTH PLANE-WHEELS-X) BASE-POSN-Y))

;; ** The World State

;; the world state is the part of the world which changes
;; our world state is the x and y position of the plane
;; stored in a posn (position) structure
;; - 0, 0 is in the upper-left of the scene
;; we also have a "virtual parameter" "in-flight"

;; *** Examining The World State

;; above values are smaller
;; from 0 at top to HEIGHT at bottom
(define (posn-above-y? pos y)
 (< (posn-y pos) y) )

(check-true (posn-above-y? PLANE-START BASE-POSN-Y))
(check-true (posn-above-y? PLANE-START HEIGHT))
(check-false (posn-above-y? PLANE-FULLY-LANDED BASE-POSN-Y))
(check-false (posn-above-y? PLANE-SUNK BASE-POSN-Y))

(define (in-flight? pos)
  (posn-above-y? pos BASE-POSN-Y) )

(check-pred in-flight? PLANE-START)
(check-false (in-flight? PLANE-SUNK))
(check-false (in-flight? PLANE-FULLY-LANDED))

(define (posn-past-x pos x)
  (- (posn-x pos) x) )

(define (posn-past-x? pos x)
  (positive? (posn-past-x pos x)) )

;; how far our plane's nose extends over land
(define (nose-over-land pos)
  (max 0 (posn-past-x pos (- WATER-WIDTH PLANE-WIDTH))) )

(check-eq? (nose-over-land PLANE-START) 0)
#;(check-true (< (nose-over-land PLANE-ALMOST-LANDED) PLANE-WHEELS-X))
(check-true (>= (nose-over-land PLANE-BARELY-LANDED) PLANE-WHEELS-X))
(check-true (> (nose-over-land PLANE-FULLY-LANDED) PLANE-WHEELS-X))

;; our plane's wheels are over land
(define (over-land? pos)
  (posn-past-x? pos (- WATER-WIDTH PLANE-WHEELS-X)) )

(check-false (over-land? PLANE-START))
(check-false (over-land? PLANE-ALMOST-LANDED))
(check-true (over-land? PLANE-BARELY-LANDED))
(check-true (over-land? PLANE-FULLY-LANDED))

(define (landed? pos)
  (and (not (in-flight? pos)) (over-land? pos)) )

;; *** Updating The World State

;; x position wraps around
;; y position decreases
;; + don't worry about landing!
(define (move-plane-on-tick pos)
  (let ( [x (posn-x pos)] [y (posn-y pos)] )
    (cond [ (in-flight? pos)
            (make-posn (if (> x WIDTH) 0 (+ x PLANE-MOVE-X))
                       (+ y PLANE-MOVE-Y) ) ]
          [ (landed? pos) pos ] ;; this would be game over!
          [ else ; sinking!
            (make-posn (- x (nose-over-land pos)) ; we may fall back
                       (+ y PLANE-SINK-Y) ) ] ) ) )

;; for tests
(define (delta-plane-on-tick pos)
  (let ( [new-pos (move-plane-on-tick pos)] )
    (list (- (posn-x new-pos) (posn-x pos)) (- (posn-y new-pos) (posn-y pos))) ) )

(check-equal? (delta-plane-on-tick PLANE-START) (list PLANE-MOVE-X PLANE-MOVE-Y))

;; ** Rendering (drawing)

; try: (place-plane (posn WATER-WIDTH BASE-POSN-Y))
(define (place-plane pos)
  (sw-place-image PLANE (posn-x pos) (posn-y pos) BACKGROUND) )

;; ** Responding To The User

(define KEY-DISTANCE 20)

;; return a new position with the y changed by delta
(define (delta-y pos delta)
  (make-posn (posn-x pos) (+ delta (posn-y pos))) )

;; Given a position and a meaningful key,
;; return an updated position.
(define (move-plane-on-key a-pos a-key)
  (cond [(key=? a-key "up") (delta-y a-pos (- KEY-DISTANCE))]
        [(key=? a-key "down") (delta-y a-pos KEY-DISTANCE)]
        [else a-pos] ) )

;; ** Game Over

(define (landed-or-sunk? pos)
  (or (landed? pos) (not (posn-above-y? pos HEIGHT))) )

(check-false (landed-or-sunk? PLANE-START))
(check-false (landed-or-sunk? PLANE-ALMOST-LANDED))
(check-true (landed-or-sunk? PLANE-BARELY-LANDED))
(check-true (landed-or-sunk? PLANE-FULLY-LANDED))
(check-true (landed-or-sunk? (make-posn 0 HEIGHT)))
(check-false (landed-or-sunk? (make-posn 0 (- HEIGHT 1))))
              
;; ** Running The Game

;; create a world with
;; - the size of our screen
;; - 30 clock ticks per second
;; - initial "world-state" of 0 (plane x location)
(big-bang PLANE-START
  (on-tick move-plane-on-tick 1/30)
  (to-draw  place-plane WIDTH HEIGHT)
  (on-key move-plane-on-key)
  (stop-when landed-or-sunk?) )
