#lang racket
;; * Flight Lander Game, Version (2htdp Lang Racket)

;; ** Overview

;; Imperative Programming used sparingly.
;; Sprites are mutable values.

;; *** Exercises

;; Throughout the code there are several suggested
;; exercises marked with EXERCISE: which will make
;; this framework better.

;; Double and triple question ?? and exclamation !!
;; marks signal issues which should be addressed.

;; Iterate:
;; - Make this framework better.
;; - Imagine cool new features for this game and
;;   other games you could build using this framework.
;; - Try to add those new features and/or
;;   build those other games.
;; - How does this framework help you and
;;   what would make it better?

;; *** The Current Game

;; The plane moves left-to-right, wrapping
;; - the plane slowly descends
;; - up/down keys give extra vertical motion
;; - game ends when landed or sunk to the bottom
;; + sinking is slow
;; + almost landed plane falls back into the water

;; inspired by Chapter 5 of the Tutorial
;;   How to Design Worlds https://world.cs.brown.edu/
;; brought up to date with the 2nd edition of
;;   How To Design Programs https://htdp.org/
;; using #lang racket

(require 2htdp/universe) ; for big-bang
(require 2htdp/image)
(require rackunit) ; for checks
#;(require file/convertible) ; for image file conversions
#;(require pict/convert) ; for conversions between images

;; Stylistic comment:
;; Global immutable non-procedures are in UPPER-CASE

;; ** The Data Structures

;; Most lisp dialects provide a handy macro for defining
;; structures, i.e. objects with named fields.

;; Racket provides struct which has a #:guard clause for
;; checking structure validity when a structure is constructed,
;; but does nothing when the structure is mutated.

;; Racket also provides the more sophisticated struct/contract
;; which allows more general contracts to be expressed to
;; ensure a structure's validity.  And the syntax is nicer!

;; *** simple-sprite - Image + Position

;; A simple-sprite represents an image at a position in a 2D scene.

;; Our x and y values designate the bottom left corner
;; of the object's bounding box.

;; WARNING: The image drawing toolkit uses the
;; coordinates of the center of the image's bounding
;; box to draw it!

;; WARNING: Our guards allow x and y values which exceed
;; the scene's width and height in order to allow creating
;; values which signal that an object has gone off scene.
;; Is this a good idea??  What might be alternatives??

(struct/contract simple-sprite
                 ( [image image?] [x natural?] [y natural?] )
                 #:mutable #:transparent )

;; *** sprite - Image Object Structure

;; A sprite adds dx and dy values for movement, along with an update
;; method for creating a new sprite, e.g. on a clock tick.

;; The 'update-sprite! function below may be easier to use than the automatically
;; created 'sprite constructor which requires values for all of the fields!

;; Create an update-function which returns its object unchanged.
;; This can be used for sprites which don't want to do anything
;; on update.
(define (update-identity x) x)

(struct/contract sprite simple-sprite
                 ( [dx integer?] [dy integer?] [update procedure?] )
                 #:mutable #:transparent )

;; *** abstract-sprite, synonyms and update-sprite!

;; The two struct types simple-sprite and sprite are related.
;; abstract-sprite can be either one.

(define (abstract-sprite? obj) (or (simple-sprite? obj) (sprite? obj)))

;; Create sprite synonyms for the simple-sprite deconstructors
(define sprite-image simple-sprite-image)
(define sprite-left simple-sprite-x)      ; x value of left edge
(define sprite-bottom simple-sprite-y)    ; y value of bottom edge
;; Create sprite synonyms for the simple-sprite mutators
(define set-sprite-image! set-simple-sprite-image!)
(define set-sprite-x! set-simple-sprite-x!)    ; x value of left edge
(define set-sprite-y! set-simple-sprite-y!)    ; y value of bottom edge

;; Update an existing-sprite, mutating the specified field values.
(define (update-sprite! target-sprite
         ;; optional new values for simple-sprite fields
         #:image [new-image #f] #:x [new-x #f] #:y [new-y #f]
         ;; optional new values for sprite fields
         #:dx [new-dx #f] #:dy [new-dy #f] #:update [new-update #f] )
  (when new-image (set-sprite-image! target-sprite new-image))
  (when new-x (set-sprite-x! target-sprite new-x))
  (when new-y (set-sprite-y! target-sprite new-y))
  (when new-dx (set-sprite-dx! target-sprite new-dx))
  (when new-dy (set-sprite-dy! target-sprite new-dy))
  (when new-update (set-sprite-update! target-sprite new-update))
  (void) )

;; *** Additional simple-sprite property functions

;; We'll often need to split an integer in half,
;; note that it will be off by 1 for odd numbers:
;;   (half 5) --> 2
;; This is because we're working with exact pixel counts.
;; Question: How can we hide this problem??
(define (half x) (quotient x 2))

(define sprite-width (compose image-width sprite-image))
(define sprite-height (compose image-height sprite-image))
(define sprite-half-width (compose half sprite-width))
(define sprite-half-height (compose half sprite-height))
(define (sprite-center-x s) (+ (sprite-left s) (sprite-half-width s)))
(define (sprite-center-y s) (- (sprite-bottom s) (sprite-half-height s)))
;; Are the right and top edges part of the object or bounds??
(define (sprite-right s) (+ (sprite-left s) (sprite-width s))) ; right edge
(define (sprite-top s) (- (sprite-bottom s) (sprite-height s))) ; top edge

;; ** Overlap And Collision Detection

;; We consider three methods for detecting whether two sprites
;; collide
;; (1) Best method: Check if any non-transparent pixels overlap.
;;     Alas, this is expensive to compute and challenging to program.
;; (2) They get close enough that the distance between their centers
;;     is less than some minimum.
;;     Alas, we'll get false negatives, i.e. cases where the sprites
;;     are visually colliding but our algorithm won't trigger.
;; (3) Cheapest method: Check if the bounding boxes of the two
;;     sprites overlap.
;;     Alas, we'll get false positives, i.e. cases where the sprites
;;     only overlap their transparent regions, but our algorithm
;;     will trigger.

;; Method (2) is recommended by the How To Design Worlds tutorial.

;; A combination of (3) and (1) will work better than (2).
;; So let's go with (3) and enhance it with (1) later.

;; *** Check if the sprites bounds overlap

;; Note: Be sure that all sprite images are trimmed within their
;; bounding boxes!

; do the bounding boxes of these two sprites overlap?
(define (sprite-bounds-overlap? a b)
  (and (ranges-overlap? (sprite-left a) (sprite-right a) (sprite-left b) (sprite-right b))
       (ranges-overlap? (sprite-top a) (sprite-bottom a) (sprite-top b) (sprite-bottom b)) ) )

;; For testing purposes
(define (show-overlap a b)
  (let ( [x1a (sprite-left a)] [x2a (sprite-right a)] [x1b (sprite-left b)] [x2b (sprite-right b)]
         [y2a (sprite-top a)] [y1a (sprite-bottom a)] [y2b (sprite-top b)] [y1b (sprite-bottom b)] )
    (let ( [x? (ranges-overlap? x1a x2a x1b x2b)]
           [y? (ranges-overlap? y2a y1a y2b y1b)] )
      (begin
        (printf "a x: [~a .. ~a) b x: [~a .. ~a) --> ~a\n"
                x1a x2a x1b x2b x? )
        (printf "a y: [~a .. ~a) b y: [~a .. ~a) --> ~a\n"
                y2a y1a y2b y1b y? )
        (printf "overlap? ~a\n" (and x? y?)) ) ) ) )

;; |---- Range 1 ------|
;;                 |--- Range 2 -----|                          _

;; check that the bounds of one object are in ascending ordered
(define (assert-ascending bound1 bound2 name)
  (unless (< bound1 bound2)
    (error (format "Failed expectation ~a in ascending order: ~a ~a" name bound1 bound2)) ) )

;; Does this handle all the possible cases??
;; Needs the bounds to be in ascending order.
;; Should the test be < or <= and why?
(define (ranges-overlap? start1 end1 start2 end2)
  (assert-ascending start1 end1 'first)
  (assert-ascending start2 end2 'second)
  (< (max start1 start2) (min end1 end2)) )

;; *** Check if the pixels of the sprites overlap

;; Challenging coding project: Write this function properly!
;; The comments here explain what needs to be coded
;; and why it's challenging!

;; It's too expensive to examine all of the pixels of
;; two sprites individually, testing for overlap.

;; First: Make sure that all of the pixels of our images which are not part of
;; the shape of the sprite are alpha-encoded as transparent.

;; Second: When a new image becomes part of a sprite, construct a set of
;; bit-vectors indicating which corresponding pixels of the image are part of
;; the shape of the object. Assume that any non-enclosed transparent part of the
;; image is not part of the shape of the sprite.

;; Third: Cache the set of bit-vectors as part of the sprite. When a new sprite
;; is created from an existing sprite without changing the image, save the
;; bit-vector in the new sprite.

;; Have sprite-images-collide? perform appropriate bitwise intersections
;; between the appropriate bit-vectors of the two overlapping images,
;; shifting the bit-vectors as needed to align.

;; For now, we'll just put it in as a stub, as it will only be used after we
;; check that the bounding boxes overlap, anyway.

(define (sprite-images-collide? a b) #t)

;; *** Putting our methods together

(define (sprites-collide? a b)
  (and (sprite-bounds-overlap? a b)
       (sprite-images-collide? a b) ) )

(define (sprite-collides? sprite world)
  (ormap (λ (s) (and (not (eq? s sprite)) (sprites-collide? sprite s))) world) )

;; ** Placing Sprites on Images

;; Like place-image, but relative to the lower-left corner
;; rather than the x and y centers of the image being placed.
(define (place-image-lower-left top-image x y bottom-image)
  (let ( [center-x (+ x (half (image-width top-image)))]
         [center-y (- y (half (image-height top-image)))] )
    (place-image top-image center-x center-y bottom-image) ) )

(define (simple-place-sprite sprite scene)
  (let ( [image (sprite-image sprite)]
         [x (sprite-left sprite)]
         [y (sprite-bottom sprite)] )
    (place-image-lower-left image x y scene) ) )

;; Wrap the image around the x axis:
;; When the image is partially off-scene,
;; draw the two parts, clipped.
;; THIS IS NOT WORKING YET!!!
(define (wrapping-place-sprite sprite scene)
  (let ( [image (sprite-image sprite)]
         [x (sprite-left sprite)]
         [y (sprite-bottom sprite)] )
    (let ( [first-image (place-image-lower-left image x y scene)]
           [shift-x (- x (image-width scene))] )
      (if (not (> (sprite-right sprite) (image-width scene)))
          first-image
          (place-image-lower-left image shift-x y image) ) ) ) )

;; EXERCISE: Switch place-sprite to use wrapping-place-sprite
;;           and get it to work properly for split images!
;; HINT: Run these expressions at the REPL:
#;(simple-place-sprite
   (update-sprite! PLANE
                   #:x (- SCENE-WIDTH (half (sprite-width PLANE)))
                   #:y BASE-TOP ) BACKGROUND )
#; (wrapping-place-sprite
    (update-sprite! PLANE
                    #:x (- SCENE-WIDTH (half (sprite-width PLANE)))
                    #:y BASE-TOP ) BACKGROUND )

;; Choose the method to use
#;(define place-sprite wrapping-place-sprite)
(define place-sprite simple-place-sprite)

;; ** The BACKGROUND of Fixed Images

;; SCENE and BASE Specifications

(define SCENE-WIDTH 800)
(define SCENE-HEIGHT 500)

;; the base (bottom of scene) consists of water followed by land
(define BASE-HEIGHT 50)
(define BASE-TOP (- SCENE-HEIGHT BASE-HEIGHT))

;; The WATER

(define WATER-WIDTH (truncate (* 5/8 SCENE-WIDTH)))

(define WATER
  (simple-sprite (rectangle WATER-WIDTH BASE-HEIGHT "solid" "blue")
                0 SCENE-HEIGHT ) )

;; for testing - WATER-1 is 1 pixel narrower than WATER so does not touch the LAND
(define WATER-1
  (simple-sprite (rectangle (- WATER-WIDTH 1) BASE-HEIGHT "solid" "blue")
                0 SCENE-HEIGHT ) )

;; for testing - WATER+1 is 1 pixel wider than WATER, overlapping the LAND
(define WATER+1
  (simple-sprite (rectangle (+ WATER-WIDTH 1) BASE-HEIGHT "solid" "blue")
                0 SCENE-HEIGHT ) )

;; The LAND

(define LAND
  (simple-sprite
   (rectangle (- SCENE-WIDTH WATER-WIDTH) BASE-HEIGHT "solid" "brown")
   WATER-WIDTH SCENE-HEIGHT ) )

;; Forming the BACKGROUND

(define BACKGROUND
  (place-sprite WATER
                (place-sprite LAND
                              (empty-scene SCENE-WIDTH SCENE-HEIGHT) ) ) )

;; ** The Moving Foreground

;; *** Computing Position and Velocity Updates

;; sprite -> 6 values
;; x = left edge, y = bottom edge
;; dx = horizontal velocity, dy = vertical velocity
;; w = width, h = height
(define (sprite-x-y-dx-dy-w-h s)
  (values (sprite-left s) (sprite-bottom s)
          (sprite-dx s) (sprite-dy s)
          (sprite-width s) (sprite-height s) ) )

;; x y dx dy -> (+ x dx) (+ y dy)
;; Any of the returned values might wind up out of bounds!
(define (new-xy x y dx dy)
  (values (+ x dx) (+ y dy)) )

;; x y dx dy width height -> new-x new-y new-x2 new-y2
;; like new-xy but with new-x and new-y bounded
;; new-x in [0 .. SCENE-WIDTH) wrapping
;; new-y in [0 .. SCENE-HEIGHT) clipping, not-wrapping
(define (new-xy-wrap-x x y dx dy width height)
  (let-values ( [(maybe-x maybe-y) (new-xy x y dx dy)] )
    (let ( [new-x (modulo maybe-x SCENE-WIDTH)]
           [new-y (min (max maybe-y 0) SCENE-HEIGHT)] )
      (when (not (= maybe-x new-x)) (printf "Wrapped x ~a to ~a!\n" maybe-x new-x))
      (when (not (= maybe-y new-y)) (printf "Clipped y ~a to ~a!\n" maybe-y new-y))
      (values new-x new-y (+ new-x width) (- new-y height)) ) ) )

;; Update a sprite with x and y mutated based on dx and dy
;; wrap x value to stay within the scene
;; bounce if hit top or land or water by reversing velocity
(define (update-sprite-with-bounce! s)
  (let-values ( [(x y dx dy w h) (sprite-x-y-dx-dy-w-h s)] )
    (let-values ( [(new-x new-y new-x2 new-y2) (new-xy-wrap-x x y dx dy w h)] )
      (if (and (>= (+ y dy) 0) (< new-y BASE-TOP))
          (update-sprite! s #:x new-x #:y new-y)
          ;; else bounce
          (let ( [newer-dx (- dx)] [newer-dy (- dy)] )
            (let-values ( [ (newer-x newer-y newer-x2 newer-y2)
                            (new-xy-wrap-x x y newer-dx newer-dy w h) ] )
              (printf "Bouncing from ~a ~a ~a ~a to ~a ~a ~a ~a!\n"
                      new-x new-y dx dy newer-x newer-y newer-dx newer-dy )
              (update-sprite! s #:x newer-x #:y newer-y #:dx newer-dx #:dy newer-dy) ) ) ) ) ) )

;; *** The Plane

;; The plane's horizontal velocity needs to be less than
;; the width of the plane, lest it wrap in a single tick
;; before its new position can be checked by game-over?
(define PLANE-FLYING-DX 5)
(define PLANE-FLYING-DY 5)
(define PLANE-SINKING-DY 1)             ; plane sink rate in water
(define PLANE-TAXIING-DX 1)                ; plane horizontal taxi speed

;; update the plane by mutating its fields appropriately
(define (update-plane! plane)
  (let-values ( [(x y dx dy w h) (sprite-x-y-dx-dy-w-h plane)] )
    (let-values ( [(new-x new-y new-x2 new-y2) (new-xy-wrap-x x y dx dy w h)] )
      (cond [(sprite-bounds-overlap? plane WATER)
             ;; the plane will sink if any part of it overlaps the water
             (update-sprite! plane
                             ;; it will fall back if any part is over the land
                             #:x (min (- WATER-WIDTH w) new-x)
                             #:y new-y
                             ;; it will not be able to move horizontally any more
                             #:dx 0
                             ;; and it will sink slowly
                             #:dy PLANE-SINKING-DY ) ]
            [(sprite-bounds-overlap? plane LAND)
             (update-sprite! plane
                             ;; it will stop at end of land
                             #:x (min (- SCENE-WIDTH w) new-x)
                             ;; it will stay on the land
                             #:y (+ 1 BASE-TOP)
                             ;; it will taxi smoothly to the end of the land
                             #:dx PLANE-TAXIING-DX
                             ;; and it will not sink or rise
                             #:dy 0 ) ]
            [else (update-sprite! plane #:x new-x #:y new-y)] ) ) ) )

(define PLANE-IMAGE  (bitmap "airplane-small-clipped-alpha.png"))
;; Start plane fully visible at upper left
(define PLANE (sprite PLANE-IMAGE
                      0 ; x: plane at left edge of scene
                      (image-height PLANE-IMAGE) ; y: plane at top edge of scene
                      PLANE-FLYING-DX PLANE-FLYING-DY
                      update-plane! ) )

;; Tests

;; Purposes of tests
;; 1. They give examples of how things are supposed to work.
;;    which helps us understand our program better.
;;    They therefore help document the program.
;;    And unlike comments, they can't lie!
;; 2. They check that things are working the way we expect
;;    them to work which helps us avoid bugs.
;;    This is especially useful in catching bugs someone might
;;    accidentally create later when they're evolving the program
;;    to do interesting new things.

;; Ideally our tests would make sure that our program does all
;; of the things we want it to do and that it won't accidentally
;; do anything we don't want it to do.  There are ways to write
;; tests which approach this ideal without having to write a
;; ridiculous amount of tests!

;; EXERCISE: Add more useful tests!

(define PLANE-SUNK (struct-copy simple-sprite PLANE [x 0] [y SCENE-HEIGHT]))
(check-true (sprite-bounds-overlap? PLANE-SUNK WATER))
(check-false (sprite-bounds-overlap? PLANE-SUNK LAND))

(define PLANE-ALMOST-LANDED             ; falls back and sinks
  (struct-copy simple-sprite PLANE
                 [x (- WATER-WIDTH (sprite-center-x PLANE))]
                 [y (+ 1 BASE-TOP)] ) )
(check-true (sprite-bounds-overlap? PLANE-ALMOST-LANDED WATER))
(check-true (sprite-bounds-overlap? PLANE-ALMOST-LANDED LAND))

(define PLANE-BARELY-LANDED             ; whew!
  (struct-copy simple-sprite PLANE-ALMOST-LANDED [x WATER-WIDTH]) )
(check-false (sprite-bounds-overlap? PLANE-BARELY-LANDED WATER))
(check-true (sprite-bounds-overlap? PLANE-BARELY-LANDED LAND))

(define PLANE-AT-LANDS-END ; landed plane at the end of the land
  (struct-copy simple-sprite PLANE-BARELY-LANDED
                 [x (- SCENE-WIDTH (sprite-width PLANE))] ) )
(check-false (sprite-bounds-overlap? PLANE-AT-LANDS-END WATER))
(check-true (sprite-bounds-overlap? PLANE-AT-LANDS-END LAND))

;; *** The Obstacles

(define OBSTACLE-IMAGE-1  (bitmap "balloon-small.png"))
;; Start plane fully visible at upper left
(define OBSTACLE-1
  (sprite OBSTACLE-IMAGE-1
          (- SCENE-WIDTH (image-width OBSTACLE-IMAGE-1)) ; x
          (image-height OBSTACLE-IMAGE-1) ; y
          -1 1 ; dx dy
          update-sprite-with-bounce! ) )

;; ** The World State

;; Our world is a list of sprites

;; The plane must be the first sprite in the world
;; - required by move-plane-on-key! and game-over?
;; All world updates must maintain this position
;; until you've accomplished the suggested exercises!

;; EXERCISE:
;;   Refactor the key responses into a sprite method,
;;   HINT: See how the sprite update method works.
;; EXERCISE:
;;   Refactor the game-over function into a sprite method.

(define INITIAL-WORLD (list PLANE OBSTACLE-1))

;; EXERCISE: Add more interesting sprites to our world!!

;; Updating The World State

;; Update all of the sprites in the world with
;; their proper update procedure.  Returns the world.
(define (update-world! world)
  (for-each (λ (sprite) ((sprite-update sprite) sprite)) world)
  world )

;; ** Rendering (drawing)

;; Compose all of the images of the sprites in the world
;; onto the background and return that composite image.
(define (draw-world world)
  (foldl (λ (s image) (place-sprite s image)) BACKGROUND (reverse world)) )

;; ** Responding To The User

(define KEY-DY 20) ; vertical movement on key press

;; increase or decrease y by dy within the given half interval
;; A half-interval includes the bottom value but not the top value.
;; The math notation is [min-y .. max-y) but this is bad lisp syntax!
(define (bounded-y+dy y dy [max-y BASE-TOP] [min-y 0])
  (let ( [new-y (+ y dy)] )
    (cond [(> new-y max-y) (- max-y 1)]
          [(< new-y min-y) min-y]
          [else new-y] ) ) )

;; Given a world and a meaningful key,
;; return a world with the plane's position updated.
(define (move-plane-on-key! world a-key)
  (let* ( [plane (car world)]
          [y (sprite-bottom plane)]
          [new-y (cond [(key=? a-key "up") (bounded-y+dy y (- KEY-DY))]
                       [(key=? a-key "down") (bounded-y+dy y KEY-DY)]
                       [else y] )] )
    (printf "move-plane-on-key: y ~a -> ~a\n" y new-y)
    (update-sprite! plane #:y new-y)
    world ) )

;; ** Game Over

;; return bool, and if bool is true, also print the comment
;; useful for tracing during debugging
(define (with-comment? bool comment)
  (and bool (begin (printf "~a\n" comment)) bool) )

;; version which prints comments
(define (game-over? world)
  (let ( [plane (car world)] )
    (or (with-comment? (>= (sprite-bottom plane) SCENE-HEIGHT)
          "Your plane is at the bottom of the ocean!" )
        (and (sprite-bounds-overlap? plane LAND)
             (with-comment? (>= (sprite-right plane) (- SCENE-WIDTH 1))
               "Your plane is at the end of the LAND!" ) )
        (with-comment? (sprite-collides? plane world)
          "Your plane hit something!" ) ) ) )

;; ** Play The Game

(define (play)
  ;; set up and run our game
  (big-bang INITIAL-WORLD               ; our initial list of active sprites
            (on-tick update-world! 1/30) ; calling update-world 30 times a second
            (to-draw draw-world         ; updating our scene with draw-world
                     SCENE-WIDTH SCENE-HEIGHT) ; why are these bounds needed??
            (on-key move-plane-on-key!)  ; our keystrokes handler
            (stop-when game-over?) ) )  ; until (game-over? our-world)

(play)
