#lang racket
;; * Flight Lander Game, Version (2htdp Lang Racket)

;; ** Overview

;; This version of flight-lander mutates the sprites
;; rather than generating new sprites each time!

;; *** Exercises

;; Throughout the code there are exercises marked
;; with EXERCISE, questions marked with ?? and
;; issues marked with !!.  These are all for you!

;; Look up any unfamiliar library functions,
;; procedures and macros.
;; In DrRacket you can put the cursor on the
;; name of such and hit F1 to get to its
;; documentation!

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

;; Stylistic comment:
;; Global immutable non-procedures are in UPPER-CASE

;; ** Sprites!

;; *** Type Checking

;; We use Racket's struct/contract to ensure that values
;; given to our fields are of the expected types.
;; This helps us catch bugs!

;; WARNING: Our contracts allow x and y values which exceed
;; the scene's boundaries in order to allow creating
;; values which can go off scene.
;; Is this a good idea??  What might be alternatives??

;; Return our first argument, ignoring the rest.
;; Can be used as a do-nothing update method.
;; How does this work?? Hint: There are no
;; parentheses around arg-list!
(define return-arg1 (λ arg-list (car arg-list)))

;; Let's have sprite
(struct/contract sprite
                 ( [image image?]
                   ;; position: x and y coodinates
                   [x integer?] [y integer?]
                   ;; velocity: delta (change) of x and y
                   [dx integer?] [dy integer?]
                   ;; methods: behavior procedures
                   [on-tick procedure?]
                   [on-key procedure?]
                   [to-draw procedure?] )
                 #:mutable ; fields can be modifed
                 #:transparent ; field values visible
                 )

;; Our x (horizontal) and y (vertical) coordinates
;; are relative to the left and bottom of the
;; boundary of each image or scene.

;; Selector functions and Setter procedures for each field
;; e.g. for field x we'll have
;;   (sprite-x s) -- selects x value of sprite x
;;   (set-sprite-x! s v) -- sets field x of sprite s to value v
;; Predicate function
;;   (sprite? v) -- #t if value v is a sprite structure, #f otherwise
;; Constructor function
;;   (sprite i x y dx dy t k d) -- makes sprite with these field values

;; *** Additional sprite geometry functions

;; We'll often need to split a pixel count in half,
;; but note that it will be off by 1 for odd numbers:
;;   (half 5) --> 2
;; How can we hide this problem??
(define (half x) (quotient x 2))

(define sprite-width (compose image-width sprite-image))
(define sprite-height (compose image-height sprite-image))
;; Are the right and top edges part of the object or bounds??
(define (sprite-x2 s) (+ (sprite-x s) (sprite-width s)))  ; right edge
(define (sprite-y2 s) (+ (sprite-y s) (sprite-height s))) ; top edge

;; ** Placing Sprites on Images

;; Like place-image, but relative to the left-bottom corner
;; of the sprite and the canvas.
(define (draw-image image x y canvas)
  (let ( [center-x (+ x (half (image-width image)))]
         [center-y (+ y (half (image-height image)))] )
    (place-image image center-x (- SCENE-HEIGHT center-y) canvas) ) )

(define (draw-sprite sprite canvas)
  (draw-image (sprite-image sprite)
              (sprite-x sprite) (sprite-y sprite)
              canvas ) )

;; EXERCISE: Create a version of draw-sprite which will
;; draw sprite is in the process of wrapping from one
;; edge of the screen to the opposite edge.  You would
;; need to show it as two partial images.

;; ** Overlap And Collision Detection

;; Consider three methods for detecting whether two sprites collide
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
;; Can you think of any other methods?

;; Method (2) is recommended by the How To Design Worlds tutorial.

;; A combination of (3) and (1) will work better than (2).
;; So let's go with (3) and enhance it with (1) later.

;; *** Check if the sprites bounds overlap

;; Note: Be sure that all sprite images are trimmed within their
;; bounding boxes!

; do the bounding boxes of these two sprites overlap?
(define (sprite-bounds-overlap? a b)
  (and (ranges-overlap? (sprite-x a) (sprite-x2 a) (sprite-x b) (sprite-x2 b))
       (ranges-overlap? (sprite-y a) (sprite-y2 a) (sprite-y b) (sprite-y2 b)) ) )

;; For testing purposes
(define (show-overlap a b)
  (let ( [x1a (sprite-x a)] [x2a (sprite-x2 a)]
         [x1b (sprite-x b)] [x2b (sprite-x2 b)]
         [y1a (sprite-y a)] [y2a (sprite-y2 a)]
         [y1b (sprite-y b)] [y2b (sprite-y2 b)] )
    (let ( [x? (ranges-overlap? x1a x2a x1b x2b)]
           [y? (ranges-overlap? y1a y2a y1b y2b)] )
      (begin
        (printf "a x: [~a .. ~a) b x: [~a .. ~a) --> ~a\n"
                x1a x2a x1b x2b x? )
        (printf "a y: [~a .. ~a) b y: [~a .. ~a) --> ~a\n"
                y1a y2a y1b y2b y? )
        (printf "overlap? ~a\n" (and x? y?)) ) ) ) )

;; |---- Range 1 ------|
;;                 |--- Range 2 -----|                          _

;; check that the bounds of one object are in ascending order
(define (assert-ascending bound1 bound2 name)
  (unless (< bound1 bound2)
    (error "Expected ~a in ascending order: ~a ~a" name bound1 bound2) ) )

;; Does this handle all the possible cases??
;; Needs the bounds to be in ascending order.
;; Should the test be < or <= and why??
(define (ranges-overlap? start1 end1 start2 end2)
  (assert-ascending start1 end1 'first)
  (assert-ascending start2 end2 'second)
  (<= (max start1 start2) (min end1 end2)) )

;; The easy method, especially if images are well-trimmed
(define sprites-collide? sprite-bounds-overlap?)

;; Return a list of all collisions any sprite in the world list has with any
;; sprite further along the world list. Each sublist begins with a sprite
;; followed by all of those later sprites it collides with.
(define (list-collisions world)
  (if (null? world)
      '() ; no collisions in an empty list
      (let* (
             ;; the first sprite in the world list
             [s1 (car world)]
             ;; everything it collides with later in the list
             [collisions (filter (λ (s) (sprites-collide? s1 s))
                                 (cdr world) )]
             ;; any further collision lists later in the list
             [later-collisions (list-collisions (cdr world))] )
        (if (null? collisions)
            later-collisions
            (cons (cons s1 collisions) later-collisions) ) ) ) )

;; ** The BACKGROUND of Fixed Images

;; SCENE and BASE Specifications

(define SCENE-WIDTH 800)
(define SCENE-HEIGHT 500)

;; the base (bottom of scene) consists of water followed by land
(define BASE-HEIGHT 50)

;; The WATER

(define WATER-WIDTH (truncate (* 5/8 SCENE-WIDTH)))

(define WATER
  (sprite (rectangle WATER-WIDTH BASE-HEIGHT "solid" "blue")
          0 0 ; location of the WATER
          0 0 ; no velocity
          return-arg1 return-arg1 ; no updates
          draw-sprite ) )

;; for testing
;; WATER-1 is 1 pixel narrower than WATER so does not touch the LAND
(define WATER-1
  (struct-copy sprite WATER
               [image (rectangle (- WATER-WIDTH 1) BASE-HEIGHT
                                 "solid" "blue" )] ) )

;; for testing
;; WATER+1 is 1 pixel wider than WATER, just barely overlapping the LAND
(define WATER+1
  (struct-copy sprite WATER
               [image (rectangle (+ WATER-WIDTH 1) BASE-HEIGHT
                                 "solid" "blue" )] ) )

;; The LAND

(define LAND
  (struct-copy sprite WATER
               [image (rectangle (- SCENE-WIDTH WATER-WIDTH) BASE-HEIGHT
                                 "solid" "brown" )]
               [x WATER-WIDTH] ) )

;; Forming the BACKGROUND

(define BACKGROUND
  (draw-sprite WATER
                (draw-sprite LAND
                              (empty-scene SCENE-WIDTH SCENE-HEIGHT) ) ) )

;; ** The World of Moving Sprites

;; *** Computing Position and Velocity Updates

;; all horizontal positions are from the left of the canvas
;; all vertical positions are from the bottom of the canvas

;; A moving sprite may be bounded by a "Bounding Box"
;; The "Bounding Box" is either all or part of a canvas

;; We're going to have some geometry functions which all take and/or return
;; 10 geometry values: x y x2 y2 dx dy xx yy xx2 yy2
;;    sprite edges: x = left, y = bottom, x2 = right, y2 = top
;;    sprite velocity: dx dy
;;    canvas edges: xx = left, yy = bottom, xx2 = right, yy2 = top

;; These geometry functions will be composed so that
;; 1. sprite+canvas returns the 10 geometry values
;; 2. various geometry functions transform the values
;; 3. update-sprite! updates the sprite with the new values
;;    ==> how does update-sprite! work??

;; Return the sprite, its geometry and the geometry of its "Bounding Box"
;; The "Bounding Box" is specified by a canvas and optional edge offsets
(define (sprite+canvas s canvas #:x+ [x+ 0] #:y+ [y+ 0] [x- 0] #:y- [y- 0])
  (values (sprite-x s)  ; sprite left edge
          (sprite-y s)  ; sprite bottom edge)
          (sprite-x2 s) ; sprite right edge
          (sprite-y2 s) ; sprite top edge
          (sprite-dx s) ; sprite horizontal velocity component
          (sprite-dy s) ; sprite vertical velocity component
          x+ ; left canvas boundary
          y+  ; bottom canvas boundary
          (- (image-width canvas) x-)  ; right canvas boundary
          (- (image-height canvas) y-) ; top canvas boundary
          ) )

;; transform sprite geometry with sprite velocity, ignoring bounds
(define (preview-sprite x y x2 y2 dx dy xx yy xx2 yy2)
  (values (+ x dx) (+ y dy) (+ x2 dx) (+ y2 dy) dx dy xx yy xx2 yy2) )

;; clip x and x2 so that the sprite is within the horizontal bounds
(define (clip-x x y x2 y2 dx dy xx yy xx2 yy2)
  (let ( [x- (max 0 (- x2 xx2 1))] ; extension of x2 to the right of) xx2
         [x+ (max 0 (- xx x))]     ; extension of x to the left of xx
         )
    (when (and (positive? x-) (positive? x+))
      (error "clip-x2 impossible ~a ~a ~a ~a" x x2 xx xx2) )
    (let ( [x-adjust (- x+ x-)] )
      (values (+ x x-adjust) y (+ x2 x-adjust) y2 dx dy xx yy xx2 yy2) ) ) )

;; what does this do??
(define (flip-xy x y x2 y2 dx dy xx yy xx2 yy2)
  (values y x y2 x2 dy dx yy xx yy2 xx2) )

;; why does this work??
(define clip-y (compose flip-xy clip-x flip-xy))

;; what does this do??
(define clip-xy (compose clip-x clip-y))

;; wrap the sprite horizontally, i.e.
;; - when sprite goes fully beyond the right bound, it appears at the left
;; - when sprite goes fully left of the left bound, it appears at the right
;; EXERCISE: How could we write this better??
(define (wrap-x x y x2 y2 dx dy xx yy xx2 yy2)
  (cond [(>= x xx2)                     ; we've gone over at the right
          (values xx y (+ xx (- x2 x)) y2 dx dy xx yy xx2 yy2) ]
          [(< x2 xx)                    ; we've gone over at -the left
          (values (- xx2 (- x2 x 1)) y (- xx2 1) y2 dx dy xx yy xx2 yy2) ]
          [else (values x y x2 y2 dx dy xx yy xx2 yy2) ] ) )

;; transform the geometry elements by reversing the velocity
;; and updating the x and y coordinates by the new velocity
(define (bounce x y x2 y2 dx dy xx yy xx2 yy2)
  (if (or (< x xx) (>= x2 xx2) (< y yy) (>= y2 yy2)) ;; we're out of bounds!
      ;; so bounce
      (values (- x dx) (- y dy) (- x2 dx) (- y2 dy) (- dx) (- dy) xx yy xx2 yy2)
      ;; otherwise do nothing
      (values x y x2 y2 dx dy xx yy xx2 yy2) ) )

;; returns a procedure which will update the sprite
;; with the geometry values its given and returns
;; the mutated sprite
(define (update-sprite! s)
  (λ (x y x2 y2 dx dy xx yy xx2 yy2)
    (set-sprite-x! s x)
    (set-sprite-y! s y)
    (set-sprite-dx! s dx)
    (set-sprite-dy! s dy)
    s ) )

;; Update a sprite with x and y mutated based on dx and dy
;; wrap x value to stay within the scene
;; bounce if hit top or land or water by reversing velocity
(define (update-sprite-with-bounce! s)
  ( (compose (update-sprite! s) bounce preview-sprite sprite+canvas) ; composite function
    s BACKGROUND #:y+ BASE-HEIGHT ) )

;; *** Create The Plane

;; **** update-plane-on-tick!

;; The plane's horizontal velocity needs to be less than
;; the width of the plane, lest it wrap in a single tick
;; before its new position can be checked by game-over?
(define PLANE-IMAGE  (bitmap "Images/airplane-small-clipped-alpha.png"))
(define PLANE-AIR-DX 5)
(define PLANE-AIR-DY -5)
(define PLANE-SINKING-DY -1)            ; plane sink rate in water
(define PLANE-TAXI-DX 1)                ; plane horizontal taxi speed
(define PLANE-WHEELS-X (half (image-width PLANE-IMAGE))) ; check with image!!

(define (plane-edge-cases x y x2 y2 dx dy xx yy xx2 yy2)
  (let ( [wheels-x (+ x PLANE-WHEELS-X)] )
    (cond [(> y BASE-HEIGHT) ; we're flying, no special cases!
           (values x y x2 y2 dx dy xx yy xx2 yy2) ]
          [(<= y yy) ; we're at the bottom of the ocean
           (values x yy x2 (- y2 y) 0 0 xx yy xx2 yy2) ]
          [ (<= wheels-x WATER-WIDTH) ; we're gonna sink!
            (let ( [x- (max 0 (- x2 WATER-WIDTH))] )
              (values (- x x-) y (- x2 x-) y2 0 PLANE-SINKING-DY xx yy xx2 yy2) ) ]
          [else ;; we'reg going to land!
           (let ( [taxi-dx (if (>= x2 xx2) 0 PLANE-TAXI-DX)]
                  [y-top (+ BASE-HEIGHT (- y2 y))] )
             (values x BASE-HEIGHT x2 y-top taxi-dx 0 xx yy xx2 yy2) ) ] ) ) )

;; updates the plane using mutation and returns it
(define (update-plane-on-tick! plane)
  ( (compose (update-sprite! plane) plane-edge-cases wrap-x preview-sprite sprite+canvas) ; composite function
    plane BACKGROUND ) )

;; **** update-plane-on-key!

(define KEY-DY 20) ; vertical movement on key press

;; updates the plane using mutation and returns it
(define (update-plane-on-key! plane a-key)
  (define (key-match x y x2 y2 dx dy xx yy xx2 yy2)
    (values x (cond [(key=? a-key "up") (+ y KEY-DY)]
                      [(key=? a-key "down") (- y KEY-DY)]
                      [else y])
             x2 y2 dx dy xx yy xx2 yy2) )
  ( (compose (update-sprite! plane) clip-y key-match sprite+canvas) ; composite function
    plane BACKGROUND #:y+ BASE-HEIGHT ) )

;; Start plane fully visible at upper left
(define the-plane
  (sprite PLANE-IMAGE
          0 (- SCENE-HEIGHT (image-height PLANE-IMAGE))
          5 -5 update-plane-on-tick! update-plane-on-key! draw-sprite) )

;; *** Some Tests

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

(define PLANE-SUNK (struct-copy sprite the-plane [x 0] [y 0]))
(check-true (sprite-bounds-overlap? PLANE-SUNK WATER))
(check-false (sprite-bounds-overlap? PLANE-SUNK LAND))

(define PLANE-ALMOST-LANDED             ; falls back and sinks
  (struct-copy sprite the-plane
                 [x (- WATER-WIDTH PLANE-WHEELS-X 1)]
                 [y (- BASE-HEIGHT 1)] ) )
(check-true (sprite-bounds-overlap? PLANE-ALMOST-LANDED WATER))
(check-true (sprite-bounds-overlap? PLANE-ALMOST-LANDED LAND))

(define PLANE-BARELY-LANDED             ; whew!
  (struct-copy sprite PLANE-ALMOST-LANDED [x (- WATER-WIDTH PLANE-WHEELS-X -1)]) )
(check-true (sprite-bounds-overlap? PLANE-BARELY-LANDED WATER))
(check-true (sprite-bounds-overlap? PLANE-BARELY-LANDED LAND))

(define PLANE-AT-LANDS-END ; landed plane at the end of the land
  (struct-copy sprite PLANE-BARELY-LANDED
                 [x (- SCENE-WIDTH (sprite-width the-plane))] ) )
(check-false (sprite-bounds-overlap? PLANE-AT-LANDS-END WATER))
(check-true (sprite-bounds-overlap? PLANE-AT-LANDS-END LAND))

;; *** Create The Obstacles

(define OBSTACLE-IMAGE-1  (bitmap "Images/balloon-small.png"))
;; Start plane fully visible at upper left
(define obstacle-1
  (sprite OBSTACLE-IMAGE-1
          (- SCENE-WIDTH (image-width OBSTACLE-IMAGE-1)) ; x
          BASE-HEIGHT ; y
          -4 1 ; dx dy
          update-sprite-with-bounce! return-arg1 draw-sprite ) )

;; *** The World State

;; Our world is a list of moving sprites.

(define WORLD (list the-plane obstacle-1))

;; EXERCISE: Add more interesting sprites to our world!!

;; Updating The World State

;; update all the sprites in the world
;;   by calling their on-tick methods on themselves
;; return the unaltered world list
(define (update-world-on-tick! world)
  (for-each (λ (sprite) ((sprite-on-tick sprite) sprite)) world)
  world )

;; update all the sprites in the world
;;   by calling their on-tick methods on themselves
;; return the unaltered world list
(define (update-world-on-key! world a-key)
  (for-each (λ (sprite) ((sprite-on-key sprite) sprite a-key)) world)
  world )

;; Compose all of the images of the sprites in the world
;; onto the background and return that composite image.
(define (draw-world world)
  (foldr  (λ (s canvas) ( (sprite-to-draw s) s canvas ) ) BACKGROUND world) )

;; ** Managing The Game

;; returns the bool
;; also displays the comment when bool is true
(define (with-comment? bool comment)
  (and bool (begin (displayln comment)) bool) )

;; version which prints comments
(define (game-over? world)
  (or (with-comment? (<= (sprite-y the-plane) 0)
        "Your plane is at the bottom of the ocean!" )
      (and (sprite-bounds-overlap? the-plane LAND)
           (with-comment? (>= (sprite-x2 the-plane) SCENE-WIDTH)
             "Your plane is at the end of the LAND!" ) )
      (with-comment? (not (null? (list-collisions world)))
        "There's been a collision!" ) ) )

(define (play)
  ;; set up and run our game
  (big-bang WORLD                         ; our initial list of active sprites
    [on-tick update-world-on-tick! 1/30]  ; call procedure 30 times a second
    [to-draw draw-world                   ; update scene with draw-world
             SCENE-WIDTH SCENE-HEIGHT]    ; why are these bounds needed??
    [on-key update-world-on-key!]         ; our keystrokes handler
    [stop-when game-over? draw-world] ) ) ; when (game-over? world)

(play)
