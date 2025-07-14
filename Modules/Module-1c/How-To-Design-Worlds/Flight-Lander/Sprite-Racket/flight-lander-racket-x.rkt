#lang racket
;; * Flight Lander Game, Version (x-perimental 2htdp Lang Racket)
;; ** Overview

;; the plane moves left-to-right, wrapping
;; - the plane slowly descends
;; - up/down keys give extra vertical motion
;; - game ends when land or sink to bottom
;; + sinking is slow
;; + almost landed planes fall back into the water

;; inspired by Chapter 5 of the Tutorial
;;   How to Design Worlds https://world.cs.brown.edu/
;; brought up to date with the 2nd edition of
;;   How To Design Programs https://htdp.org/
;; using Lang Racket

(require 2htdp/universe) ; for big-bang
(require 2htdp/image)
(require rackunit) ; for checks
#;(require file/convertible) ; for image file conversions
#;(require pict/convert) ; for conversions between images

;; Stylistic comment:
;; Global immutable non-procedures are in UPPER-CASE


;; ** The Data Structures
;; *** Type Checking

;; Make an expectation function which takes a value
;; which is supposed to conform to the expectation function
;; along with a symbol which names the expectation.
;; If the value conforms, the expectation function returns the value unchanged.
;; If it doesn't, the function raises an error with the name and the value. 
(define (make-expect expectation? name)
  (λ (val [context #f])
    (if (expectation? val)
        val
        (error (format "Failed expectation ~a~a: ~a"
               name (if context (format " ~a" context) "") val )) ) ) )

;; Create a function which expects an image
(define the-image (make-expect image? 'image) )

;; Create a function which expects a signed integ
(define the-integer (make-expect integer? 'integer) )

;; Create a function which expects a non-negative integer,
;; i.e. a "natural number".
(define the-natural
  (make-expect (λ (x) (and (integer? x) (>= x 0))) 'non-negative-integer) )

;; *** Cacheing Shape Maps

;; Bitmap images are essentially a 2D array of pixels
;; typically with at least 4 bytes or 32 bits per pixel.
;; Each pixel encodes a value for red, blue, green and alpha.
;; The alpha value represents transparency.
;; see (image->color-list image) for how to get the pixel values out

;; We will assume that the fully transparent pixels on the
;; outside of a bitmap are not part of the shape the bitmap depicts.

;; A bit vector is a vector of single bits.  We can use a bit vector
;; to encode the shape implied by a bitmap.

;; Encoding the bits of an image which are not transparent will occupy at most
;; 1/32 of the storage space of the bitmap image. More importantly, computing
;; intersections of bit vectors can be done very fast - if the vectors are
;; properly aligned to represent the positions of the overlapping bitmaps.
;; see https://docs.racket-lang.org/bv for how to build and use the bit vectors
;; also see https://docs.racket-lang.org/rosette-guide/sec_bitvectors.html

;; Functions which might be useful
;; - (bit i x) → (bitvector 1)

;; Alternatively, we could just use a vector of integers and apply bitwise operations
;; on them, e.g.
;; - (bitwise-xor n ...)
;; see https://docs.racket-lang.org/reference/generic-numbers.html#%28part._.Bitwise_.Operations%29

;; *** fixed-image - Image Object Structure

;; A sprite is either a fixed-image or a moving-image.
;; The sprite functions will work with either.

;; A fixed-image represents an image at a specific position in a 2D scene.
;; A moving-image adds dx and dy values for movement, along with an update
;; method for creating a new moving-image, e.g. on a clock tick.

;; Our x and y values designate the lower left corner
;; of the object's bounding box.

;; WARNING: The image drawing toolkit uses the
;; coordinates of the center of the image's bounding
;; box to draw it!

;; WARNING: Our guards allow x and y values which exceed
;; the scene's width and height in order to allow creating
;; values which signal that an object has gone off scene.
;; Is this a good idea??  What might be alternatives??

;; The make-fixed-image function may be easier to use than the
;; automatically created fixed-image constructor which requires
;; values for all of the fields!

(struct	fixed-image (image x y)
  #:transparent
  #:guard  (λ (image x y name) ; check that our field values are valid
             (values (the-image image '(fixed-imagel image))
                     (the-natural x '(fixed-image x))
                     (the-natural y '(fixed-image y)) ) ) )

;; Create synonyms for the field accessor methods
(define sprite-image fixed-image-image)
(define sprite-x fixed-image-x)
(define sprite-y fixed-image-y)

;; *** moving-image - Image Object Structure

;; Currently just checks that the argument is a procedure.
;; Ideally would check it's of type (-> moving-image moving-image)
;; i.e. a procedure which takes a moving-image and returns a moving-image
(define update-function? procedure?)
;; Create a function which expects an update-function
(define the-update-function (make-expect update-function? 'update-function) )
;; Create an update-function which returns its object unchanged
(define (update-identity x) x)

(struct	moving-image fixed-image (dx dy update)
  #:transparent
  #:guard  (λ (image x y dx dy update name)
             (values (the-image image '(moving-image image))
                     (the-natural x '(moving-image x))
                     (the-natural y '(moving-image y))
                     (the-integer dx '(moving-image dx))
                     (the-integer dy '(moving-image dy))
                     (the-update-function update '(moving-image update)) ) ) )
;; Create synonyms for the field accessor methods
(define sprite-dx moving-image-dx)
(define sprite-dy moving-image-dy)
(define sprite-update moving-image-update)

(define (sprite? obj) (or (fixed-image? obj) (moving-image? obj)))
;; Create a function which expects a sprite
(define the-sprite (make-expect sprite? 'sprite) )

;; *** The fabulous make-sprite constructor function

;; Return a sprite with the indicated field values
;; return existing sprite if given and no changes are needed
;; return a moving-image if given existing moving-image or moving-image field values
;; otherwise, return a fixed-image
(define (make-sprite
         ;; maybe an old sprite providing default values
         #:from [old-sprite #f]
         ;; maybe some new values for fixed-image fields
         #:image [new-image #f] #:x [new-x #f] #:y [new-y #f]
         ;; maybe some new values for moving-image fields
         #:dx [new-dx #f] #:dy [new-dy #f] #:update [new-update #f] )
  (let (
        [old-image (and old-sprite (sprite-image old-sprite))]
        [old-x (and old-sprite (sprite-x old-sprite))]
        [old-y (and old-sprite (sprite-y old-sprite))]
        [old-dx (and (moving-image? old-sprite) (sprite-dx old-sprite))]
        [old-dy (and (moving-image? old-sprite) (sprite-dy old-sprite))]
        [old-update (and (moving-image? old-sprite) (sprite-update old-sprite))] )
    (let (
          [image (the-image (or new-image old-image) 'make-sprite)]
          [x (or new-x old-x 0)]
          [y (or new-y old-y 0)]
          [dx (or new-dx old-dx 0)]
          [dy (or new-dy old-dy 0)]
          [update (or new-update old-update update-identity)] )
      (let (
            ;; will any fields change?
            [same (and (eq? image old-image)
                       (eq? x old-x)
                       (eq? y old-y)
                       (eq? dx old-dx)
                       (eq? dy old-dy)
                       (eq? update old-update) )]
            ;; do we have any moving-image field values?
            [no-moving (not (or new-dx new-dy new-update))] )
        (cond [(and same old-sprite) old-sprite]
              [(and no-moving (not (moving-image? old-sprite))) (fixed-image image x y)]
              [else (moving-image image x y dx dy update)] ) ) ) ) )

;; *** Additional fixed-image property functions

(define (sprite-width s)  (image-width (sprite-image s)))
(define (sprite-height s) (image-height (sprite-image s)))
(define (sprite-half-width s) (quotient (sprite-width s) 2))
(define (sprite-half-height s) (quotient (sprite-height s) 2))
(define (sprite-center-x s) (+ (sprite-x s) (sprite-half-width s)))
(define (sprite-center-y s) (- (sprite-y s) (sprite-half-height s)))
;; Are the right and top edges part of the object or bounds?
(define (sprite-x2 s) (+ (sprite-x s) (sprite-width s))) ; right
(define (sprite-y2 s) (- (sprite-y s) (sprite-height s))) ; top

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
  (and (ranges-overlap? (sprite-x a) (sprite-x2 a) (sprite-x b) (sprite-x2 b))
       (ranges-overlap? (sprite-y2 a) (sprite-y a) (sprite-y2 b) (sprite-y b)) ) )

;; For testing purposes
(define (show-overlap a b)
  (let ( [x1a (sprite-x a)] [x2a (sprite-x2 a)] [x1b (sprite-x b)] [x2b (sprite-x2 b)]
         [y2a (sprite-y2 a)] [y1a (sprite-y a)] [y2b (sprite-y2 b)] [y1b (sprite-y b)] )
    (let ( [x? (ranges-overlap? x1a x2a x1b x2b)]
           [y? (ranges-overlap? y2a y1a y2b y1b)] )
      (begin
        (printf "a x: [~a .. ~a) b x: [~a .. ~a) --> ~a\n"
                x1a x2a x1b x2b x? )
        (printf "a y: [~a .. ~a) b y: [~a .. ~a) --> ~a\n"
                y2a y1a y2b y1b y? )
        (printf "overlap? ~a\n" (and x? y?)) ) ) ) )

;; |---- Range 1 ------|
;;                       |--- Range 2 -----|                          _

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

;; Like place-image, but relative to the southwest (lower-left)
;; corner rather than the center of the image being placed.
(define (ll-place-image top x y bottom)
  (let ( [center-x (+ x (quotient (image-width top) 2))]
         [center-y (- y (quotient (image-height top) 2))] )
    (place-image top center-x center-y bottom) ) )

(define (simple-place-sprite sprite scene)
  (let ( [image (sprite-image sprite)]
         [x (sprite-x sprite)]
         [y (sprite-y sprite)] )
    (ll-place-image image x y scene) ) )

;; Wrap the image around the x axis:
;; When the image is partially off-scene,
;; draw the two parts, clipped.
;; THIS IS NOT WORKING YET!!!
(define (wraping-place-sprite sprite scene)
  (let ( [image (sprite-image sprite)]
         [x (sprite-x sprite)]
         [y (sprite-y sprite)] )
    (let ( [first-image (ll-place-image image x y scene)]
           [shift-x (- x (image-width scene))] )
      (if (not (> (sprite-x2 sprite) (image-width scene)))
          first-image
          (ll-place-image image shift-x y image) ) ) ) )

;; Choose the method to use
(define place-sprite simple-place-sprite)

;; ** The BACKGROUND of Fixed Images

;; SCENE and BASE Specifications

(define SCENE-WIDTH 800)
(define SCENE-HEIGHT 500)

;; the base (bottom of scene) consists of water followed by land
(define BASE-HEIGHT 50)
(define BASE-Y (- SCENE-HEIGHT BASE-HEIGHT))

;; The WATER

(define WATER-WIDTH (truncate (* 5/8 SCENE-WIDTH)))

(define WATER
  (make-sprite #:image (rectangle WATER-WIDTH BASE-HEIGHT "solid" "blue")
               #:x 0 #:y SCENE-HEIGHT ) )

;; for testing - WATER-1 is 1 pixel narrower than WATER so does not touch the LAND
(define WATER-1
  (make-sprite #:image (rectangle (- WATER-WIDTH 1) BASE-HEIGHT "solid" "blue")
               #:x 0 #:y SCENE-HEIGHT ) )

;; for testing - WATER+1 is 1 pixel wider than WATER, overlapping the LAND
(define WATER+1
  (make-sprite #:image (rectangle (+ WATER-WIDTH 1) BASE-HEIGHT "solid" "blue")
               #:x 0 #:y SCENE-HEIGHT ) )

;; The LAND

(define LAND
  (make-sprite
   #:image (rectangle (- SCENE-WIDTH WATER-WIDTH) BASE-HEIGHT "solid" "brown")
   #:x WATER-WIDTH #:y SCENE-HEIGHT ) )

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
  (values (sprite-x s) (sprite-y s)
          (sprite-dx s) (sprite-dy s)
          (sprite-width s) (sprite-height s) ) )

;; x y dx dy -> (+ x dx) (+ y dy)
;; Any of the returned values might wind up out of bounds!
(define (new-xy x y dx dy)
  (values (+ (the-natural x '(new-xy x)) dx)
          (+ (the-natural y '(new-xy y)) dy) ) )

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

;; Create a new moving-image with x and y updated based on dx and dy
;; wrap x value to stay within the scene
;; bounce if hit top or land or water by reversing velocity
(define (update-sprite-with-bounce s)
  (let-values ( [(x y dx dy w h) (sprite-x-y-dx-dy-w-h s)] )
    (let-values ( [(new-x new-y new-x2 new-y2) (new-xy-wrap-x x y dx dy w h)] )
      (if (and (>= (+ y dy) 0) (< new-y BASE-Y))
          (make-sprite #:from s
                       #:x (the-natural new-x '(update-sprite-with-bounce new-x))
                       #:y (the-natural new-y '(update-sprite-with-bounce new-y)) )
          ;; else bounce
          (let ( [newer-dx (- dx)] [newer-dy (- dy)] )
            (let-values ( [(newer-x newer-y newer-x2 newer-y2) (new-xy-wrap-x x y newer-dx newer-dy w h)] )
              (printf "Bouncing from ~a ~a ~a ~a to ~a ~a ~a ~a!\n"
                      new-x new-y dx dy newer-x newer-y newer-dx newer-dy )
              (make-sprite #:from s
                           #:x (the-natural newer-x '(update-sprite-with-bounce new-x 2))
                           #:y (the-natural newer-y '(update-sprite-with-bounce new-y 2))
                           #:dx newer-dx #:dy newer-dy ) ) ) ) ) ) )

;; *** The Plane

;; The plane's horizontal velocity needs to be less than
;; the width of the plane, lest it wrap in a single tick
;; before its new position can be checked by game-over?
(define PLANE-AIR-DX 5)
(define PLANE-AIR-DY 5)
(define PLANE-SINKING-DY 1)             ; plane sink rate in water
(define PLANE-TAXI-DX 1)                ; plane horizontal taxi speed

;; update-plane parameters
(define (update-plane plane)
  (let-values ( [(x y dx dy w h) (sprite-x-y-dx-dy-w-h plane)] )
    (let-values ( [(new-x new-y new-x2 new-y2) (new-xy-wrap-x x y dx dy w h)] )
      (cond [(sprite-bounds-overlap? plane WATER)
             ;; the plane will sink if any part of it overlaps the water
             (make-sprite #:from plane
                         ;; it will fall back if any part is over the land
                         #:x (the-natural (min (- WATER-WIDTH w) new-x) '(update-plane x))
                         #:y (the-natural new-y '(update-plane new-y))
                         ;; it will not be able to move horizontally any more
                         #:dx 0
                         ;; and it will sink slowly
                         #:dy PLANE-SINKING-DY ) ]
            [(sprite-bounds-overlap? plane LAND)
             (make-sprite #:from plane
                         ;; it will stop at end of land
                          #:x (the-natural (min (- SCENE-WIDTH w) new-x) '(update-plane new-x))
                         ;; it will stay on the land
                         #:y (+ 1 BASE-Y)
                         ;; it will taxi smoothly to the end of the land
                         #:dx PLANE-TAXI-DX
                         ;; and it will not sink or rise
                         #:dy 0 ) ]
            [else (make-sprite #:from plane
                               #:x (the-natural new-x '(update-plane else new-x))
                               #:y (the-natural new-y '(update-plane else new-y)) )] ) ) ) )

(define PLANE-IMAGE  (bitmap "../Images/airplane-small-clipped-alpha.png"))
;; Start plane fully visible at upper left
(define PLANE (make-sprite #:image PLANE-IMAGE
                           #:y (image-height PLANE-IMAGE)
                           #:dx 5 #:dy 5
                           #:update update-plane ) )


;; for tests:
(define PLANE-SUNK (make-sprite #:from PLANE #:x 0 #:y SCENE-HEIGHT))
(define PLANE-ALMOST-LANDED             ; falls back and sinks
  (make-sprite #:from PLANE
               #:x (- WATER-WIDTH (sprite-center-x PLANE))
               #:y (+ 1 BASE-Y) ) )
(define PLANE-BARELY-LANDED             ; whew!
  (make-sprite #:from PLANE-ALMOST-LANDED #:x WATER-WIDTH) )
(define PLANE-AT-LANDS-END ; landed plane at the end of the land
  (make-sprite #:from PLANE-BARELY-LANDED
               #:x (- SCENE-WIDTH (sprite-width PLANE)) ) )

;; *** The Obstacles

(define OBSTACLE-IMAGE-1  (bitmap "../Images/balloon-small.png"))
;; Start plane fully visible at upper left
(define OBSTACLE-1
  (make-sprite #:image OBSTACLE-IMAGE-1
               #:x (- SCENE-WIDTH (image-width OBSTACLE-IMAGE-1))
               #:y (image-height OBSTACLE-IMAGE-1)
               #:dx -1 #:dy 1
               #:update update-sprite-with-bounce ) )

;; ** The World State

;; Our world is a list of the objects which might change

;; The plane must be the first sprite in the world
;; - required by move-plane-on-key and game-over?
;; All world updates will maintain this position.
(define INITIAL-WORLD (list PLANE OBSTACLE-1))

;; Updating The World State

;; Create a new world
;; i.e. a new list of sprites from the old list of sprites
;; each updated by its own update function.
(define (update-world world)
  (map (λ (sprite) ((sprite-update sprite) sprite)) world) )

;; ** Rendering (drawing)

;; Compose all of the images of the sprites in the world
;; onto the background and return that composite image.
(define (draw-world world)
  (foldl (λ (s image) (place-sprite s image)) BACKGROUND (reverse world)) )

;; ** Responding To The User

(define KEY-DY 20)

;; increase or decrease y by dy within the given half interval
(define (bounded-y+dy y dy [max-y BASE-Y] [min-y 0])
  (let ( [new-y (+ y dy)] )
    (cond [(> new-y max-y) (- max-y 1)]
          [(< new-y min-y) min-y]
          [else new-y] ) ) )

;; Given a world and a meaningful key,
;; return a world with the plane's position updated.
(define (move-plane-on-key world a-key)
  (let* ( [plane (car world)]
          [y (sprite-y plane)]
          [new-y (cond [(key=? a-key "up") (bounded-y+dy y (- KEY-DY))]
                       [(key=? a-key "down") (bounded-y+dy y KEY-DY)]
                       [else y] )] )
    (if (= y new-y)
        world                           ; the world stays the same
        (begin
          (printf "move-plane-on-key: y ~a -> ~a\n" y new-y)
          (cons (make-sprite #:from plane #:y new-y) (cdr world)) ) ) ) )

;; ** Game Over

;; return bool, and if bool is true, also print the comment
;; useful for tracing during debugging
(define (with-comment? bool comment)
  (and bool (begin (printf "~a\n" comment)) bool) )

;; version which prints comments
(define (game-over? world)
  (let ( [plane (car world)] )
    (or (with-comment? (>= (sprite-y plane) SCENE-HEIGHT)
          "Your plane is at the bottom of the ocean!" )
        (and (with-comment? (sprite-bounds-overlap? plane LAND)
               "Your plane is on land!" )
             (with-comment? (>= (sprite-x2 plane) (- SCENE-WIDTH 1))
               "Your plane is at the end of the LAND!" ) )
        (with-comment? (sprite-collides? plane world)
          "Your plane hit something!" ) ) ) )

;; ** Play The Game

(define (play)
  ;; set up and run our game
  (big-bang INITIAL-WORLD               ; our initial list of active sprites
            (on-tick update-world 1/30) ; calling update-world 30 times a second
            (to-draw draw-world         ; updating our scene with draw-world
                     SCENE-WIDTH SCENE-HEIGHT) ; why are these bounds needed??
            (on-key move-plane-on-key)  ; our keystrokes handler
            (stop-when game-over?) ) )  ; until (game-over? our-world)

(play)
