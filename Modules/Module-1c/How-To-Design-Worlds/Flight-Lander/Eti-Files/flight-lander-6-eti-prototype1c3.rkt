#lang racket
;; * Flight Lander Game, Version 6

;; the plane moves left-to-right, wrapping
;; + the plane slowly descends
;; + stopping when it intersects water or land

;; from Chapter 5 of the Tutorial
;;   How to Design Worlds https://world.cs.brown.edu/
;; based on the old 1st edition of
;;   How To Design Programs https://htdp.org/

(require 2htdp/universe) ; for big-bang
(require 2htdp/image)
(require rackunit) ; for checks

;; ** Placing Images

;; Notes
;; - In the 2htdp/image library
;;   - images are placed relative to a "keyhole"
;;   - keyholes default to the centers of their bounding boxes
;;   - scenes are like images but lack keyholes
;;   - the horizontal scene x coordinates increases left-to-right from 0
;;   - the vertical scene y coordinate increases top-to-bottom from 0
;; - We prefer to
;;   - ignore the keyhole concept
;;   - place images relative to the lower left of their bounding boxes
;;   We could simplify our code if we made scene and image coordinates the same
;;   - i.e. relative to their lower left.
;;   - we have not yet chosen to do this, hence a few minus signs are required.

;; half is used to halve sizes in pixels
;; - it gives an inexact result for odd integers
;; - consider making all image sizes even!!
(define (half x) (quotient x 2))

;; place an image onto a scene at a specified scene location
;; location is relative to
;; the lower left of the image
;; the upper left of the scene
;; a wrapper around place-image which is relative to
;; the center of the image
(define (ll-place-image top-image x y bottom-scene)
  (let ( [center-x (+ x (half (image-width top-image)))]
         [center-y (- y (half (image-height top-image)))] )
    (place-image top-image center-x center-y bottom-scene) ) )

;; ** The Background

;; We're following the convention of using
;; UPPER-KEBOB-CASE for constants.

(define WIDTH 800)  ; the width of the scene
(define HEIGHT 500) ; the height of the scene
(define BASE-HEIGHT 50) ; the height of the land and the water
(define BASE-Y (- HEIGHT BASE-HEIGHT))  ; the start of the base, from the top
(define WATER-WIDTH (truncate (* 5/8 WIDTH)))
;; verticle distance to move the plane when you press a key
(define KEY-DISTANCE 15)

;; Using these constants, here’s an image to represent water:
(define WATER (rectangle WATER-WIDTH BASE-HEIGHT "solid" "blue"))
;; and another one to represent land:
(define LAND (rectangle (- WIDTH WATER-WIDTH) BASE-HEIGHT "solid" "brown"))

(define BACKGROUND
  (ll-place-image WATER
               0
               HEIGHT
               (ll-place-image LAND
                            WATER-WIDTH
                            HEIGHT
                            (empty-scene WIDTH HEIGHT) ) ) )

;; ** the plane parameters

(define PLANE (bitmap "image/airplane-small-clipped-alpha.png"))
(define ROBOT-RIGHT (bitmap "image/KevDiscarXRobotSmall.png"))
(define ROBOT-LEFT (bitmap "image/KevPokanRobotSmall.png"))
(define G-MISSILE (bitmap "image/KevinMissileSmall.png"))
(define NUCLEAREXPLOSIONA (bitmap "image/explosionKevin.png"))

(define PLANE-MOVE-X 5) ; plane horizontal velocity component
(define PLANE-MOVE-Y 5) ; plane vertical velocity component
(define PLANE-IN-WATER-MOVE-Y 1)
(define PLANE-ON-LAND-MOVE-X 1)
(define *plane-move-on-land* #t)

;; ** Hot Boxes and Sprites


(define (fraction? x)
  (and (rational? x)
       (>= x 0)
       (<= x 1) ) )

;; a hotbox is a rectangular region within a sprite
(struct/contract
 hotbox (
         ;; let the % suffixes remind us that
         ;; these fields are fractions of the
         ;; size of the sprite
         [x% fraction?]
         [y% fraction?]
         [width% fraction?]
         [height% fraction?]
         ;; because sprite? isn't defined yet
         ;; we delay processing of that contract
         ;; until it's actually used
         [sprite (recursive-contract sprite? #:flat #:extra-delay)] )
 #:transparent )

;; x and y are positions within the scene
;; EXERCISES:
;; Make our contracts more specific:
;; - our method procedures have the right arity
;; - our locations are in a reasonable range
;; - our lists are lists of hotboxes
;; Do I make the explosion happen with the draw procedure or is it separate??
;; allow multiple images for single sprite ANIMATION!

;; A sprite is an animated image containing
;; - an image
;; - a location relative to the lower left of the scene
;; - a velocity to update its location
;; - methods to respond to events
;; - special lists of hotboxes
(struct/contract
 sprite ([image image?]
         [x integer?] [y integer?]
         [dx integer?] [dy integer?]
         [on-tick procedure?] [on-key procedure?] [draw procedure?]
         [killboxes list?] [hurtboxes list?] )
  #:mutable
  #:transparent)

(define sprite-width (compose image-width sprite-image))
(define sprite-height (compose image-height sprite-image))

;; x coordinate of right edge of sprite s
;; relative to left of screen
(define (sprite-x2 s)
  (+ (sprite-x s) (sprite-width s)) )

;; y coordinate of top edge of sprite s
;; relative to top of screen
(define (sprite-y2 s)
  (- (sprite-y s) (sprite-height s)) )

;; ** Functions to Integrate Hotboxes with Sprites

;; a hotbox's size is proportional to the size of its sprite

;; a hotbox has a relative location within a sprite
;; which we can use to compute
;; - its bounds within its sprite
;; - its size
;; - its location within the sceen

;; the x location of the hotbox
;; relative to the sprite's left edge, non-negative
(define (hotbox-x hb)
     (truncate (* (hotbox-x% hb) (sprite-width (hotbox-sprite hb)))) )

;; the y location of the hotbox
;; relative to the sprite's bottom, non-negative
(define (hotbox-y hb)
     (truncate (* (hotbox-y% hb) (sprite-height (hotbox-sprite hb)))) )

;; a hotbox's right edge relative to the sprite's left edge, non-negative,
;; trimmed by the bounds of its sprite
(define (hotbox-x2 hb)
  (let* ( [s-w (sprite-width (hotbox-sprite hb))]
          [hb-w- (truncate (* (hotbox-width% hb) s-w ))] ; untrimmed height
          [hb-x (hotbox-x hb)]
          [hb-x2- (+ hb-x hb-w-)] ; untrimmed right edge
          )
    (min hb-x2- s-w) ) )

;; a hotbox's top edge relative to the sprite's bottom edge, non-negative,
;; trimmed by the bounds of its sprite
(define (hotbox-y2 hb)
  (let* ( [s-h (sprite-height (hotbox-sprite hb))]
          [hb-h- (truncate (* (hotbox-height% hb) s-h))] ; untrimmed height
          [hb-y (hotbox-y hb)]
          [hb-y2- (+ hb-y hb-h-)] ; untrimmed top edge
          )
    (min hb-y2- s-h) ) )

;; a hotbox's width, trimmed by the bounds of its sprite
(define (hotbox-width hb)
  (- (hotbox-x2 hb) (hotbox-x hb)) )

;; a hotbox's height, trimmed by the bounds of its sprite
(define (hotbox-height hb)
  (- (hotbox-y2 hb) (hotbox-y hb)) )

;; the x location of the hotbox relative to the scene's left edge
(define (hotbox-scene-x hb)
  (+ (sprite-x (hotbox-sprite hb)) (hotbox-x hb) ) )

;; the y location of the hotbox relative to the scene's top
(define (hotbox-scene-y hb)
  (- (sprite-y (hotbox-sprite hb)) (hotbox-y hb)) )

(define (hotbox-scene-x2 hb)
  (+ (hotbox-scene-x hb) (hotbox-width hb) ) )

(define (hotbox-scene-y2 hb)
  (- (hotbox-scene-y hb) (hotbox-height hb)) )

;; ** Placing and Showing Sprites and Hotboxes

;; Most sprites use this procedure to place themselves
(define (place-sprite sprite sceen)
  (ll-place-image
   (sprite-image sprite)
   (sprite-x sprite)
   (sprite-y sprite)
   sceen) )

;; tell us about it, then show it
(define (show-hotbox hb [label #f])
  (when label
    (printf "Check location of hotbox ~a:\n" label) )
  (let ( [image (sprite-image (hotbox-sprite hb))] )
    (printf "sprite width: ~a height: ~a\n"
            (image-width image) (image-height image) )
    (printf "hotbox width: ~a --> ~a height: ~a --> ~a\n"
            (hotbox-width% hb) (hotbox-width hb) 
            (hotbox-height% hb) (hotbox-height hb) )
    (printf "hotbox x: ~a --> ~a y: ~a --> ~a\n"
            (hotbox-x% hb) (hotbox-x hb)
            (hotbox-y% hb) (hotbox-y hb) )
    (printf "hotbox x2: ~a y2: ~a\n"
            (hotbox-x2 hb) (hotbox-y2 hb) )
    (overlay/align/offset
     "left" "bottom"
     image (hotbox-x hb) (- (hotbox-y hb))
     (rectangle (hotbox-width hb) (hotbox-height hb) "solid" "red") ) ) )
            
(define (gatling-image sprite)
  (let* ( [image1 (sprite-image sprite)]
          [height (image-height image1)]
          [width (half (image-width image1) )]
          [image2 (overlay/offset image1 width height image1)]
          [max-size (max (image-width image2) (image-height image2))]
          [radius (half max-size )]
          [num-points 40] )
   (overlay image2
            (radial-star radius radius num-points "outline" "red" ) ) ) )

;; Missiles use this fancy procedure to place themselves!
(define (gatling-fire sprite sceen)
  (ll-place-image
;;   (gatling-image sprite) ;; fancy version
   (sprite-image sprite) ;; simple version
   (sprite-x sprite)
   (sprite-y sprite)
   sceen) )

;; ** Procedures to Create and Mutate Sprites

(define (new-sprite image
                    x y
                    #:dx[dx 0] #:dy[dy 0]
                    #:on-key [on-key do-nothing-on-key!]
                    #:on-tick [on-tick sprite-move!]
                    #:draw [draw place-sprite] )
  (sprite image x y dx dy on-tick on-key draw '() '() ) )

(define (update-sprite!
         old-sprite
         ;; maybe some new values for fixed-image fields
         #:image [new-image #f] #:x [new-x #f] #:y [new-y #f]
         ;; maybe some new values for moving-image fields
         #:dx [new-dx #f] #:dy [new-dy #f] #:update [new-update #f]
         #:killboxes [new-killboxes #f]
         #:hurtboxes [new-hurtboxes #f])
  (when new-image (set-sprite-image! old-sprite new-image))
  (when new-x (set-sprite-x! old-sprite new-x))
  (when new-y (set-sprite-y! old-sprite new-y))
  (when new-dx (set-sprite-dx! old-sprite new-dx))
  (when new-dy (set-sprite-dy! old-sprite new-dy))
  (when new-update (set-sprite-on-tick! old-sprite new-update))
  (when new-killboxes (set-sprite-killboxes! old-sprite new-killboxes))
  (when new-hurtboxes (set-sprite-hurtboxes! old-sprite new-hurtboxes))
  old-sprite )
  
;; move the sprite
;;we are making sure that the planes x doesnt leave the scene
;; should we do that for y??
;; what about going off the left edge??
(define (move-sprite-stay-on-screen! the-sprite dx dy)
  (let ([new-x (+ dx (sprite-x the-sprite))]
        [new-y (+ dy (sprite-y the-sprite))] )
    (update-sprite! the-sprite
                    #:x (if (> new-x WIDTH) 0 new-x)
                    #:y new-y ) ) )

(define (sprite-move! sprite)
  (move-sprite-stay-on-screen! sprite(sprite-dx sprite)(sprite-dy sprite)) )

;; EXERCISE: test if the bottom of the landing-gear is in the water or on the land!!
(define (move-plane-on-tick sprite)
  (let ( [x (sprite-x sprite)] [y (sprite-y sprite)] )
    ;; figure out the special conditions
    (cond
      [(in-flight? sprite)  ]
      ;; if we are strattled we are stuck
      [(and(over-water? sprite)
           #;(display "s" (current-error-port))
           (over-land? sprite))
                (update-sprite! sprite
                       ;;should the plane fall in the water??
                       #:x (if (< (sprite-x2 sprite) WATER-WIDTH) ; is the right edge on the land or in the water??
                               (sprite-x sprite)
                               (- (sprite-x sprite) (- WATER-WIDTH (sprite-x2 sprite) ) ) )
                       #:dx 0  #:dy PLANE-IN-WATER-MOVE-Y)
                ]
      ;; if in water you cant move fast
      [(over-water? sprite)
       #;(display "w" (current-error-port))
       (update-sprite! sprite
                       #:dx 0  #:dy PLANE-IN-WATER-MOVE-Y)
       ]
      ;;if on land and we're supposed to move we need to be doing so
      [(over-land? sprite)
       #;(display "l" (current-error-port))
       (update-sprite! sprite #:dx PLANE-ON-LAND-MOVE-X #:dy 0)]
      ;;were lost
      [else (error "have we flown over the bermuda triangl?")] )
    ;; then update the sprite using its own method
    (sprite-move! sprite) ) )

(define (do-nothing-on-key! s a-key) s )

(define (alter-plane-y-on-key! p a-key)
  (cond
    [(or (key=? a-key "q")(key=? a-key "up"))
     (update-sprite! p #:y (- (sprite-y p) KEY-DISTANCE))]  
    [(or (key=? a-key "a")(key=? a-key "down"))
     (update-sprite! p #:y (+ (sprite-y p)  KEY-DISTANCE))]
    [(or (key=? a-key "g")(key=? a-key "\r"))
     (set! *plane-move-on-land* (not *plane-move-on-land*))]  ))

;; ** Build a World of Sprites

;; Each hotbox creation is followed by a show-hotbox form
;;   which will show the location of the hotbox graphically
;;   in the interaction window.
;; Once you feel that a hotbox is in the right place,
;;   you can comment out the show-hotbox form.

;; EXERCISE:
;; - Create a new-hotbox procedure which
;;   1. Allows creating the hotbox more conveniently
;;   2. Attaches the hotbox to a designated list,
;;      e.g. hurtboxes or killboxes

(define MARGIN 40) ; used between robots and scene edges

(define plane (new-sprite PLANE 0 0 #:dx PLANE-MOVE-X #:dy PLANE-MOVE-Y
                          #:on-tick move-plane-on-tick
                          #:on-key alter-plane-y-on-key! ) )
(define cockpit (hotbox 4/5 3/10 2/5 1/2 plane))
(show-hotbox cockpit 'cockpit)
(set-sprite-hurtboxes! plane (cons cockpit (sprite-hurtboxes plane)))
(define landing-gear (hotbox 15/32 1/13 1/8 1/5 plane))
(show-hotbox landing-gear 'landing-gear)

(define (launch-missile-on-key! p a-key)
  (cond
    [(or (key=? a-key "p")(key=? a-key "o")(key=? a-key "i"))
     (fire-gatling-missile)]) )

(define disker-x-robot
  (new-sprite ROBOT-RIGHT
              (- WIDTH (image-width ROBOT-RIGHT) MARGIN)
              400
              #:on-key launch-missile-on-key!) )
(define disker-x-m-launcher (hotbox 2/3 3/4 1/10 1/10 disker-x-robot))
(show-hotbox disker-x-m-launcher 'disker-x-m-launcher)

(define pokkan-robot (new-sprite ROBOT-LEFT MARGIN 400) )



;; EXERCISE:
;; - Rather than having 1 missile appear at a random location
;; - Have missles shot from a robot in response to a key!
;; - When we go multi-player, each robot and the plane will
;;   be controlled by a different player!!

;; Here's our world!
(define sprite-magazine
  (list plane disker-x-robot pokkan-robot ) )

(define (fire-gatling-missile [launcher disker-x-m-launcher])
  (display "fire!\n")
  (define gatling-missile
    (new-sprite G-MISSILE
                (hotbox-scene-x2 launcher)
                (hotbox-scene-y2 launcher)
                ;(random WIDTH) (random HEIGHT)
                #:dx -6 #:dy -1 #:draw gatling-fire ) )
  (define missile-tip (hotbox 0 0 4/10 76/100 gatling-missile))
  (set-sprite-killboxes! gatling-missile (cons missile-tip (sprite-killboxes gatling-missile)))
  #;(show-hotbox missile-tip 'missile-tip)
  (set! sprite-magazine (cons gatling-missile sprite-magazine)) )


;; ** Detecting Sprite Collisions

;; Computes the Euclidian (straight-line) distance between 2 positions,
;; for reference, we don't use it!
#;(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

(define (sprite-overlap s1 s2)
  (and (range-overlap (sprite-x s1) (sprite-x2 s1) (sprite-x s2) (sprite-x2 s2))
       (range-overlap (sprite-y2 s1) (sprite-y s1) (sprite-y2 s2) (sprite-y s2)
)))

(define (hotbox-overlap hb1 hb2)
  (and (not (eq? (hotbox-sprite hb1) (hotbox-sprite hb2) ) )
       (range-overlap (hotbox-scene-x hb1) (hotbox-scene-x2 hb1) (hotbox-scene-x hb2) (hotbox-scene-x2 hb2))
       (range-overlap (hotbox-scene-y2 hb1) (hotbox-scene-y hb1) (hotbox-scene-y2 hb2) (hotbox-scene-y hb2)) )
)

(define (sprite-collide guy-killer hurt-guy)
  (and (sprite-overlap guy-killer hurt-guy)
       (ormap (λ (killbox)
                (ormap (λ (hurtbox)  (hotbox-overlap killbox hurtbox) )
                       (sprite-hurtboxes hurt-guy)  )  )
              (sprite-killboxes guy-killer )  )  ) )

(define (whole-world-collide world-list)
  (ormap (λ (killer)
           (ormap (λ (victim)
                    (sprite-collide killer victim)) world-list) )
           world-list) )

;; |---- Range 1 ------|
;;                   |--- Range 2 -----|

;; Does this handle all the possible cases??
(define (range-overlap start1 end1 start2 end2)
     (<= (max start1 start2) (min end1 end2)) );compute the size of the objects

;; ** the world state

;; Our world consists of a list of sprites.  The state
;; of our world consists of what sprites are on that list
;; and the state of the changeable parts of those sprites.
;; The most common parts of our sprites which change are
;; their x and y locations in the Scene.  But other parts
;; can change too!

;; *** High-Level State Predicates

;; Predicate functions to query high-level states of a Sprite,
;; i.e. states which are special in our game.

(define (in-flight? sprite)
  (< (sprite-y sprite) BASE-Y))

;;the plane is over the water at least part
(define (over-water? sprite)
  (< (sprite-x sprite) WATER-WIDTH))

(define (on-bottom? sprite)
  (>= (sprite-y sprite) HEIGHT))

;; at least part of the plane is over the land
(define (over-land? sprite)
  (>= (sprite-x2 sprite) WATER-WIDTH))

;; *** Testing our High-Level State Predicates

;; make a test-plane at a specific location
(define (test-plane x y)
  (new-sprite PLANE x y) )

;; test that should fail
#;(check-pred over-water? (test-plane WATER-WIDTH 0))

;; check if predicates succeed when they should
(check-pred over-water? (test-plane 0 0))
(check-pred over-land? (test-plane WATER-WIDTH 0))
(check-pred in-flight? (test-plane 0 0))
(check-pred on-bottom? (test-plane 0 HEIGHT))

;; check if predicates fail when they should
(check-pred (compose not over-land?) (test-plane 0 0))
(check-pred (compose not over-water?) (test-plane WATER-WIDTH 0))
(check-pred (compose not in-flight?) (test-plane 0 BASE-Y))
(check-pred (compose not on-bottom?) (test-plane 0 0))
(check-pred (compose not on-bottom?) (test-plane 0 BASE-Y))

;; example test !!
#;(check-expect (move-plane-on-tick (make-posn 0 0)) (make-posn PLANE-MOVE-X PLANE-MOVE-Y))

;; ** Running The Game

;; These procedures are called by the big-bang procedure
;; which is called at the bottom to start the game.

;; given
;; - a scene with some graphical objects already in it
;; - a list of more graphical objects to include
;; - each with their own draw method
;; returns
;; - a new scene, with the remaining graphical objects
;;   composed into it using their own draw method
(define (compoze sceen sprites)
  (if (null? sprites) ; if no objects remain
      sceen  ; return completed scene
      (let ( [first-sprite (car sprites)]
             [other-sprites (cdr sprites)] )
      ;; compose the first object with the rest 
      ( (sprite-draw first-sprite) ; the sprite's own method
        first-sprite ; composes it with the rest recursively
        (compoze sceen other-sprites) ) ) ) )

(define (draw-world world) (compoze BACKGROUND world))

;; make each sprite update itself!
(define (update-world-on-tick! world)
  ;; call the lambda procedure on each sprite in world
  (for-each (λ (s) (if (sprite-on-tick s)
                       ( (sprite-on-tick s) s )
                       s ))
            world )
  ;; return the alteraed world
  world )

(define (update-world-on-key! world a-key)
  ;; call the lambda procedure on each sprite in world
  (for-each (λ (s) (if (sprite-on-key s)
                       ( (sprite-on-key s) s a-key )
                       s ))
            world )
  ;; return the altered world
  sprite-magazine )

;; Currently game-over ignores the world
;; - it just tests the global variables
;; - plane and gatling-missile!!
;; EXERCISE:
;; Change game-over to test
;; - whether any of the killboxes in our world
;; - overlap with any of the hurtboxes in our world!
;; The killboxes and hurtboxes can be found by
;; iterating over the world parameter.  No global
;; variables will be needed anymore!
(define (game-over? world)
  (or (on-bottom?  plane)
      (whole-world-collide sprite-magazine)
      (and (not (in-flight? plane))
           (>= (sprite-x2 plane) WIDTH) ) ) )

(define (draw-last-world world)
  (when (whole-world-collide sprite-magazine)
    (set-sprite-image! plane NUCLEAREXPLOSIONA) )
  (draw-world world) )

;; run a game using
;; - our world-state (list of sprites)
;; - the desired screen size
;; - our drawing, update and game-over procedures
;; - called 30 times per second (clock ticks)
(big-bang sprite-magazine
  [to-draw draw-world WIDTH HEIGHT]
  [on-tick update-world-on-tick! 1/30]
  [on-key update-world-on-key!]
  [stop-when game-over? draw-last-world] )
