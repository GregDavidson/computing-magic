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

;; * the background
;; Like place-image, but relative to the southwest (lower-left)
;; corner rather than the center of the image being placed.
(define (ll-place-image top x y bottom)
  (let ( [center-x (+ x (quotient (image-width top) 2))]
         [center-y (- y (quotient (image-height top) 2))] )
    (place-image top center-x center-y bottom) ) )

(define WIDTH 800)
(define HEIGHT 500)
(define BASE-HEIGHT 50)
(define BASE-Y (- HEIGHT BASE-HEIGHT))
(define WATER-WIDTH (* 5/8 WIDTH))
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

;; * the plane


(define PLANE (bitmap "image/airplane-small-clipped-alpha.png"))
(define BALLOONA (bitmap "image/KevDiscarXRobotSmall.png"))
(define BALLOONB (bitmap "image/KevPokanRobotSmall.png"))
(define BALLOONC (bitmap "image/KevinMissileSmall.png"))
(define NUCLEAREXPLOSIONA (bitmap "image/explosionKevin.png"))

(define PLANE-MOVE-X 5) ; plane horizontal velocity component
(define PLANE-MOVE-Y 5) ; plane vertical velocity component
(define PLANE-IN-WATER-MOVE-Y 1)
(define PLANE-ON-LAND-MOVE-X 1)
(define *plane-move-on-land* #t)


;; a world of objects


;; x and y are proportional offsets within the sprite
;; contract needed on spritefield!!
(struct/contract hotbox(
                        [x rational?] [y rational?]
                        [height rational?] [width rational?]
                        [sprite (λ (thing) #t)])
                 #:transparent) ;for debugging

;; x and y are positions within the scene
;; add on-key in the future!!
;; can our contract check on-tick takes the correct arguments!!
;; can we check that the boxes are lists of hotboxes!!
;; how can we keep x and y non negative??


;; do i make the explosion happen with the draw procedure or is it seperate?
(struct/contract sprite ([image image?] [x integer?] [y integer?]
                                        [dx integer?] [dy integer?]
                                        [on-tick procedure?][on-key procedure?][draw procedure?]
                                        [killboxes list?] [hurtboxes list?])
  #:mutable
  #:transparent)
;;??
(define (hotbox-leftside hb)
  (+ (sprite-x (hotbox-sprite hb))
     (* (hotbox-x hb) (sprite-width (hotbox-sprite hb)))) )
;;??
(define (hotbox-rightside hb)
  (let ( [ s (hotbox-sprite hb) ] )
    (- (sprite-x s)
       (* (hotbox-x hb) (sprite-width s) ) )) )
;;??
(define (hotbox-top hb)
  (let ( [ s (hotbox-sprite hb) ] )
    (- (sprite-y s)
       (* (hotbox-y hb) (sprite-height s) ) )) )
;;??
(define (hotbox-bottom hb)
  (let ( [ s (hotbox-sprite hb) ] )
    (+ (sprite-y s)
       (* (hotbox-y hb) (sprite-height s) ) )) )

(define (place-sprite sprite sceen)
  (ll-place-image
   (sprite-image sprite)
   (sprite-x sprite)
   (sprite-y sprite)
   sceen) )

(define (gatling-fire sprite sceen)
  (let* ( [image (sprite-image sprite)]
          [max-size (max (image-width image) (image-height image))]
          [radius (/ max-size 2)]
          [num-points 40] )
  (ll-place-image
   (overlay image (radial-star radius radius num-points "outline" "red" ))
   (sprite-x sprite)
   (sprite-y sprite)
   sceen) ) )

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
  

;;sprite constructor
#;(define (sprite image x y dx dy on-tick)
  (list x y dx dy image on-tick))

;;sprite destructors

#;(define sprite-x first) ;car
#;(define sprite-y second) ;cadr

#;(define sprite-dx third)
#;(define sprite-dy fourth)

#;(define sprite-image fifth)

#;(define sprite-on-tick sixth)
;; sprite convienience functions

;; one way to compose
(define (sprite-width sprite)
  (image-width (sprite-image sprite)))

;; another way to compose
(define sprite-height (compose image-height sprite-image))

;; move the sprite
;;we are making sure that the planes x doesnt leave the scene
;; should we do that for y??
;; what about going off the left edge??
(define (move-sprite-stay-on-screen! the-sprite dx dy)
  (let ([new-x(+ dx (sprite-x the-sprite))]
        [new-y(+ dy (sprite-y the-sprite))] )
    (update-sprite! the-sprite
                    #:x (if (> new-x WIDTH) 0 new-x)
                    #:y new-y ) ) )

(define (sprite-move! sprite)
  (move-sprite-stay-on-screen! sprite(sprite-dx sprite)(sprite-dy sprite)) )

;; for updating the world
;; this can be improved!!
;; use (hotbox-bottom landing-gear) instead!!
(define (move-plane-on-tick sprite)
  (let ( [x (sprite-x sprite)] [y (sprite-y sprite)] )
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
      [else (error "have we flown over the bermuda triangle?") ] )
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

;;world sprites
(define margin 40)

(define (new-sprite image
                    x y
                    #:dx[dx 0] #:dy[dy 0]
                    #:on-key[on-key do-nothing-on-key!]#:on-tick[on-tick sprite-move!]#:draw[draw place-sprite] )
  (sprite image x y dx dy on-tick on-key draw '() '() ) )

(define plane (new-sprite PLANE 0 0 #:dx PLANE-MOVE-X #:dy PLANE-MOVE-Y #:on-tick move-plane-on-tick #:on-key alter-plane-y-on-key!))
(define landing-gear (hotbox 4/10 9/10 95/100 1/10 plane))
(define cockpit (hotbox 9/10 1/2 1/5 4/10 plane))
(set-sprite-hurtboxes! plane (cons landing-gear (sprite-hurtboxes plane)))
(set-sprite-hurtboxes! plane (cons cockpit (sprite-hurtboxes plane)))
(define disker-x-robot (new-sprite BALLOONA
                     (- WIDTH (image-width BALLOONA) margin) 400) ) 
(define pokkan-robot (new-sprite BALLOONB margin 400) )
(define gatling-missile
  (new-sprite BALLOONC
              (random WIDTH) (random HEIGHT)
              #:dx -2 #:dy -5
              #:draw gatling-fire ) )

(define sprite-magazine
  (list plane disker-x-robot pokkan-robot gatling-missile ) )


(define wheels(hotbox (/ (image-width PLANE) 2) 0 12 12 (first sprite-magazine)))
(define missile-tip(hotbox 0 11 11 11 (fourth sprite-magazine)))

;;collission detetection

;;return the distance between 2 positions
#;(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

; right edge of object:
(define (sprite-x2 obj) (+ (sprite-x obj) (sprite-width obj)))
; top edge of object:
(define (sprite-y2 obj) (- (sprite-y obj) (sprite-height obj)))

(define (sprite-overlap obj1 obj2)
  (and (range-overlap (sprite-x obj1) (sprite-x2 obj1) (sprite-x obj2) (sprite-x2 obj2))
       (range-overlap (sprite-y2 obj1) (sprite-y obj1) (sprite-y2 obj2) (sprite-y obj2)
)))
;; |---- Range 1 ------|
;;                   |--- Range 2 -----|

;; Does this handle all the possible cases??
(define (range-overlap start1 end1 start2 end2)
     (<= (max start1 start2) (min end1 end2)) );compute the size of the objects         
         
;;test if the distance is less than the size
      

;;movement

#;(define (alter-plane-y-on-key! p a-key)
  (cond
    [(or (key=? a-key "q")(key=? a-key "up"))
     (update-sprite! p #:y (- (sprite-y p) KEY-DISTANCE))]  
    [(or (key=? a-key "a")(key=? a-key "down"))
     (update-sprite! p #:y (+ (sprite-y p)  KEY-DISTANCE))]
    [(or (key=? a-key "g")(key=? a-key "\r"))
     (set! *plane-move-on-land* (not *plane-move-on-land*))]  ))

;;make each object update itself!!
#;(define (update-world-on-key! world a-key)
  (let ( [plane-obj (car world)] )
     (alter-plane-y-on-key! plane-obj a-key) )
  world) 

;; * the world state

;; the world state is the part of the world which changes
;; our world state is the x and y position of the plane
;; stored in a posn (position) structure
;; - 0, 0 is in the upper-left of the scene
;; we also have a "virtual parameter" "in-flight"

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

;; fix; pos to sprite!!
;; for drawing the world
; try: (place-plane 200)
#;(define (place-sprite sprite sceen)
  (place-image
   (sprite-image sprite)
   (sprite-x sprite)
   (- (sprite-y sprite) (/(sprite-height sprite) 2))
   sceen) )


(define (draw-world world)
  ; Here is the bug:
  #;(compoze BACKGROUND sprite-magazine)
  ; Here is what we intended:
  (compoze BACKGROUND world) )

;;make each object update itself!!
(define (update-world-on-tick! world)
  (for-each (λ (s) (if (sprite-on-tick s)
                       ( (sprite-on-tick s) s )
                       s ))
            world )
  world )

(define (update-world-on-key! world a-key)
  (for-each (λ (s) (if (sprite-on-key s)
                       ( (sprite-on-key s) s a-key )
                       s ))
            world )
  world)
    #;(let ( [plane-obj (car world)]
           [other-objects (cdr world)] )
      (cons (sprite-move! plane-obj) other-objects))

;; given a scene with some things already on it
;; and a list of more things to compose
;; returns a new scene with everything
(define (compoze sceen world)
  (if (null? world) ;if no objeccts remain
      sceen  ;;return scene
      (let ( [first-sprite (car world)]
             [other-sprites (cdr world)] )
      ;; compose the first object with the rest 
      ((sprite-draw first-sprite) first-sprite
                    (compoze sceen other-sprites)) ) ) )

(define (game-over w)
  (let ( [plane (car w) ]
         [missile (fourth w)] )
    (or (on-bottom?  plane)
        (sprite-overlap plane missile)
        (and (not (in-flight? plane))
             (>= (sprite-x2 plane) WIDTH) ) ) ) )

;; * putting it all together

;; create a world with
;; - the size of our screen
;; - 30 clock ticks per second
;; - initial "world-state" (a list of sprites)
(big-bang sprite-magazine
  (on-tick update-world-on-tick! 1/30)
  (to-draw draw-world WIDTH HEIGHT)
  (on-key update-world-on-key!)
  (stop-when game-over ))

;;(on-redraw place-balloona)
;;(on-redraw place-balloonb)
;;(on-redraw place-balloonc) 
;(on-redraw place-balloona)
