#lang racket
;; Let the up-arrow key give us an upwards kick!

(require 2htdp/image) ; for drawing, image manipulation
(require 2htdp/universe) ; animation and game framework

;; Establish the Scene

;; Issues with the 2htdp/image library:
;; Horizontal (x) coordinates begin with 0 on the left and increase to the right
;; Vertical (y) coordinates begin with 0 at the top and increase downwards
;; Images are placed relative to their "keyholes".
;; Keyholes default to the center of each image.
;; Scenes are like images but without keyholes.

(define (half num-pixels) (quotient num-pixels 2))

(define SCENE-WIDTH 400) ; pixels
(define HALF-SCENE-WIDTH (half SCENE-WIDTH))
(define SCENE-HEIGHT 400) ; pixels
(define EMPTY-SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

;; A sprite could be any moving shape, e.g. a ball

(struct sprite (shape y dy on-tick on-key to-draw)
  #:transparent ; show insides when printed
  #:constructor-name make-sprite ; instead of sprite
  )

;; We'll have the sprite move at a fixed slow speed until it fully
;; emerges into the scene.  Then gravity will accelerate it!
(define EMERGING-DY -1)
(define DY-KICK 10)                 ; an upwards kick
;; Gravity near the earth creates an acceleration which adds
;; about 10 meters/second to the downwards velocity of an object.
;; If y is the height above ground, dy is the downwards velocity
;; and gravity is then a ddy, i.e. a change in dy.
;; If we let every pixel equal a meter, then gravity would be a
;; ddy of 10 every 30 ticks, since we tick 30 times a second.
(define GRAVITY (- (/ 10.0 30.0)))
;; A non-integer gravity will mean that dy and y won't always
;; be an integer, so be sure to round y before placing the sprite!

;; The state of our "World" is a sprite with state attributes

(define (update-sprite-on-tick s)
  (let* ( ;; when emerging (at the top) the sprite maintains the emerging-dy
          ;; otherwise it accelerates due to gravity
          [new-dy (if (emerging? s) EMERGING-DY (+ (sprite-dy s) GRAVITY))]
          ;; a bouncing sprite (at the bottom) will return to the top of the scene
          [new-y (if (bouncing? s) SCENE-HEIGHT (+ (sprite-y s) new-dy))] )
    (struct-copy sprite s [y new-y] [dy new-dy]) ) )

(define (update-sprite-on-key s k)
  ;; cond handier than if when responding to multiple keys!
  (cond [(key=? k "up")
         ;; return new sprite with a new dy value, all else unchanged
         (struct-copy sprite s [dy (+ (sprite-dy s) DY-KICK)]) ]
        [#t s] ) )  ; else return unchanged original sprite

(define (draw-sprite-onto-canvas sprite canvas)
  (place-image (sprite-shape sprite)
               HALF-SCENE-WIDTH
               ;; Our y represents height above ground
               ;; so it goes from SCENE-HEIGHT down to 0.
               ;; place-image wants to know where to put
               ;; the center of the sprite relative to the
               ;; upper-left corner of the scene.
               (round (- SCENE-HEIGHT (sprite-y sprite)))
               canvas ) )

(define initial-world
  (list 
   (make-sprite (circle 20 "solid" "blue") ; shape
              SCENE-HEIGHT ; y location = height-above-ground
              EMERGING-DY ; dy = drop rate
              update-sprite-on-tick
              update-sprite-on-key
              draw-sprite-onto-canvas ) ) )

;; Rigorous sprite-list? would check each element in the list
#;(define (sprite-list? maybe-sprite-list)
    (or-map (λ (maybe-sprite) (sprite? maybe-sprite)) maybe-sprite-list) )
;; A less rigorous but fast sprite-list? predicate might just
;; check that it looks like we've got a (possibly empty) list
(define (sprite-list? x) (or (null? x) (pair? x)))

(define (half-sprite-height sprite) (half (image-height (sprite-shape sprite))))

;; is the sprite emerging from the top of the scene?
(define (emerging? sprite)
  (< (- SCENE-HEIGHT (sprite-y sprite)) (half-sprite-height sprite)) )

;; is the sprite bouncing?
(define (bouncing? sprite)
  (< (sprite-y sprite) (half-sprite-height sprite)) )

;; add 0 or more updated sprites to a new world
;; - world is the accumulating list of sprites for the new world
;; - sprites (from a sprite update function) might be
;; - '() meaning drop them from the new world
;; - 1 sprite for the new world
;;   - might or might not be the original sprite
;; - a list of sprites for the new world
;;   - might or might not include the original sprite
(define (append-sprites sprites world)
  (cond [(not (sprite-list? world)) (error 'append-sprites "expected sprite-list ~a" world)]
        [(sprite? sprites) (cons sprites world)]
        [(sprite-list? sprites) (append sprites world)]
        [#t (error 'append-sprites "expected sprites ~a" sprites)] ) )

;; return updated world composed of all sprites returned by updates
(define (update-world-on-tick world)
  (foldr append-sprites ; append together all the return values
         '() ; starting with an empty world
         (map ; get a list of return values by
          ;; calling sprite's own on-tick method to itself returning 0 or more sprites
          (λ (sprite) ( (sprite-on-tick sprite) sprite ))
          ;; on every sprite in the world
          world ) ) )

;; return updated world composed of all sprites returned by updates
(define (update-world-on-key world key)
  (foldr append-sprites ; append together all the return values
         '() ; starting with an empty world
         (map ; get a list of return values by
          ;; calling sprite's own on-tick method to itself returning 0 or more sprites
          (λ (sprite) ( (sprite-on-key sprite) sprite key))
          ;; on every sprite in the world
          world ) ) )

;; Return a new scene with the world (the sprite)
;; centered horizontally and at its correct y height.
(define (draw-world world)
  (foldr draw-sprite-onto-canvas EMPTY-SCENE world) )

(big-bang initial-world
  [on-tick update-world-on-tick 1/30] ; tick 30 times each second
  [on-key update-world-on-key]
  [to-draw draw-world SCENE-WIDTH SCENE-HEIGHT] ; draw world in canvas
  )