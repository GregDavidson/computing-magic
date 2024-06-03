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

;; Something to fall, why not a ball?

(struct ball (shape y dy)
  #:transparent ; show insides when printed
  #:constructor-name make-ball ; make-ball instead of just ball
  )

;; We'll have the ball move at a fixed slow speed until it fully
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
;; be an integer, so be sure to round y before placing the ball!

;; The state of our "World" is a ball with state attributes

(define initial-ball
  (make-ball (circle 20 "solid" "blue") ; shape
             SCENE-HEIGHT ; y location = height-above-ground
             EMERGING-DY ; dy = drop rate
             ) )

(define HALF-BALL-HEIGHT (half (image-height (ball-shape initial-ball))))

;; is the ball emerging from the top of the scene?
(define (emerging? ball)
  (< (- SCENE-HEIGHT (ball-y ball)) HALF-BALL-HEIGHT) )

;; is the ball bouncing?
(define (bouncing? ball)
  (< (ball-y ball) HALF-BALL-HEIGHT) )

(define (update-ball-on-tick ball)
  (let* ( ;; when emerging (at the top) the ball maintains the emerging-dy
          ;; otherwise it accelerates due to gravity
          [new-dy (if (emerging? ball) EMERGING-DY (+ (ball-dy ball) GRAVITY))]
          ;; a bouncing ball (at the bottom) will return to the top of the scene
          [new-y (if (bouncing? ball) SCENE-HEIGHT (+ (ball-y ball) new-dy))] )
    (make-ball (ball-shape ball) ; not changing the shape, but we could!
               new-y new-dy ) ) )

(define (update-ball-on-key b k)
  ;; should we wish to respond to multiple keys in the future
  ;; cond will be handier than if.
  (cond [(key=? k "up")
         ;; return new ball with a new dy value, all else unchanged
         (struct-copy ball b [dy (+ (ball-dy b) DY-KICK)]) ]
        ; else return unchanged original ball
        [else b] ) )

;; Return a new scene with the ball (the ball)
;; centered horizontally and at its correct y height.
(define (draw-ball ball)
  (place-image (ball-shape ball)
               HALF-SCENE-WIDTH
               ;; Our y represents height above ground
               ;; so it goes from SCENE-HEIGHT down to 0.
               ;; place-image wants to know where to put
               ;; the center of the ball relative to the
               ;; upper-left corner of the scene.
               (round (- SCENE-HEIGHT (ball-y ball)))
               EMPTY-SCENE ) )

(big-bang initial-ball                  ; our initial world
  [on-tick update-ball-on-tick 1/30]    ; update world 30 "ticks" a second
  [on-key update-ball-on-key]           ; update world when user presses a key
  [to-draw draw-ball SCENE-WIDTH SCENE-HEIGHT] ; draw world in canvas
  )
