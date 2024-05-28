#lang racket
;; * Ball Toss Client

;; This client is either tossing a ball or resting.

(require 2htdp/image)
(require 2htdp/universe)

;; Library Concepts:
;; a world - a client program
;; a universe - a collection of worlds managed by a server

;; Our mail Messages:
(define GO-MESSAGE 'it-is-your-turn) ;  S2W = Server to World
(define STOP-MESSAGE 'done) ; W2S = World to Server

;; Our WorldState is either
;; – 'resting meaning we don't have the ball
;; – a ball-height?

(define (ball-height? n)
  (and (natural? n) (>= n 0) (<= n SCENE-HEIGHT)) )

(define (world-state? state)
  (or (equal? 'resting state)
      (ball-height? state) ) )

(define INITIAL-STATE 'resting) ; we're initially resting
(define SCENE-WIDTH 100) ; pixels
(define HALF-SCENE-WIDTH (quotient SCENE-WIDTH 2))
(define SCENE-HEIGHT 100) ; pixels
(define MT (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define BALL (circle 20 "solid" "blue"))
 
;; A WorldResult is one of:
;; – WorldState
;; – (make-package WorldState STOP-MESSAGE)

;; WorldState GO-MESSAGE -> WorldResult
(define (receive state message)
  ;; Only current possibility is state='resting, message=GO-MESSAGE
  ;; but let's anticipate an evolution of possibilities!
  (cond [(equal? GO-MESSAGE message)
         (cond
           ;; change our state to the ball location
           [(equal? 'resting state) SCENE-HEIGHT]
           ;; report erroneous messages, leave state unchanged
           [else (printf "receive error 1: ~a in state ~a!\n" message state) state] ) ]
        [else (printf "receive error 2: ~a in state ~a!\n" message state) state] ) )
 
; WorldState -> WorldResult
; move this ball upwards for each clock tick
; or stay 'resting
(define (move x)
  (cond
    [(equal? 'resting x) x]
    [(zero? x) (make-package 'resting 'done)]
    [(ball-height? x) (sub1 x)]
    [else (printf "move error: state ~a!\n" x) (make-package 'resting 'done)] ) )

;; ** Rendering

;; return a function which can turn our state into an image
;; client-name -> WorldState -> Image
(define (render-for client-name)
  (λ (state)
    (cond
      [(symbol? state)
       (place-image/align (above/align "center"
                                       (text client-name 12 "black")
                                       (text "resting" 12 "red") )
                          HALF-SCENE-WIDTH 10
                          "center" "top"
                          MT ) ]
      [(number? state)
       (place-image/align BALL
                          HALF-SCENE-WIDTH (- state (image-height BALL))
                          "center" "top"
                          MT ) ] ) ) )

; String -> WorldState
; create and hook up a world
(define (create-world a-name [server LOCALHOST])
  (when (symbol? a-name) (set! a-name (symbol->string a-name)))
  (big-bang INITIAL-STATE
   (on-receive receive)
   (to-draw    (render-for a-name) )
   (on-tick    move 1/30)
   (name       a-name)
   (register   server) ) )

(define (go [server LOCALHOST]) (launch-many-worlds (create-world "Eti" server)
                                 (create-world "Brandt" server)
                                 (create-world "Touch" server) ))