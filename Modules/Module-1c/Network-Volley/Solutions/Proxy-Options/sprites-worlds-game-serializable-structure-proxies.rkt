#lang racket
;; * Multiple Worlds Sprites Game Protocol and Overview

;; This file provides both the Universe Server and the World Clients with the
;; protocol which connects them.

(require racket/serialize)
(require uuid) ; univerally unique identifiers

;; The definition of struct sprite-proxy is at the end of the file
;; after a lot of comments!
(provide sprite-proxy)
(provide W2U-EMPTY W2U-DONE)
(provide U2W-WELCOME)
(provide message-head welcome? make-welcome welcome-alist welcome-world-number)

;; W2U messages are sent from a World Client to the Universe Server

;; Does the server need to know this?  Maybe notify
;; other clients instead??
(define W2U-EMPTY 'empty)  ;; we've lost our sprites

(define W2U-DONE 'done)  ;; detach us!

;; U2W-WELCOME messages provide information for a new
;; world in the form of an association list.
;; They should always provide a natural? number to
;; uniquely identify that world.
(define U2W-WELCOME 'welcome)
(define WORLD-NUMBER-KEY 'world-number)

;; Is alist an association list whose keys are all symbols?
(define (symbol-key-alist? alist)
  (or (null? alist)
      (and (pair? alist)
           (pair? (car alist)) (symbol? (caar alist))
           (symbol-key-alist? (cdr alist)) ) ) )

(define (message-head m)
  (if (pair? m) (car m) #f) )

(define (welcome? message)
  (and (pair? message)
       (eq? U2W-WELCOME (car message))
       (let ( [wn (assoc WORLD-NUMBER-KEY (cdr message))] )
         (and wn (natural? (cadr wn))) )
       (symbol-key-alist? (cdr message))) )

(define (make-welcome n . alist)
  (unless (natural? n) (error "invalid world-number ~a" n))
  (unless (symbol-key-alist? alist) (error "invalid alist ~a" alist))
  (cons U2W-WELCOME (cons (list WORLD-NUMBER-KEY n) alist)) )

(define (welcome-alist welcome)
  (unless (welcome? welcome) (error "invalid welcome ~a" welcome))
  (cdr welcome) )

(define (welcome-world-number welcome)
  (let ( [found (assoc WORLD-NUMBER-KEY (welcome-alist welcome))] )
    (unless (and (pair? found) (= (length found) 2)) (error "missing world number in welcome ~a" welcome))
    (let ( [number (second found)] )
      (unless (natural? number) (error "invalid world number ~a in ~a" number welcome))
      number ) ) )

;; ** Key Concepts

;; World Program aka "a client"
;; - controlled by one user
;; - runs on that user's computer
;; - connects to a universe server

;; Universe Server aka "the server"
;; - connects a collection of worlds
;; - coordinates them in playing a game together

;; Sprite
;; - A graphical object with
;;   - A shape - which might change!
;;   - A unique identity
;;   - A location and velocity
;;   - Reponses to certain key presses
;; - May be visible in multiple worlds
;; - Sprites might interact!

;; ** Simplest Version of Game

;; - Each world has its own unique World Number.
;; - Each world has its own unique color.
;; - Each world has one sprite of its color.
;; - The server will give a new sprite to any world that
;;   does not have one.
;; - Initially all sprites are balls labeled with their World Number.
;; - The balls start at the top of the canvas and fall at a constant velocity.
;; - A sprite is lost if it reaches the edge of the canvas.
;;   - Users can avoid this!

;; As a user
;; - We see all sprites of all worlds.
;; - We can use the left/right arrow keys to move our ball left or right.
;; - We can use the up/down arrow keys to give up/down velocity boosts
;;   to our ball.
;; - Velocity boosts will decay back to the original constant falling velocity.
;; - Careful boosting can keep our balls away from edges.
;; - Over-boosting can cause balls to collide with the edges!

;; ** Possible Game Enhancements

;; Allow persistent velocity changes in any direction
;; - Our sprite structures are already designed for this!
;; Add gravity to accelerate falling!

;; Gifting Sprites!
;; - Have an action to give our sprite to a world needing one
;;   - If no other world needs one we lose ours!
;;     - Maybe someone else passed them a sprite first!
;;   - If multiple worlds need one, a random one gets it.
;;   - If we successfully gave ours away
;;     - We gain a point
;; - The Server will only give a new sprite
;;   - to a world if they succesfully given one away
;;   - to a random world if no worlds have any sprites

;; Ejecting Transient Sprites!
;; - Eject transient sprites to change our velocity
;; - Transient sprites
;;   - Do not affect the game play.
;;   - Are not controllable
;;   - Disappear when they leave the canvas

;; More possible Enhancements
;; - Allow diverse shapes for sprites
;;   - Maybe upon succesfully giving one away you get
;;     a more interesting new one!
;; - Allow some surfaces to bounce sprites
;; - Allow sprites to interact with sprites of other worlds
;;   - Momentum exchanges and/or destructive interactions!

;; ** struct sprite-proxy

;; We need to be able to send sprites across worlds.
;; This requires us to serialize them, i.e. convert them to a byte stream.
;; Alas, Racket doesn't provide for serialization of
;; - regular structures, images or procedures

;; A serializable-struct can be serialized if all of their components
;; can be serialized.  So let's create a serializable sprite-proxy!

;; Images will be represented either by
;; (1) a filesystem path to a stored image
;; (2) a symbol representing a function which
;;     takes a color and returns an image.
;; Procedures will be represented by their names (symbols).
;; A sprite-proxy will have the same uuid as the sprite it is a proxy for.
;; Only the uuid field is required.  The other fields can default to #f if
;; the corresponding sprite field is irrelevant, i.e. not requiring an update.
(serializable-struct
 sprite-proxy (uuid image x y dx dy on-tick on-key to-draw)
 #:guard (struct-guard/c strict-uuid-string?  (or/c #f string? symbol?)
                         (or/c #f natural?)   (or/c #f natural?)
                         (or/c #f integer?)   (or/c #f integer?)
                         (or/c #f procedure?) (or/c #f procedure?) (or/c #f procedure?) )
 #:transparent )

;; ** Notes
