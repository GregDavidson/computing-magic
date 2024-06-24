#lang racket
;; * Multiple Worlds Sprites Game Protocol and Overview

(require racket/serialize)
(require uuid) ; univerally unique identifiers

;; The definition of struct sprite-proxy is at the end of the file;
;; everything else beyond this section is comments!
(provide sprite-proxy)
(provide W2S-PASS W2S-DONE S2W-COLOR S2W-CLIENT)

;; W2S messages are sent from a World to the Server

;; Does the server need to know this?  Maybe notify
;; other clients instead??
(define W2S-PASS 'pass)  ;; we've lost our sprites

(define W2S-DONE 'done)  ;; detach us!

;; Obsoleted by S2W-CLIENT??
;; S2W messages are sent from the Server to a World
;; S2W-COLOR should only be sent at the beginning of a message
;; preceeding any sprite creation.
(define S2W-COLOR 'set-color)  ;; establish our color

;; S2W-CLIENT messages assign the smallest available
;; non-negative integer uniquely identifying a new client.
;; When a client detaches their number becomes available
;; for reassignment.
(define S2W-CLIENT 'set-client-number)

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

;; - Each world has its own unique color.
;; - Each world has one sprite of its color.
;; - The server will give a new sprite to any world that
;;   does not have one.
;; - All sprites are simple balls.
;; - The balls start at the top of the canvas
;;   and fall.
;; - If a ball reaches the bottom or any other
;;   edge of the canvas it will be lost.

;; As a user
;; - We see all sprites of all worlds in their colors.
;;   - They're spaced horizontally.
;; - We can use the arrow keys to give velocity boosts
;;   to our ball in any direction.
;; - Any velocity beyond simple falling will decay.

;; ** Possible Game Enhancements

;; Allow velocity changes in any direction
;; - Our sprite structures are already designed for this!

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
;; - Add gravity to accelerate falling.
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
