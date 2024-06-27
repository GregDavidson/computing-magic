#lang racket

;; * Multiple Worlds Universe Server

;; The file sprites-words-games.rkt provides
;; - a description of the game
;; - inter-client (inter-world) protocol information
;; - additional require forms
(require 2htdp/universe)
(require data/gvector) ; growable vectors
(require "sprites-worlds-game.rkt")

;; The goal is to have this Server know as little
;; about any specific game as possible.  It should
;; - Welcome new worlds with a U2W-WELCOME message.
;; - Detach world on receipt of a W2U-DONE message.
;; Eventually this will evolve into a general-purpose
;; coordination server knowing nothing of the domain.

;; ** Our Library Types

;; Library Concepts:
;; a world - a client program
;; a universe - a collection of worlds managed by a server

;; Server-Side Library Types:

;; iworld - represents a World which has joined our Universe
;; mail - a message between a Universe Server and a World Client
;; bundle - a structure consisting of
;; - a Universe State
;; - a list of mail to send
;; - a list of worlds to disconnect
;; - bundle is used by the Universe Server!

;; The library doesn't know the detailed type of
;; - a Universe State
;; - mail

;; ** Our Concrete Types

;; A UniverseState is a collection of Worlds
;; Worlds are represented by iWorld structures.
;; We would like to
;; - Lookup Worlds by their World Numbers
;; - Reuse World Numbers after a World detaches

;; A good representation for a UniverseState is
;; UniverseState: Growable Vector (gvector) of (or/c #f iWorld) 
;; - An iWorld will be stored in the gvector slot given by that
;;   world's World Number.  So an iWorld index = a World Number.
;; - When a World detaches, its slot in the UniverseState becomes #f.
;; - New worlds get the lowest available World Number, replacing
;;   #f slots when available, otherwise growing the gvector.

;; If we should ever need more information about a world than its iWorld
;; structure, we could create a structure extending iWorld, e.g.

#;(serializable-struct world iWorld ( [world-number natura?]) )

;; *** Universe State Abstraction

(define (empty-universe) (gvector))

(define (universe-world u i) (gvector-ref u i))

;; Return the index of the first slot of
;; Universe u which is unused, i.e. #f.
;; If there are none, return the index which
;; is one beyond the last, which is given
;; by gvector-size.
(define (universe-next u)
  (let loop ( [i 0] )
    (if (or (= i (gvector-size u)) (not (gvector-ref u i)))
        i
        (loop (+ 1 i)) ) ) )

;; Return the index of iWorld w in Universe u
;; or #f if none.
;; Note: It would be nice if we could pull the
;; World Number out of the iWorld structure!!
(define (universe-world-index u w)
  (let loop ( [i 0] )e
    (cond [(= i (gvector-size u)) #f]
          [(not (gvector-ref u i)) (loop (+ 1 i))]
          [(iWorld=? w (gvector-ref u i)) i] ) ) )

;; Add World w to Universe u, modifying u.
(define (universe-add! u w)
  (gvector-add gv (universe-next u) w) )

;; Drop World at index i from Universe u, modifying u.
(define (universe-drop! u i)
  (gvector-set! u i #f) )

;; ** States and Actions

;; Given
;; - a Universe as a gvector of Worlds
;; - a new World as an iWorld or an extension of an iWorld
;; Return a bundle representing
;; - a new Universe incorporating the new World
;; - a U2W-WELCOME message to Mail to the new World
;; - no worlds to drop
(define (add-world universe new-world)
  (let ( [message (make-welcome (universe-next universe))]
         [worlds-to-drop '()] )
    (universe-add! universe new-world)
    (make-bundle universe
                 (list (make-mail new-world message))
                 worlds-to-drop ) ) )

;; Given
;; - a Universe as a List of Worlds
;; - a World as an iWorld
;; - a message from that world
;; Handle the message
;; Return a bundle representing
;; - the (possibly updated) Universe
;; - any return messages
;; - a list of any worlds to remove
(define (handle-world-msg universe world message)
  (let ( [world-number (universe-world-index universe world)]
         [msg-head (car message)] )
    (cond [(eq? W2U-DROP msg-head)
           (universe-drop! universe world-number)
           (make-bundle universe return-msgs (list world)) ]
          [else universe] ) ) )

;; Start the server

(universe (empty-universe)
          #;[state #f] ; suppress opening separate state window
          [on-new add-world]
          [on-msg handle-world-msg] )
