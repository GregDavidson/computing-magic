;; -*- mode: racket; racket-repl-buffer-name: "*sprites-worlds-client-repl*"; -*-
#lang racket/base
;; * Multiple Worlds Multiple Sprites Client Test Module

;; See sprites-worlds-game.org for information about the game.

;; ** Packages, Game Type Predicates, struct sprite, more Game Requires

;; The file sprites-words-games.rkt provides
;; - inter-client (inter-world) protocol information
;; - including a sprite-proxy structure
;; - You'll want to look it over carefully!

;; ** What We Require

;; update to import from specific game file too!!!
#: (require "sprites-worlds-game.rkt")
#: (require "sprites-worlds-client.rkt")
(require "network-volley-game.rkt")

;; ** Testing Machinery

;; requirements for testing
(require data/gvector
         rackunit
         racket/function
         racket/contract/base
         #; 2htdp/universe )

;; To test any side-effecting procedure, any
;; data structure which will be side-effected
;; must be deep copied, i.e. reallocated into
;; fresh storage in memory.  A deep copy of an
;; object should compare as equal? but not eq?.

;; Allocate new gvectors to make a copy of
;; directly nested gvectors.
;; Any element that is not a gvector will
;; not be copied, even if it's a container
;; of some sort with a gvector inside of it.
(define (gv-copy x)
  (if (not (gvector? x))
      ;; return x's value without copying it
      x
      ;; build a new gvector copy of x
      (for/gvector ( [y (in-gvector x)] )
        (gv-copy y) ) ) )

;; ** Testing Params, Proxies, Sprites

;; world 0, color "red", no falling
(define params-0 (make-params 0 (choose-color 0) 0))

;; default sprite created manually
(define sprite-1 (sprite (make-ball params-0)
                              (sprite-proxy-x PROXY-0)
                              (sprite-proxy-y PROXY-0)
                              (sprite-proxy-dx PROXY-0)
                              (sprite-proxy-dy PROXY-0)
                              move-sprite boost-sprite-on-key draw-sprite ))

(define sprite-2 (struct-copy sprite sprite-1 [dy DY-FALLING]))

(define proxy-2 (make-sprite-proxy 0 ; sprite id
                                   #f ; image
                                   #f ; x
                                   (+ (sprite-y sprite-2) (sprite-dy sprite-2)) ; y
                                   #f ; dx
                                   (decay (sprite-dy sprite-2) 0) ; dy
                                   #f #f #f #;methods ) )

(check-equal? sprite-1 (proxy->sprite params-0 PROXY-0) "proxy->sprite")

;; *** Testing Actions, Worlds, Universes

;; an action which will drop the sprite at index 0
(define drop-0 (make-actions params-0 (list 0)))

;; an action which will update or create a sprite
;; described by PROXY-0
(define create-0 (make-actions params-0 (list PROXY-0)))

;; an empty universe
(define universe-0 (gvector))

;; missing world
(check-pred no-sprites-left? (make-state params-0 universe-0) "universe-0")

;; an empty universe with one emptied world
(define universe-00 (gvector (gvector #f)))

(check-pred no-sprites-left? (make-state params-0 universe-00) "universe-00")

;; an emptied universe with one emptied world
(define universe-000 (gvector (gvector #f) #f))

(check-pred no-sprites-left? (make-state params-0 universe-000) "universe-000")

(let ( [u (gv-copy universe-000)] )
  (check-equal? u universe-000 "gv-copy not equal?")
  (check-not-eq? u universe-000 "gv-copy eq?") )

;; a world with a single unmoving sprite
(define world-1 (gvector sprite-1))

;; a world with a single falling sprite
(define world-2 (gvector sprite-2))

;; a universe with a single default sprite
(define universe-1 (gvector world-1))

;; ** Testing update-sprites!

(check-equal? universe-00
              (let ( [u (gv-copy universe-1)] )
                (update-sprites! drop-0 u)
                u ) "drop-0 from universe-1" )

(check-equal? universe-1
              (let ( [u (gv-copy universe-0)] )
                (update-sprites! create-0 u)
                u ) "create-0 in universe-0" )

(check-equal? universe-1
              (let ( [u (gv-copy universe-00)] )
                (update-sprites! create-0 u)
                u ) "create-0 in univeres-00" )

;; ** Testing Gathering Actions

(check-false (gather-actions-on-tick params-0 world-1) "gather-actions-on-tick world-1")

;; Redo test after patch removing incomplete proxies is better resolved
#;(let ( [sp (move-sprite params-0 0 sprite-2)] )
  (check-equal? proxy-2 sp "move-sprite")
  (check-equal? (make-actions params-0 (list proxy-2))
                (gather-actions-on-tick params-0 world-2) "gather-actions-on-tick world-2" ) )

;; ** Testing States, Packages

;; uninitialized world state is forgiven
(check-pred (negate no-sprites-left?) no-state-yet "no-state-yet")

;; a state based on universe-0
(define state-0 (make-state params-0 universe-0))

(check-pred no-sprites-left? state-0 "universe-0 in state-0")

;; a state based on universe-1
(define state-1 (make-state params-0 universe-1))

(check-pred (negate no-sprites-left?) state-1 "universe-1 in state-1")

;; a package based on universe-1 with
;; mail to replicate dropping PROXY-0
(define package-drop-0 (make-package state-0 (list drop-0)))

;; a package based on universe-0 with
;; mail to replicate materializing PROXY-0
(define package-1 (make-package state-1 (list create-0)))
