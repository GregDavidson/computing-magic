#lang racket

;; * Ball Toss Server

;; This server coordinates a ball tossed between worlds.

(require 2htdp/universe)

;; Library Concepts:
;; a world - a client program
;; a universe - a collection of worlds managed by a server

;; Library Types:

;; iworld - represents a World which has joined our Universe
;; mail - a message between a Universe Server and a World Client
;; bundle - a structure consisting of
;; - a Universe State
;; - a list of mail to send
;; - a list of worlds to disconnect

;; The library doesn't know the detailed type of
;; - a Universe State
;; - mail

;; Our Concrete Types

;; UniverseState: [Listof iworld?]
;; Our mail Messages:
(define GoMessage 'it-is-your-turn) ;  S2W = Server to World
(define StopMessage 'done) ; W2S = World to Server

;; States and Actions

;; The first world in our UniverseState has state ACTIVE
;; Any other worlds have state PASSIVE
;; When the ACTIVE world sends us StopMessage
;; - We move it to the end of the UniverseState
;; - We send GoMessage to the new ACTIVE world

;; Return a bundle representing
;; - a universe extended with the new-world
;; - a GoMessage for the ACTIVE world
;; - no worlds to remove
(define (add-world universe new-world)
  (let ( [new-universe (append universe (list new-world))] )
    (make-bundle new-universe
                 (list (make-mail (first new-universe) GoMessage))
                 '() ) ) )

;; Return a bundle representing
;; - a universe with the previous ACTIVE state moved to the end
;; - a GoMessage for the new ACTIVE world
;; - no worlds to remove
(define (switch universe world m)
  (let ( [new-universe (append (rest universe) (list (first universe)))] )
    (make-bundle new-universe
                 (list (make-mail (first new-universe) GoMessage))
                 '() ) ) )

;; Start the server
(universe '() [on-new add-world] [on-msg switch])
