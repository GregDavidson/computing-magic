;; -*- mode: racket; racket-repl-buffer-name: "*sprites-universe-server*"; -*-
#lang racket/base

;; * Multiple Worlds Universe Server

;; See sprites-universes.org for information

;; The file sprites-words-games.rkt provides
;; - inter-client (inter-world) protocol information
;; - including a sprite-proxy structure
;; - You'll want to look it over carefully!
(require 2htdp/universe
         racket/cmdline
         racket/sequence
         racket/contract
         racket/math )

;; We'll need these predicate functions imported first
;; so that we can use them in the contracts of the other
;; functions we'll import next.
(require
 (contract-in "sprites-framework.rkt"
              [universe? (-> any/c boolean?)]
              [world-id? (-> any/c boolean?)]
              [message? (-> any/c boolean?)]
              [welcome-message? (-> any/c boolean?)]
              [goodbye-message? (-> any/c boolean?)] ) )

(require
 (contract-in "sprites-framework.rkt"
              [make-universe (->* () (natural?) universe?)]
              [universe-worlds (-> universe? sequence?)]
              [universe-world-index (-> universe? iworld? natural?)]
              [universe-add! (-> universe? iworld? natural?)]
              [universe-drop! (-> universe? natural? void?)]

              [message-world (-> message? world-id?)]
              [make-welcome (->* (natural?) () #:rest (listof any/c) welcome-message?)]

              [my-parameter (->* (any/c procedure? symbol?) ((or/c #f symbol? string?)) parameter?)]
              [*tracing* parameter?]
              [tracing (->* () (symbol?) boolean?)]
              [*testing* parameter?] ) )

(require (only-in "sprites-framework.rkt"
                  program-name program-is-standalone?
                  trace-procs get-string-line ))

 ;; ** Our Goal: Write a Very Generic World Server

;; The goal is to have this Server know as little
;; about any specific game as possible.  It should
;; - Welcome new worlds with a welcome? message.
;; - Detach world on receipt of a goodbye-message.
;; We would like to evolve this into a general-purpose
;; coordination server knowing nothing of the domain.

;; ** The 2http Framework

;; Framework Concepts:
;; World - a client program
;; Universe - a collection of worlds managed by a server

;; Server-Side Framework Types:

;; iworld - represents a World which has joined our Universe
;; mail - a message between a Universe Server and a World Client
;; bundle - a structure consisting of
;; - a Universe State
;; - a list of mail to send
;; - a list of worlds to disconnect

;; a bundle, as used by the Universe Server
;; is similar to a package used by a World Client.

;; The framework doesn't know the detailed type of
;; - a Universe State
;; - mail

;; ** Our Concrete Types

;; A UniverseState is a collection of Worlds
;; Worlds are represented by iWorld structures.
;; We would like to
;; - Lookup Worlds by their World Numbers
;; - Reuse World Numbers after a World detaches

;; If we should ever need more information about a world than its iWorld
;; structure, we could create a structure extending an iWorld.

;; ** States and Actions

;; Given
;; - a Universe as a collection of Worlds indexed by World Numbers
;; - a new World as an iworld or an extension of an iWorld
;; Return a bundle representing
;; - a new Universe incorporating the new World
;; - a welcome message to Mail to the new World
;; - no worlds to drop
(define (add-world universe new-world)
  (define this 'add-world)
  (let* ( [index (universe-add! universe new-world)]
          [message (make-welcome index)]
          [worlds-to-drop '()] )
    (when (tracing this) (eprintf "~a index ~a world ~a\n" this index new-world))
    (make-bundle universe
                 (list (make-mail new-world message))
                 worlds-to-drop ) ) )

;; Given
;; - a Universe as a collection of Worlds indexed by World Numbers
;; - a World as an iWorld
;; - a message from that world
;; Handle the message
;; - We only have goodbye messages so far
;; - More message types can be added as needed
;; Return a bundle representing
;; - the (possibly updated) Universe
;; - any return messages
;; - a list of any worlds to remove
(define (handle-world-msg universe world message)
  (let ( [world-id (message-world message)] )
    (when (goodbye-message? message)
      (universe-drop! universe world-id) )
    ;; relay message to all other worlds,
    ;; i.e. not the one which sent it
    (make-bundle universe
                 (sequence->list
                  (sequence-map (λ (w) (make-mail w message) )
                                (sequence-filter
                                 (λ (w) (and w (not (equal? w world))))
                                 (universe-worlds universe) ) ) )
                 '() ; no worlds to remove
                 ) ) )

;; BUG: This isn't being called!!!
;; Consequence: memory leak
;; Q: Is this related to the complaint
;; about old world names, e.g.
;; "\"Greg\"" not on list
;; When world "Greg" was disconnected
;; and a new world connects???
;; Wow, it complains about all of the
;; disconnected worlds!!!
(define (drop-world u w)
  (define this 'drop-world)
  (let ( [i (universe-world-index u w)] )
    (when (tracing this) (eprintf "~a ~a\n" this i))
    (universe-drop! u i)
    (make-bundle u '() (list w)) ) )

(define (go [trace #f])
  (define this 'go)
  (parameterize ( [*tracing* (or trace (*tracing*))] )
    (universe (make-universe)
              #;[state #f] ; suppress opening separate state window
              [on-new add-world]
              [on-msg handle-world-msg]
              [on-disconnect drop-world] ) ) )

;; ** Run as Command or within REPL

(if (program-is-standalone?)
    (begin
      (command-line
       #:once-each
       [("-t" "--tracing") "trace everywhere" (*tracing* #t)]
       [("-T" "--testing") "make easier to test" (begin (*tracing* #t) (*testing* #t))]
       #:args procs-to-trace
       (trace-procs procs-to-trace) )
      (go) )
    ;; we should be at a REPL
    (let ( [yes-pattern (regexp "^ *[yY]")]
           [reply (get-string-line "run universe server? [y/n]" )] )
      (when (regexp-match yes-pattern reply)
        (go #t) ) ) )
