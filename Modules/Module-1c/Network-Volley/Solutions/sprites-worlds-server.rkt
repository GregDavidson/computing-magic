;; (setq-local racket-repl-buffer-name "*sprites-worlds-server-repl*")
#lang racket/base

;; * Multiple Worlds Universe Server

;; See sprites-worlds-game.org for information about the game.

;; The file sprites-words-games.rkt provides
;; - inter-client (inter-world) protocol information
;; - including a sprite-proxy structure
;; - You'll want to look it over carefully!
(require 2htdp/universe
         racket/cmdline
         racket/stream
         #; racket/contract
         #; racket/math )

(require "sprites-worlds-game.rkt")
#;(require (contract-in
  "sprites-worlds-game.rkt"
  [universe? (-> any/c boolean?)]
  [make-universe (-> universe?)]
  [universe-world (-> universe? natural? (or/c #f iworld?))]
  [universe-world-list (-> universe? (listof iworld?))]
  [universe-next-index (-> universe? natural?)]
  [universe-world-index (-> universe? iworld? (or/c #f natural?))]
  [universe-set! (-> universe? natural? iworld? void?)]
  [universe-drop! (-> universe? natural? void?)] ) )

#;(require
  (only-in "sprites-worlds-game.rkt"
           tracing *testing*
           message-head W2U-DONE make-welcome) )

;; ** Notes

;; The goal is to have this Server know as little
;; about any specific game as possible.  It should
;; - Welcome new worlds with a welcome? message.
;; - Detach world on receipt of a W2U-DONE message.
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
  (let* ( [index (universe-next-index universe)]
          [message (make-welcome index)]
          [worlds-to-drop '()] )
    (universe-set! universe index new-world)
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
                 (map (λ (w) (make-mail w message) )
                      (stream-filter
                       (λ (w) (and w (not (equal? w world))))
                       (universe-worlds universe) ) )
                 '() ; no worlds to remove
                 ) ) )

(define (drop-world u w)
  (define this 'drop-world)
  (let ( [i (universe-world-index u w)] )
    (when (tracing this) (eprintf "~a ~a\n" this i))
    (universe-drop! u i)
    (make-bundle u '() (list w)) ) )

(define (go)
  (universe (make-universe)
            #;[state #f] ; suppress opening separate state window
            [on-new add-world]
            [on-msg handle-world-msg]
            [on-disconnect drop-world] ) )

;; ** Process Command Line or Enter REPL

(define *cli* (make-parameter (positive? (vector-length (current-command-line-arguments)))))
(define *repl* (make-parameter (not (*cli*))))

(define (testing) (*testing* #t) (tracing #t))

(define args
  (if (not (*cli*))
      '()
      (command-line
       #:once-each
       [("-t" "--tracing") "trace everywhere" (tracing #t)]
       [("-T" "--testing") "make easier to test" (testing)]
       [("-i" "--repl") "enter repl, do not start client" (*repl* #t)]
       #:args functions-to-trace
       functions-to-trace ) ) )

(when (not (null? args))
  (let ( [this (find-system-path 'run-file)]
         [names (map string->symbol args)] )
    ;; warn us if name is not bound to a procedure
    (for-each (λ (name) (unless (procedure? (eval name))
                          (eprintf "~a: No procedure ~a to trace\n" this name) ))
              names )
    (when (not (null? names)) (apply tracing (cons #t names))) ) )

#;(when (*repl*)
  (tracing #t)                          ; trace everywhere!
  (*testing* #f)                        ; customize for easy testing
  (go) )
