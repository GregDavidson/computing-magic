#lang racket

;; * A Simple Game Protocol

;; ** About The Protocol

;; This Racket Package provides functions which implement a communications
;; protocol between a Client Program and a Server Program which together
;; implement a game

;; In the simplest version of the game:
;; - the Client informs the Server of
;;   - a player's name and a range of values for the guesses
;; - the Server picks a number in that range
;; - the Client sends guesses to the Server
;; - the Server replies with feedback

;; Note that this module says nothing about any User Interface between the
;; Client and the User Playing the Game.

;; How could this protocol evolve to support
;; - a game board of top players
;; - competitive games between multiple users

;; How might this protocol be made
;; - more resistant to errors?
;; - more graceful in the face of errors?

;; ** Provided Bindings

(provide (struct-out game)
         write-game
         read-game
         (struct-out guess)
         write-guess
         read-guess
         (struct-out feedback)
         write-feedback
         read-feedback
         (struct-out unexpected)
         serialize-write
         deserialize-read )

;; ** Serialization Support

(require racket/serialize)

(define (serialize-write item out [type-proc #f] [type-name #f])
  (when type-proc
    (unless (type-proc item)
      (raise (unexpected
              (or type-name (object-name type-proc) 'specific-type)
              item )) ) )
  (printf "Sending ~a\n" item)
  (write (serialize item) out )
  (flush-output out) )

(define (deserialize-read in [type-proc #f] [type-name #f])
  (let ( [item (deserialize (read in))] )
    (when type-proc
      (unless (type-proc item)
        (raise (unexpected
                (or type-name (object-name type-proc) 'specific-type)
                item )) ) )
    item ) )

;; ** serializable-struct game

;; A game record has
;; - a player's name, currently unused, available for
;;   - multi-player games, game boards, etc.
;; - min and max values for the number to be guessed
;;   - negotiated at game start
(serializable-struct game (player min max)
                      #:transparent
                      #:guard
                      (λ (player min max type-name)
                        (unless (integer>=0? min)
                          (error type-name "bad min ~a" min) )
                        (unless (integer>0? max)
                          (error type-name "bad max ~a" max) )
                        (values player min max) ) )

(define (write-game game out)
  (serialize-write game out game?) )

(define (read-game in)
  (deserialize-read in game?) )

;; ** serializable-struct guess

;; A guess record simply contains a guess
(serializable-struct guess (number)
                      #:transparent
                      #:guard
                      (λ (number type-name)
                        (unless (integer>=0? number)
                          (error type-name "bad number: ~a" number) )
                        number ) )

(define (write-guess guess out)
  (serialize-write guess out guess?) )

(define (read-guess in)
  (deserialize-read in guess?) )

;; ** serializable-struct feedback

;; A feedback record contains the server's feedback
;; '< when the guess was too large
;; '> when the guess was too small
;; '= when the guess was right
;; '! when the guess was outside of the min and max of this game
(serializable-struct feedback (message)
                      #:transparent
                      #:guard
                      (λ (message type-name)
                        (unless (member message '(< > = !))
                          (error type-name "bad message: ~a" message) )
                        message ) )

(define (write-feedback feedback out)
  (serialize-write feedback out feedback?) )

(define (read-feedback in)
  (deserialize-read in 'feedback) )

;; ** struct unexpected and utility functions

(struct unexpected (expected got))

;; for guesses and for mininum of allowed range
(define (integer>=0? n) (and (integer? n) (>= n 0)))

;; for maximum of allowed range
(define (integer>0? n) (and (integer? n) (> n 0)))
