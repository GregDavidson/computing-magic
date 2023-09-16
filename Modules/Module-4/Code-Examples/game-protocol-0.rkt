#lang racket

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

(require racket/serialize)

(define (serialize-write item out)
  (printf "Sending ~a\n" item)
  (write (serialize item) out )
  (flush-output out) )

(define (deserialize-read in)
  (let ( [item (read in)] )
    (unless (serializable? item) (raise (unexpected 'serializable item)))
    (deserialize item) ) )

;; for guesses and for mininum of allowed range
(define (integer>=0? n) (and (integer? n) (>= n 0)))

;; for maximum of allowed range
(define (integer>0? n) (and (integer? n) (>= n 0)))

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
                          (error type-name "bad min ~e" min) )
                        (unless (integer>0? max)
                          (error type-name "bad max ~e" max) )
                        (values player min max) ) )

(define (write-game game out)
  (serialize-write game out) )

(define (read-game in)
  (let ( [item (deserialize-read in)] )
    (unless (game? item) (raise (unexpected 'game item)))
    (printf "Got ~a\n" item)
    item ) )

;; A guess record simply contains a guess
(serializable-struct guess (number)
                      #:transparent
                      #:guard
                      (λ (number type-name)
                        (unless (integer>=0? number)
                          (error type-name "bad number: ~e" number) )
                        number ) )

(define (write-guess guess out)
  (serialize-write guess out) )

(define (read-guess in)
  (let ( [item (deserialize-read in)] )
    (unless (guess? item) (raise (unexpected 'guess item)))
    (printf "Got ~a\n" item)
    item ) )

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
                          (error type-name "bad message: ~e" message) )
                        message ) )

(define (write-feedback feedback out)
  (serialize-write feedback out) )

(define (read-feedback in)
  (let ( [item (deserialize-read in)] )
    (unless (feedback? item) (raise (unexpected 'feedback item)))
    (printf "Got ~a\n" item)
    item ) )

(struct unexpected (expected got))
