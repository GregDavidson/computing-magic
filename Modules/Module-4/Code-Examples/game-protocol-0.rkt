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

;; How would this protocol need to evolve to provide
;; - a game board of top players
;; - competitive games between multiple users

(provide (struct-out game)
         (struct-out guess)
         (struct-out feedback) )

(require racket/serialize)

;; for guesses and for mininum of allowed range
(define (integer>=0? n) (and (integer? n) (>= n 0)))

;; for maximum of allowed range
(define (integer>0? n) (and (integer? n) (>= n 0)))

;; A game record has
;; - a player's name
;;   - currently unused, but would be great for
;;     - multi-player games
;;     - game boards, etc.
;; - min and max values for the number to be guessed
;;   - negotiated at game start
(serializable-struct game (player min max)
                      #:transparent
                      #:guard
                      (λ (player min max type-name)
                        (unless (integer>=0? min)
                          (error type-name "bad min: ~e" min) )
                        (unless (integer>0? max)
                          (error type-name "bad max: ~e" max) ) )
                      (values player min max) )

;; A guess record simply contains a guess
(serializable-struct guess (number)
                      #:transparent
                      #:guard
                      (λ (number type-name)
                        (unless (integer>=0? number)
                          (error type-name "bad number: ~e" number) )
                        number ) )

;; A feedback record contains the server's feedback
;; < when the guess was too large
;; > when the guess was too small
;; = when the guess was right
;; ! when the guess was outside of the min and max of this game
(serializable-struct feedback (message)
                      #:transparent
                      #:guard
                      (λ (message type-name)
                        (when (not (member message '(< > = !)))
                          (error type-name "bad message: ~e" message))
                        message ) )
