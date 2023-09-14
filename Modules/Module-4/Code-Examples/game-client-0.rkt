#lang racket

;; See this required module for ideas on how to evolve the game
(require "game-protocol-0.rkt")

(define (play host port-no)
  (with-handlers
    ( [exn:fail-network?
       (λ (e) (send-string out 'no-game-server (exn-message e))) ]
      [exn:fail?
       (λ (e) (send-string out 'expected (exn-message e))
         (close-input-port in)
         (close-output-port out) ) ] )
    ;; Connect to a game server
    (define-values (in out) (tcp-connect host port-no))
    ;; Get game info from Player
    ;; Send game info to Server
    (play-game in out)
  (close-input-port in)
  (close-output-port out) )

(define (play-game in out)
  ;; get guess from user
  ;; send guess to server
  ;; get feedback from server
  ;; give feedback to user
  ;; unless they guessed correctly, call play-game to continue
)
