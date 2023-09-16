#lang racket

;; * A Simple Game Server

;; See this module for ideas on how to evolve the game
(require "game-protocol-0.rkt")

#; (define stop-serve (serve 8080)) ; start the server
#; (stop-serve) ; stop the server

;; ** Create a server and return a shutdown thunk
(define (serve port-num)
  ;; Ensure all resources are managed by main-custodian
  (define main-custodian (make-custodian))
  (parameterize ( [current-custodian main-custodian] )
    ;; Create a rendezvous socket associated with the given port-num
    ;; which can queue up to 5 clients before we get to them.
    (define listener (tcp-listen port-num 5 #t))
    ;; Define an infinite loop of accepting and handling clients
    (define (loop)
      (accept-and-handle listener)
      (loop) )
    ;; Start that loop!
    (thread loop)
    (λ () (custodian-shutdown-all main-custodian)) ) )

;; ** Accept and Handle a new client from the Rendezvous Socket
(define (accept-and-handle listener)
  ;; Receive the I/O connections for a new client
  ;; - this will suspend until a client comes along
  (define-values (in out) (tcp-accept listener))
  ;; Delegate those connections to a new procedure to handle that client
  ;; Until we finish with this client, new clients have to wait!
  (handle in out)
  ;; Close our connections to the client we've now finished with.
  (close-input-port in)
  (close-output-port out) )

;; ** Get game information then Play the Game
(define (handle in out)
  (with-handlers
    ( ; standard failures derive from struct exn:fail
     [exn:fail? (λ (e) (fprintf out "Error: ~a" (exn-message e))
                  (close-input-port in)
                  (close-output-port out) )]
     ; our protocol failures use struct unexpected
     [unexpected? (λ (e) (fprintf out "Expected ~a got ~a"
                                     (unexpected-expected e)
                                     (unexpected-got e) )
                       (close-input-port in)
                       (close-output-port out) )] )
    (let* ( [g (read-game in)]
            [player (game-player g)]
            [min (game-min g)]
            [max (game-max g)]
            [target (+ min (random (- max min)))] )
      (printf "Target: ~a\n" target)
      (play-game in out player min max target) ) ) )

;; ** Play the Game
(define (play-game in out player min max target)
    (let* ( [g (read-guess in)]
            [n (guess-number g)] )
      (printf "Guess: ~a\n" n)
      (write-feedback (feedback (cond ([< n min] '!)
                                      ([>= n max] '!)
                                      ([< n target] '<)
                                      ([> n target] '>)
                                      (#t '=) )) out )
      (unless (= n target) ; this is always false??
        (play-game in out player min max target) ) ) )
