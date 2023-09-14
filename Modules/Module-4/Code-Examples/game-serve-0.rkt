#lang racket

;; See this required module for ideas on how to evolve the game
(require "game-protocol-0.rkt")

;; Should we return a shutdown thunk?
(define (serve port-no)
  ;; Create a rendezvous socket associated with the given port-no
  ;; which can queue up to 5 clients before we get to them.
  (define listener (tcp-listen port-no 5 #t))
  ;; Define an infinite loop of accepting and handling clients
  (define (loop)
    (accept-and-handle listener)
    (loop) )
  ;; Start that loop!
  (loop) )

;; Accept and Handle a new client from the Rendezvous Socket
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

;; Get game information then play the game
(define (handle in out)
  (with-handlers ([exn:fail? (λ (e) (send-string out 'expected (exn-message e))
                                    (close-input-port in)
                                    (close-output-port out) )])
    (let ([ss (read in)])
      (unless (game? ss)
        (error (format "expected game structure instead of: ~s" ss)) )
      (let* ( [player (game-player ss)]
              [min (game-min ss)]
              [max (game-max ss)]
              [target (+ min (random (- max min)))] )
        (play-game in out player min max target) ) ) ) )

(define (play-game in out player min max target)
  (with-handlers ([exn:fail? (λ (e) (send-string out 'expected (exn-message e))
                               (close-input-port in)
                               (close-output-port out) )])
    (let ( [ss (read in)] )
      (unless (guess? ss)
        (error (format "expected guess structure instead of: ~s" ss)) )
      (let ( [n (guess-number ss)] )
        (write (feedback (cond ([< n min] '!)
                               ([>= n max] '!)
                               ([< n target] '<)
                               ([> n target] '>)
                               (#t '=) )) out ) ) ) )
  (when (not (= n target))
    (play-game in out player min max target) ) )
