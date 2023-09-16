#lang racket

;; See this required module for ideas on how to evolve the game
(require "game-protocol-0.rkt")

(define our-min 0)   ; guess must be >= than this
(define our-max 100) ; guesses must be less that this
(define our-player "player")

(define (play host port-num)
  ;; Connect to a game server
  (with-handlers
      ( [exn:fail:network?
         (λ (e) (printf "no game server: ~n\n" (exn-message e))) ] )
    (let-values ( [(in out) (tcp-connect host port-num)] )
      (with-handlers
          ( [exn:fail?
             (λ (e) (printf "error: ~a\n" (exn-message e))
               (close-input-port in)
               (close-output-port out) ) ]
            [unexpected?
             (λ (e) (printf "Expected ~a, got: ~a\n" (unexpected-expected e) (unexpected-got e))
               (close-input-port in)
               (close-output-port out) ) ] )
        ;; Get game info from Player
        (set! our-player (get-name))
        ;; Send game info to Server
        (write-game (game our-player our-min our-max) out)
        ;; Play 1 game
        (play-game in out)
        (close-input-port in)
        (close-output-port out) ) ) ) )

(define (play-game in out)
  (let ( [n (get-guess our-min our-max)] )
    (write-guess (guess n) out)
    (let ( [f (feedback-message (read-feedback in))] )
      (printf "~a!\n" (case f
                        ((!) "Bad guess") ; shouldn't be possible!
                        ((<) (set! our-min (+ n 1)) "Too low")
                        ((>) (set! our-max n) "Too high")
                        ((=) "You guessed it") ))
      (unless (eq? '= f)
        (play-game in out) ) ) ) )

(define (read-nonempty-line)
  (let ( [line (read-line)] )
    (if (> (string-length line) 0)
        line
        (read-nonempty-line) ) ) )

(define (get-name)
  (display "What's your name: ")
  (read-nonempty-line) )

 (define (get-guess min max)
   (printf "What's your guess? [~a .. ~a) " min max)
   (let* ( [line (read-line)]
           [n (string->number line)] )
     (if (and (integer? n) (>= n min) (< n max))
         n
         (begin (printf "You entered ~v\n" line)
                (printf "Please enter an integer >= ~a and < ~a\n" min max)
                (get-guess min max) ) ) ) )
