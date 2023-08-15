#lang racket

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

;; Handle this client as we wish!
(define (handle in out)
  (with-handlers ([exn:fail? (λ (e) (send-string out 'expected (exn-message e))
                                    (close-input-port in)
                                    (close-output-port out) )])
    (send-string out 'hello-from "game-serve-1")
    (send-tag out 'request-hello-from)
    (let-values ( [[tag user-name] (recv-string in)] )
      (expect tag 'hello-from)
    (when (not (eq tag 'user-name)) (expected 'user-name)
    )
)
