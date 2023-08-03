#lang racket

;; Only the code in `accept-and-handle' changes from serve2.rkt

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop) (accept-and-handle listener)
                 (loop) )
  (define t (thread loop))
  (λ () (kill-thread t)
        (tcp-close listener) ) )

;; So that we can spend as much time as we like with each client,
;; without making other clients wait or be refused,
;; let's create a new thread for each new client!
(define (accept-and-handle listener)
  ;; wait until we get a client
  (define-values (in out) (tcp-accept listener))
  ;; we got one!  In a new thread let's
  ;; - handle the new client, taking as much time as we like
  ;; - then close our connections to that client when we're done
  ;; The optional call to sleep simulates taking random times
  ;; up to 10 seconds with each new client!
  (thread
   (λ () #;(sleep (random 10)) ; try uncommenting this!
        (handle in out)
        (close-input-port in)
        (close-output-port out))))

(define (handle in out)
  ;; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))
