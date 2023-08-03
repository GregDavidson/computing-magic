#lang racket

;; Some of the basics here are well-covered in the Racket Guide
;; Read some of: https://docs.racket-lang.org/guide/i_o.html
;; and a little bit of https://docs.racket-lang.org/guide/regexp.html

;; Make sure you've looked up each of the library procedures we use
;; here in the Reference Manual!  You can use F1 from DrRacket.

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
;; What do regexp-match and display do?
(define (handle in out)
  ;; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))
