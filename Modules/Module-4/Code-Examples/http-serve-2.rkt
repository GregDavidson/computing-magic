#lang racket

;; The `serve' function is revised to run the loop
;; in a thread, and it returns a function to shut down
;; the server.

(define (serve port-no)
  ;; the server creates a TCP/IP socket using a port number
  ;; known to our clients.  Up to 5 clients can rendezvous
  ;; here before we have to start dropping them.
  (define listener (tcp-listen port-no 5 #t))
  ;; Create a function which will loop forever
  ;; accepting 1 client at a time from the rendezvous.
  (define (loop)
    (accept-and-handle listener)
    (loop))
  ;; Run the loop in a new thread of control, saving
  ;; a handle on the thread
  (define t (thread loop))
  ;; return a "shutdown" procedure which when called will
  ;; - destroy the thread
  ;; - close the rendezvous socket
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

;; The rest is the same as serve1.rkt

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (handle in out)
  ;; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))

;; How to use it:

;; Start the service:

#;(define stop-serve (serve 8081))

;; Our original thread returns to us allowing us
;; to evaluate more things.

;; meanwhile, the service will keep running as long as we like

;; might any clients fail to connect?

#; (stop-serve)   ; shutdown the service
