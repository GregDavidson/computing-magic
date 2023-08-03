#lang racket

;; A long-running program needs to avoid "leaking" resources.
;; Racket's automatic garbage collection avoids leaking memory.
;; There is no automatic reclaiming of other resources, e.g.
;; - Sockets, Ports, Threads, Custodians, et al.
;; Fortunately, all such things when created are associated
;; with the current-custodian which is dynamically bound
;; to that symbol.  Since new custodians are tracked by the
;; current-custodian, custodians form a resource tree!

(define (serve port-no)
  ;; Create a new custodian bound to main-cust.
  ;; It becomes a resource associated with the current custodian.
  (define main-cust (make-custodian))
  ;; Let main-cust temporarily become the current-custodian.
  (parameterize ( [current-custodian main-cust] )
    ;; the listener socket will be associated with main-cust
    (define listener (tcp-listen port-no 5 #t))
    (define (loop) (accept-and-handle listener)
                   (loop) )
    ;; the serve thread will be associated with main-cust
    (thread loop) )
  (λ ()
     ;; release all resources associated with main-cust
     ;; recursively, i.e. in sub-custodians as well.
     (custodian-shutdown-all main-cust) ) )

(define (accept-and-handle listener)
  ;; accept-and-handle doesn't know (or care)that the new custodian
  ;; cust will become associated with serve's main-cust custodian
  ;; since that is the current-custodian!
  (define cust (make-custodian))
  ;; Any further resources created in accept-and-handle or in
  ;; any procedure it calls will become associated with cust
  ;; which itself is associated with serve's main-cust.
  (parameterize ([current-custodian cust])
    ;; The in and out ports (byte stream connections)
    ;; will be associated with our new custodian cust.
    (define-values (in out) (tcp-accept listener))
    ;; The new thread will be associated with cust.
    (thread (λ () (handle in out)
                  ;; how else could we close these connections?
                  (close-input-port in)
                  (close-output-port out)) ) )
  ;; Watcher thread:
  ;; What if handle wanted more time with this client?
  ;; How could we alter the watcher design for that?
  (thread (λ () (sleep 10)
                (custodian-shutdown-all cust)) ) )

(define (handle in out)
  ;; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out) )

;; This will start the server
#;(define stop-serve (serve 8081))

;; This will stop it and release all resources!
#;(stop-serve)
