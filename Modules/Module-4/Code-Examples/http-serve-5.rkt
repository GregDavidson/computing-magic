#lang racket

;; New imports:
(require xml net/url)

;; No changes to `serve' or `accept-and-handle'...

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop) (accept-and-handle listener)
                   (loop) )
    (thread loop) )
  (λ () (custodian-shutdown-all main-cust)) )

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (λ () (handle in out)
                  (close-input-port in)
                  (close-output-port out) )) )
  ;; Watcher thread:
  (thread (λ () (sleep 10)
                (custodian-shutdown-all cust) )) )

;; The `handle' function now parses the request into `req', and it
;; calls the new `dispatch' function to get the response, which is an
;; xexpr instead of a string.

(define (handle in out)
  (define req
    ;; The first line from the browser will look like
    ;; GET *URL* HTTP/1.1
    ;; Match the first line to extract the *URL* part:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in) ) )
  ;; if match failed, req is #f
  ;; if it succeeded, req is a list of the matched strings
  (when req
    ;; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Dispatch:
    (let ( [xexpr (dispatch (cadr req))] )
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

;; New: the `dispatch' function and `dispatch-table':

(define (dispatch str-path)
  ;; Parse the request as a URL:
  (define url (string->url str-path))
  ;; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ;; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ;; Call a handler:
      (h (url-query url))
      ;; No handler found:
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))

;; A simple dispatcher:

(hash-set! dispatch-table "hello"
           (λ (query)
             `(html (body "Hello, World!"))))
