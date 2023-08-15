#lang racket

;; * An Example Web Server

;; ** Introduction

;; This example solution is an alternative
;; to the last example in the Tutorial
;; More: Systems Programming with Racket
;; https://docs.racket-lang.org/more/

;; This is still intended to demonstrate *general*
;; systems and application programming techniques,
;; including how to create a framework.

;; If you're specifically interested in creating web servers,
;; see this tutorial using Racket's web-server framework:
;; https://docs.racket-lang.org/continue/

;; Comments beginning with a space and one or more asterisks
;; structure this program into Multi-Level Sections which
;; may help structure the code and are also used by Emacs for
;; code (un)folding if you load, e.g. =Outshine= Mode.

;; ** How to Run and Test the Server

;; Run the server at a Racket REPL by calling procedure =serve=
;; with an unbound /port number/, e.g.

#; (serve 8080)

;; After the server starts with no errors, run a test request
;; from a Web Browser with a suitable url, e.g.

;; http://localhost:8080/hello

;; Note that ports may stay bound for a little while after a program using them
;; quits, so you may sometimes want to use a different port number, e.g. 8081,
;; 8082, etc.

;; ** Packages Required

(require xml net/url racket/control)

;; ** Network Boilerplate

;; Create
;; - custodian to manage server resources
;; - rendezvous socket
;; Start
;; - service loop calling accept-and-handle
;; Return
;; - procedure to shut down the server
(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ( [current-custodian main-cust] )
    (define listener (tcp-listen port-no 5 #t))
    (define (loop) (accept-and-handle listener)
                   (loop) )
    (thread loop) )
  (λ ()
    (custodian-shutdown-all main-cust) ) )

;; Create
;; - custodian to manage client-associated resources
;; - client connection as input & output ports
;; - error handler to abort client on error
;; - timeout thread to abort client after max-time
;; Delegate
;; - client connection to procedure handle-client
(define (accept-and-handle listener [max-seconds 10] [max-bytes (* 50 1024 1024)])
  (define cust (make-custodian))
  (custodian-limit-memory cust max-bytes)
  (parameterize ( [current-custodian cust] )
    (define-values (in out) (tcp-accept listener))
    (thread (λ ()
              (with-handlers            ; catch any thrown exceptions
                ([exn:fail?             ; error thrown while handling client?
                   (λ (e)               ; call this to handle it
                     (eprintf "error: ~a\n" (exn-message e) )
                     (custodian-shutdown-all cust) ) ])
                (handle-client in out)
                (close-input-port in)
                (close-output-port out) ))) )
  (thread (λ () (sleep max-seconds)     ; timeout thread
            (custodian-shutdown-all cust) )) )

;; ** Utility Procedures

;; Given a regexp with exactly one parenthesized sub-expression
;; - throw an error if multiple matching sub-expressions!
;; - return the part of the target string which it matches
;; - return #f if match fails
(define (regexp-match-1 regexp target)
  (let ( [matches (regexp-match regexp target )] )
    (if (false? matches)
        #f                              ; no match at all!
        (let ( [num-matches (length matches)] )
          (cond [(< num-matches 2) #f]  ; no sub-expression match!
                [(> num-matches 2)      ; too many sub-expressions!
                 (error (format "regexp-match-1 bad regexp: ~a target: ~a\n"
                                regexp target )) ]
                ;; return what the sub-expression matched
                [#t (cadr matches)] ) ) ) ) )

;; [TODO] Check that there are no weird bytes!!
;; Trim off any leading or trailing [:/[:space:]] characters
;; Replace [[:space:]] sequences with a single regular space
(define (normalize-resource str)
  (let* ( [left-trimmed (regexp-replace #rx"^[:/[:space:]]*" str "")]
          [trimmed (regexp-replace #rx"[:/[:space:]]*$" left-trimmed "")] )
    (regexp-replace #rx"[[:space:]]+" trimmed " ") ) )

;; Read HTTP request line and return the
;; requested resource as a string,
;; "" if empty or error if malformed
(define (get-resource in)
  (let* ( [first-line (read-line in)]
          [regexp #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"]
          [matches (regexp-match-1 regexp first-line )] )
    (if (false? matches)
        (error (format "get-resource regexp: ~a first-line: ~a"
                       regexp first-line ) )
        (normalize-resource (cadr matches)) ) ) )

;; [TODO] Check that there are no weird bytes!!
;; Trim off any leading or trailing [[:space:]] characters
;; Replace [[:space:]] sequences with a single regular space
;; Replace first ": " with a single :
(define (normalize-headers str)
  (let* ( [left-trimmed (regexp-replace #rx"^[[:space:]]*" str "")]
          [trimmed (regexp-replace #rx"[[:space:]]*$" left-trimmed "")]
          [spaced (regexp-replace #rx"[[:space:]]+" trimmed " ")] )
    (regexp-replace #rx": *" trimmed ":") ) )

;; get the HTTP headers
;; return them as an association list
(define (get-headers in [accum '()])
  (let ( [line (normalize-headers (read-line in))] )
    (if (not (non-empty-string? line))
          (reverse accum)               ; no more headers
          (let ( [matches (regexp-replace #rx"^([^:]+):(.+)$" line)] )
            (if (or (false? matches) (not (= 3 (length matches))))
                (error (format "get-headers line: ~a"))
                (get-headers (cons (cons (cadr matches) (caddr matches)))) ) ) ) ) )

(define (send-http-response out headers content
                            #:protocol [protocol "HTTP/1.0"]
                            #:code [code 200]
                            #:message [message "Okay"] )
  (fprintf out "~a ~a ~a\r\n" protocol code message) )

;; ** Procdure Handle-Client

(define (handle-client in out)
  (let ( [req (get-resource in)]
         [headers (get-headers in)] )
    (let-values ( [(code xexpr) (prompt (dispatch req headers))] )
      (send-http-response
       out #:code code
       '( ("Serve" "k") ("Content-Type" "text/html") )
       (xexpr->string xexpr) ) ) ) )

;; ** Dispatching 
;; *** Dispatch Protocol

;; Handler procedures currently have the interface
;; (query headers) -> xhtml
;; We need to return content & headers
;; [IDEA] (query headers) -> headers as association-list
;; where within headers you can have
;;     code -> the code to return
;;     message -> the message to return with the code
;;     xhtml -> xhtml content
;; when no xhtml key or code you must have one or more
;;     content -> string-content

;; Dictionaries are a kind of dictionary
;; which can use the generic dictionary operations

;; return headers augmented with any of the defaults
;; for which headers has no corresponding key
(define (merge-headers-defaults headers defaults)
  (if (null? defaults)
      headers
      (let* ( [rest (cdr defaults)]
              [first (car defaults)]
              [key (car first)]
              [match (assoc key headers)] )
        (merge-headers-defaults (if match headers (cons first headers)) rest) ) ) )

;; *** Dispatch Table

;; Pair url paths to handler procedures with
(define dispatch-table (make-hash))

;; Adding a new handler procedure without syntactic sugar

(hash-set! dispatch-table "hello"
           (λ (query headers) `(html (body "Hello, World!"))) )

;; *** Syntactic Sugar

;; nicer syntax for defining handler procedures
(define-syntax-rule (define-handler name body ...)
  (hash-set! dispatch-table (symbol->string 'name)
             (λ (query headers) body ...) ) )

;; Adding a new handler procedure with syntactic sugar

(define-handler goodbye
  '(html (body "Goodbye, come again!")) )

;; To see this after expansion, uncomment and evaluate:
#;(expand-once
 '(define-handler goodbye
    '(html (body "Goodbye, come again!")) ) )

;; *** (dispatch request headers)

;; Parameters:
;; - request: http request as string
;; - headers: http request headers as association (list of pairs)
;; Returns 2 values
;; - http integer reponse code
;; - content as an xhtml expression
;; - [TODO] response headers
(define (dispatch request [headers '()])
  (let* ( [url (string->url request)] ; Parse request as URL
          [path (map path/param-path (url-path url))] ; Extract the path part
          ;; Find handler from path's first element
          [handler (hash-ref dispatch-table (car path) #f)] )
    (if handler
        (values 200 (handler (url-query url)))
        (values 404
                `(html (head (title (string append "Error: " ,request " unknown!")))
                       (body
                        (h1 "Error")
                        (p (font ((color "red")) "Unknown page: " ,request ) ) ) ) ) ) ) )

;; ** Form Utilities and Examples

(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                        [value ""]))
                (input ([type "hidden"] [name "hidden"]
                        [value ,hidden]))
                (input ([type "submit"] [name "enter"]
                        [value "Enter"] )) ) ) ) )

(define (many query)
  ;; Create a page containing the form:
  (build-request-page "Number of greetings:" "/reply" "") )

(hash-set! dispatch-table "many" many)

(define (reply query)
  ;; Extract and use the form results:
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ( [i (in-range n)] )
                   " hello" ) )) )

(hash-set! dispatch-table "reply" reply)

;; ** Original Style Handlers

(define (sum query)
  (build-request-page "First number:" "/one" ""))

(define (one query)
  (build-request-page "Second number:"
                      "/two"
                      (cdr (assq 'number query))))

(define (two query)
  (let ([n (string->number (cdr (assq 'hidden query)))]
        [m (string->number (cdr (assq 'number query)))])
    `(html (body "The sum is " ,(number->string (+ m n))))))

(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)

;; ** Servlet Style with Continuations

;; Helper to grab a computation and generate a handler for it:

(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    (abort (mk-page (string-append "/" tag))) ) )

;; Helper to run the number-getting page via `send/suspend':

(define (get-number label)
  (define query
    ;; Generate a URL for the current computation:
    (send/suspend
     ;; Receive the computation-as-URL here:
     (λ (k-url)
       ;; Generate the query-page result for this connection.
       ;; Send the query result to the saved-computation URL:
       (build-request-page label k-url "") ) ) )
  ;; We arrive here later, in a new connection
  (string->number (cdr (assq 'number query))) )

;; ** Servlet Style Handlers

(define (sum2 query)
  (define m (get-number "First number:"))
  (define n (get-number "Second number:"))
  `(html (body "The sum is " ,(number->string (+ m n)))) )

(hash-set! dispatch-table "sum2" sum2)
