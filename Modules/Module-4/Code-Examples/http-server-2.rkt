#lang racket

;; * An Example Web Server

;; ** Introduction

;; This is the second and more ambitious
;; alternative to the example servers in the tutorial
;; More: Systems Programming with Racket
;; https://docs.racket-lang.org/more/

;; See the README.org file (which should be in the
;; same directory as this source file) for the
;; context behind the design of this example.

;; This is still intended to demonstrate *general*
;; systems and application programming techniques,
;; including how to create a framework.

;; If you're specifically interested in creating web servers,
;; see this tutorial using Racket's web-server framework:
;; https://docs.racket-lang.org/continue/

;; Comments beginning with a space and one or more asterisks
;; structure this program into Multi-Level Sections which
;; may help structure the code and are also used by Emacs for
;; code (un)folding with Outshine Mode.

;; ** How to Run and Test the Server

;; If you'd like to see more of what's going on in the server,
;; remove the #; prefix from some of the (eprintf ...) lines.

;; Run the server at a Racket REPL by calling procedure =serve=
;; with an unbound /port number/, e.g.

#; (define stopper (serve 8080))

;; After the server starts with no errors, run a test request
;; from a Web Browser with a suitable url, e.g.

;; http://localhost:8080/hello

;; When you're done with the server, call

#; (stopper)

;; The server runs in a separate thread so the port will stay bound until you
;; call the stopper procedure. If something goes wrong and this doesn't happen
;; you can just use a different port, e.g. 8081, 8082, etc.

;; ** Required Packages

(require xml net/url racket/control)
(require racket/date)
(require pretty-format)                 ; only for debugging

;; ** Serving Clients

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
(define (accept-and-handle listener
                           [max-seconds 10]
                           [max-bytes (* 50 1024 1024)] )
  (define cust (make-custodian))
  (custodian-limit-memory cust max-bytes)
  (parameterize ( [current-custodian cust] )
    (define-values (in out) (tcp-accept listener))
    (eprintf "accept-and-handle: We got a client!\n")
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

;; [TODO] Allow handler to supply their own
;; - content as a string if they don't want to use xhtml
;; - response headers, status-code, status-string
(define (handle-client in out)
  (let ( [req (read-resource in)]
         [headers (read-headers in)] )
    (let-values ( [(code xexpr) (prompt (dispatch req headers))] )
      #;(pretty-eprintf "handle-client got code: ~s xexpr: ~s\n" code xexpr)
      (write-http-response
       out #:code code
       '()                    ; handler-supplied headers should go here!
       (xexpr->string xexpr) ) ) ) )

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
                 (error (format "regexp-match-1 bad regexp: ~s target: ~s"
                                regexp target )) ]
                ;; return what the sub-expression matched
                [#t (cadr matches)] ) ) ) ) )

;; ** HTTP Resource Line

;; HTTP Resources are on first line of HTTP Request
;; In the example: GET mouse/laboratory HTTP/1.0
;; - the HTTP Resource is: mouse/laboratory
;; - as that's the Resource the GET Request is requesting

;; [TODO] Remove this procedure?
;; - It may be redundant now that we're using
;; - string->url,url-path and path/param-path
;; from the Racket web framework in dispatch.
;;
;; [TODO] Check that there are no weird bytes!!
;; Trim off any leading or trailing [:/[:space:]] characters
;; Replace [[:space:]] sequences with a single regular space
(define (normalize-resource str)
  (string-trim str) ; remove whitespace from the edges
  #;(let* ( [left-trimmed (regexp-replace #rx"^[:/[:space:]]*" str "")]
          [trimmed (regexp-replace #rx"[\r:/[:space:]]*$" left-trimmed "")]
          [normalized (regexp-replace* #rx"[[:space:]]+" trimmed " ")] )
    (eprintf "normalize-resource str ~s left-trimmed ~s right-trimmed ~s normalized ~s\n"
             str left-trimmed trimmed normalized )
    normalized ) )

;; Read HTTP request line
;; return requested resource as a string,
;; return "" if none
;; raise error if malformed
(define (read-resource in)
  (let* ( [first-line (read-line in)]
          [regexp #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"]
          [match (regexp-match-1 regexp first-line )] )
    (if (false? match)
        (error (format "read-resource regexp: ~s first-line: ~s"
                       regexp first-line ) )
        (let ( [resource (normalize-resource match)] )
          (eprintf "read-resource returning ~s\n" resource)
          resource ) ) ) )

;; ** HTTP Headers

;; For now we'll represent headers as a list of
;; (header-key header-value) lists where
;; - header-key is a symbol normalized to lower case
;;   - should be on standard list of HTTP header keys
;; - header-value is a string -- should be LATIN-1 encoded
;; Note: This makes a list of headers into an Association List
;;       which can be searched using the assoc function!

(define default-headers '( [content-type "text/html; charset=utf-8"] ) )
;; Good headers to add
;; content-length: size of content in bytes
;; date: date and time we sent the content in RFC 9110 format
;; - e.g. Tue, 15 Nov 1994 08:12:31 GMT

;; [TODO] How should we handle illegal header keys??
;; - some of which might be illegal symbols??
(define (normalize-header-key str)
  (string->symbol (string-downcase (string-trim str))) )

;; [TODO] How should we handle
;; - Charset encoding issues??
;; - Multiple lines??
(define (normalize-header-value str)
   (string-trim str) )

;; Trim off any leading or trailing [[:space:]] characters
(define (normalize-headers str) (string-trim str) )

;; Read the HTTP headers
;; return them as an Association List
;; [TODO] how shall we deal with
;; - malformed headers??
;; - unknown header keys??
;; - duplicate header keys??
(define (read-headers in [accum '()])
  (let ( [line (normalize-headers (read-line in))] )
    (if (not (non-empty-string? line))
          (begin
            #;(pretty-eprintf "read-headers returning ~v\n" accum)
            (reverse accum)               ; no more headers
            )
          (let ( [matches (regexp-match #rx"^([^:]+):(.+)$" line)] )
            (if (or (false? matches) (not (= 3 (length matches))))
                (error (format "read-headers line: ~s ghe-matches: ~s" line matches))
                (let ( [key (normalize-header-key (cadr matches))]
                       [value (normalize-header-value (caddr matches))] )
                  #;(eprintf "read-headers line ~v\n" line)
                  (read-headers in (cons (list key value) accum)) ) ) ) ) ) )

(define (write-headers out headers)
  (if (null? headers)
      (display "\r\n" out)  ; blank line indicating end of headers
      (begin
        (fprintf out "~a: ~a\r\n" (caar headers) (cadar headers))
        (write-headers out (cdr headers)) ) ) )

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

;; ** Writing Responses

;; [TODO] What's a more efficient alternative??
(define (padded-digits num [min-digits 2])
  (let* ( [digits (number->string num)]
          [num-digits (string-length digits)] )
    (if (>= num-digits min-digits)
        digits
        (string-append (make-string (- min-digits num-digits) #\0) digits) ) ) )

;; [TODO] What's more efficient for the Weekdays and Months??
;; - consider (... #"SunMonTueWedThuFriSat")
;; returns date-and-time in RFC 9110 format
;; - e.g. Tue, 15 Nov 1994 08:12:31 GMT
(define (http-timestamp)
  (let ( [date (seconds->date (current-seconds) #f)] )
    (format "~a, ~a ~a ~a ~a:~a:~a GMT"
            (vector-ref #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") (date-week-day date))
            (date-day date)
            (vector-ref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (date-month date))
            (date-year date)
            (padded-digits (date-hour date))
            (padded-digits (date-minute date))
            (padded-digits (date-second date)) ) ) )

(define (write-http-response out headers content
                            #:protocol [protocol "HTTP/1.0"]
                            #:code [code 200]
                            #:message [message "Okay"] )
  (let* ( [content-bytes (string->bytes/utf-8 content)]
          [content-length (bytes-length content-bytes)]
          [with-content-length (cons (list 'content-length content-length) default-headers)]
          [with-date (cons (list 'date (http-timestamp)) with-content-length)] )
    #;(eprintf "write-http-response protocol ~s code ~s message ~s\n"
               protocol code message )
    #;(eprintf "write-http-response content ~s\n" content)
    (fprintf out "~a ~a ~a\r\n" protocol code message)
    ;; [todo] add date field to default-headers
    (write-headers out (merge-headers-defaults headers with-date))
    (display content-bytes out) ) )

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

;; *** Dispatch Table

;; Pair url paths to handler procedures with
(define dispatch-table (make-hash))

;; Adding a new handler procedure without syntactic sugar

(hash-set! dispatch-table "hello"
           (λ (query [headers '()])
             #;(eprintf "hello query: ~a\n" query)
             #;(eprintf "hello returning some xhtml now\n")
             `(html (body "Hello, World!")) ) )

;; *** Syntactic Sugar

;; nicer syntax for defining handler procedures
(define-syntax-rule (define-handler name body ...)
  (hash-set! dispatch-table (symbol->string 'name)
             (λ (query [headers '()]) body ...) ) )

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
  (eprintf "dispatch request ~s\n" request)
  #;(pretty-eprintf "dispatch headers ~s\n" headers)
  (let* ( [url (string->url request)] ; Parse request as URL
          [path (map path/param-path (url-path url))] ; Extract the path part
          ;; Find handler from path's first element
          [handler (hash-ref dispatch-table (car path) #f)] )
    (eprintf "dispatch url ~s\n" url)
    (eprintf "dispatch url-query ~s\n" (url-query url))
    (eprintf "dispatch path ~s\n" path)
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

;; Whats with tag ???
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
