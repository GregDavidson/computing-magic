#lang racket

(define (http-get resource #:port [port-no 80] #:host [host "localhost"])
  ;; should tcp-connect fail, it will throw an exn:fail:network structure
  (with-handlers ( [exn:fail:network? (λ (e) (printf "Could not connect\n"))] )
    (let-values ( [(in out) (tcp-connect host port-no)] )
      (fprintf out "GET ~a HTTP/1.1\r\nHost: ~a\r\n\r\n"
                   resource host )
      (flush-output out)
      (define (loop)
        (let ( [line (read-line in)] )
             (unless (eof-object? line)
               (printf "~a\n" line)
               (loop) ) ) )
      (loop) ) ) )
