#lang racket/base

(define *host* (make-parameter "ngender.net" string? 'host))

(define (go1 [host #f])
  (define this 'go1)
  (parameterize ( [*host* (or host (*host*))] )
    (eprintf "~a host ~a\n" this (*host*) ) ) )

(go1)
(go1 "hello")
