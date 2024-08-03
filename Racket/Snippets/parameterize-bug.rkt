#lang racket/base

;; fix the problems with the make-parameter guard
;; - ensure it is applied to the initial value
;; - ensure it returns the value it has checked
(define (my-parameter value guard name [check-name #f])
  (define (string x) (format "~a" x))
  (define (check v)
    (unless (guard v)
      (raise-argument-error (string name)
                            (string (or check-name (object-name guard) 'check))
                            v ) )
    v )
  (make-parameter (check value) check name) )

(define *host* (my-parameter "ngender.net" string? 'host))

(eprintf "[0] host ~a\n" (*host*))

(parameterize ( [*host* "hello"] )
  (eprintf "[1] host ~a\n" (*host*))
  (*host* "hi")
  (eprintf "[2] host ~a\n" (*host*)) )
