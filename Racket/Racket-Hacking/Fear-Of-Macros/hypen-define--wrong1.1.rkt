#lang racket/base

(require (for-syntax racket/base))

(define-syntax (hyphen-define/wrong1.1 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (let ([name (string->symbol (format "~a-~a" #'a #'b))])
         #'(define (name args ...)
             body0 body ...))]))

(hyphen-define/wrong1.1 foo bar () #t)

 (foo-bar)