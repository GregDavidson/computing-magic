#lang racket/base

(require (for-syntax racket/base))

(define-syntax (hyphen-define/wrong1.2 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (syntax-case (datum->syntax #'a (string->symbol (format "~a-~a" #'a #'b))) ()
         [name #'(define (name args ...) body0 body ...)] ) ] ) )
 
(hyphen-define/wrong1.2 foo bar () #t)

(foo-bar)