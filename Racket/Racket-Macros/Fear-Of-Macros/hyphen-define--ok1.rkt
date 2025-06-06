#lang racket/base

(require (for-syntax racket/base))

(define-syntax (hyphen-define/ok1 stx)
    (syntax-case
        stx ()
      [(_ a b (args ...) body0 body ...)
       (syntax-case
           (datum->syntax
            #'a ; get syntax data (lexical context) from a
            (string->symbol (format "~a-~a"
                                    (syntax->datum #'a)
                                    (syntax->datum #'b) )) ) ()
         [name #'(define (name args ...) body0 body ...)] ) ] ) )
 
(hyphen-define/ok1 foo ok1 () #t)

(foo-ok1)

(define-syntax (hyphen-define/ok2 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (with-syntax ( [name (datum->syntax
                             #'a ; get syntax data (lexical context) from a
                             (string->symbol (format "~a-~a"
                                                     (syntax->datum #'a)
                                                     (syntax->datum #'b) )) )] )
         #'(define (name args ...) body0 body ...) ) ] ) )
 
(hyphen-define/ok2 foo ok2 () #t)

(foo-ok2)

(require (for-syntax racket/syntax))

(define-syntax (hyphen-define/ok3 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (with-syntax ( [name (format-id #'a "~a-~a" #'a #'b)] )
         #'(define (name args ...) body0 body ...) ) ] ) )

(hyphen-define/ok3 bar baz () #t)

(bar-baz)