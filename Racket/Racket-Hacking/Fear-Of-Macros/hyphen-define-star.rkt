#lang racket/base

(require (for-syntax
          racket/base
          racket/string
          racket/syntax ))

(define-syntax (hyphen-define* stx)
    (syntax-case stx ()
      [(_ (names ...) (args ...) body0 body ...)
       (let ( [name-stxs (syntax->list #'(names ...))] )
         (with-syntax ( [name (datum->syntax
                               (car name-stxs) ; lexical context
                               (string->symbol
                                (string-join
                                 (map (λ (s) (symbol->string (syntax-e s)))
                                      name-stxs )
                                 "-" ) ) )] )
           #'(define (name args ...)
               body0 body ...) ) ) ] ) )

(hyphen-define* (foo bar baz) (v) (* 2 v))

(foo-bar-baz 50)