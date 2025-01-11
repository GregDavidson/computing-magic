#lang racket/base

(require (for-syntax racket/base)
         (for-syntax syntax/transformer))

(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ expr)
     (let ([raw-expr (syntax->datum #'expr)]
           [x-id    (datum->syntax #f 'x)])  ; 'x' in #f context
       (datum->syntax
         stx
         `(let ([,(syntax-e x-id) 10])
            ,raw-expr)))]))
