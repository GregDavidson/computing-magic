#lang racket/base

(require racket/stream)

(require (for-syntax
          racket/base
          racket/stream
          racket/syntax ))

(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     ;; ensure we've got symbols in all the right places!magit
     (for-each (λ (x) (unless (identifier? x)
                        (raise-syntax-error #f "not an identifier" stx x)) )
               (cons #'id (syntax->list #'(fields ...))) )
     (with-syntax ( [pred-id (format-id #'id "~a?" #'id)] )
       #`(begin
           (define (id fields ...) ; constructor
             (apply vector (cons 'id  (list fields ...))) )
           (define (pred-id v) ; predicate
             (and (vector? v) (eq? (vector-ref v 0) 'id)) )
           ;; Define an accessor for each field.
           #,@(for/list ( [x (syntax->list #'(fields ...))]
                          [n (in-naturals 1)] )
                (with-syntax ( [acc-id (format-id #'id "~a-~a" #'id x)]
                               [ix n] )
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id) )
                      (vector-ref v ix) ) ) ) ) ) ] ) )

(require rackunit)
(our-struct foo (a b))
(define s (foo 1 2))
(check-true (foo? s))
(check-false (foo? 1))
(check-equal? (foo-a s) 1)
(check-equal? (foo-b s) 2)
(check-exn exn:fail? (λ () (foo-a "furble")))