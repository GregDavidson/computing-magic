#lang racket/base

(require racket/stream)

(require (for-syntax
          racket/base
          racket/stream
          racket/syntax
          racket/vector
          racket/contract ))

;; How do we make sure that v is not captured non-higienically?
(define-syntax (struct/flex stx)
  (syntax-case stx ()
    [(_ id #:vector (fields ...))
     (let ( [fspecs #'(fields ...)] )
       ;; ensure the syntax is good
       (unless (identifier? #'id) (raise-syntax-error #f "not an identifier" stx #'id))
       (for-each (λ (fs) (unless (list? fs) (raise-syntax-error #f "not a field specification list" stx x))) fspecs)
       (for-each (λ (fs) (unless (identifier? (car fs)) (raise-syntax-error #f "not a field specification identifier" stx x))) fspecs)
       (for-each (λ (fs) (unless (contract? (cadr fs)) (raise-syntax-error #f "not a field specification contract" stx x))) fspecs)
       ;; bind some regular variables
       (let ( [names-vec (for/vector ( [fspec fspecs] ) (car fspec))]
              [num-names (vector-length names-vec)]
              [contracts-vec (for/vector ( [fspec fspecs] ) (cadr fspec))] )
         ;; bind some pattern variables
         (with-syntax ( [pred-id (format-id #'id "~a?" #'id)]
                        [names-list (vector->list names-vec)]
                        [size num-names]
                        [pred-lambda `(λ (v) (and (vector? v) (>= num-names (vector-length v))
                                                  (stream-andmap (λ (i) ((vector-ref ,contracts-vec i) (vector-ref v i))) (in-range 0 num-names)) ))] )
           #`(begin
               ;; connstructor
               (define (id ,@names-list)
                 (let ( [v (vector #,@names-list)] )
                   (begin (unless (pred-id v) (error #f "not a ~a vector" id)) v) ) )
               ;; predicate
               (define pred-id pred-lambda)
               ;; Define an accessor for each field.
               #,@(for/list ( [x (syntax->list #,@names-list)]
                              [n (in-range 0 #,size)] )
                    (with-syntax ( [acc-id (format-id #'id "~a-~a" #'id x)]
                                   [ix n] )
                      #`(define (acc-id v)
                          (unless (pred-id v)
                            (error 'acc-id "~a is not a ~a struct" v 'id) )
                          (vector-ref v ix) ) ) ) ) ) ) ) ] ) )

(require rackunit)
(our-struct foo (a b))
(define s (foo 1 2))
(check-true (foo? s))
(check-false (foo? 1))
(check-equal? (foo-a s) 1)
(check-equal? (foo-b s) 2)
(check-exn exn:fail? (λ () (foo-a "furble")))