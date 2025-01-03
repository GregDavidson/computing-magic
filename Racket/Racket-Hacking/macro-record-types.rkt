#lang racket
;;#lang racket/base
(require racket/contract)
(require racket/stream)

(define/contract (stream-andmap-2 f seq0 seq1)
  (-> procedure? stream? stream? boolean?)
  (define this 'stream-andmap-2)
  (let loop ( [s0 seq0] [s1 seq1] )
    (cond [(and (null? s0) (null? s1)) #t]
          [(or (null? s0) (null? s1))
           (error this "irregular streams ~a and ~a" seq0 seq1) ]
          [(not (f (stream-first s0) (stream-first s1))) #f]
          [#t (loop (stream-rest s0) (stream-rest s1))] ) ) )

#;(stream-andmap-2 (λ (x y) (> x y)) '(3 2 5) '(2 0 4))

;; https://docs.racket-lang.org/syntax/stxparse-intro.html

(define-syntax foo
 (syntax-rules ()
    [(_ id) (let ([x 10]) id)]))

(syntax->datum (expand-once #'(foo m)))

(require (for-syntax syntax/parse racket/function))

;; https://docs.racket-lang.org/syntax/Minimal_Library.html
#;(require syntax/parse/pre)

(require syntax/parse)

;; a syntax class for describing a field
#;(define-syntax-class record-macro-field-tuple-syntax
  #:description "record macro field name"
  (pattern var:id) )

(syntax-parse #'(foo integer? #:mutable)
  [name:id (syntax->datum #'name)]
  [(name:id . attrs) (list (syntax->datum #'name) (syntax->datum #'attrs))]
  )

;; a syntax class for a list of fields
#;(define-syntax-class -record-macro-field-list)

;; FIX
;; - define hidden predicate function
;; - use hidden predicate to
;;   - implement macro predicate
;;   - check ctor validity with a value contract on the vector
;; - check mutations with a value contract on proposed new value

(define-syntax (struct/macro stx0)
  (syntax-parse stx0
    [(_ name-syntax:id field-syntax . feature-syntax)
     (let* ( [field-syntax-vec (list->vector (syntax->list #'field-syntax))]
             [record-features (syntax->datum #'feature-syntax)]
             [field-names (vector-map (λ (field)
                                        (syntax-parse field
                                          [field:id #'field]
                                          [(field:id . _) #'field] ) )
                                      field-syntax-vec )]
             [field-types (vector-map (λ (field)
                                        (syntax-parse field
                                          [field:id #f]
                                          [(field:id field-type . _) #'field-type] ) )
                                      field-syntax-vec )]
             [field-features (vector-map (λ (field)
                                           (syntax-parse field
                                             [field:id '()]
                                             [(field:id _ . features) (syntax->datum #'features)] ) )
                                         field-syntax-vec )]
             [field-mutable (vector-map (λ (features) (member '#:mutable  features)) field-features )]
             [all-mutable (member '#:mutable  record-features)]
             [vector-ctor (if (or all-mutable (ormap identity field-mutable))
                              #'vector #'vector-immutable )] )
       #'(define-syntax (#'name-syntax stx)
           [(_ #:new . stx-args)
            #:fail-unless (= (length (syntax->list #'stx-args)) (vector-length #'field-names))
            "Number of values must match number of fields"
            #'(let ( [this #'(format "~a:new" (syntax->datum #;name-syntax))]
                     [vec #'(cons vector-ctor (syntax->list #'stx-args))] )
                (unless (stream-andmap-2 (λ (pred val) (or (not pred) (pred val))) #'field-types #'vec)
                  (error this "bad type in ~a" vec) ) ) ]
           ) ) ] ) )

(syntax->datum (expand-once #'(struct/macro foobar (foo bar))))
