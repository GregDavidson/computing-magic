;; #lang racket
#lang racket/base

;; ** Requires we need for testing only

(require racket/contract)
(require racket/stream)
;; https://docs.racket-lang.org/syntax/Minimal_Library.html
(require syntax/parse/pre)

;; ** for-syntax includes we might need

(require (for-syntax
          racket/base
          syntax/parse
          racket/function
          racket/stream
          racket/vector
          syntax/parse
          ))

;; ** Helpers we might need

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


;; ** struct/macro/predicate

;; We need to define a predicate procedure for our structure type
;; - so we don't have to open-code it all over the place
;; - and so it can be used in contracts

#;(define-syntax (struct/macro/predicate stx)
  (syntax-case stx ()
    [(_ id fields . config)
     (identifier? #'id) ; racket-specific??
     (datum->syntax stx `(define (,(string->symbol (string-append (symbol->string (syntax->datum #'id)) "?")) x)
                           (and (vector? x) (>= (vector-length x) ,(length (syntax->list #'fields)))) )) ] ) )



(define-syntax (struct/macro/predicate stx)
  (define (predicate-name stx)
    (string->symbol (string-append (symbol->string (syntax->datum #'id)) "?")) )
  (syntax-case stx ()
    [(_ id fields . config)
     (identifier? #'id) ; racket-specific??
     #;(list? #'fields) ; okay??
     #`(define (#,(predicate-name #'id) x)
                           (and (vector? x) (>= (vector-length x) #,(length (syntax->list #'fields)))) ) ] ) )

(syntax->datum (expand-once #'(struct/macro/predicate foobar (foo bar))))

;; ** struct/macro/define

;; This will be the fancy macro, recognizing
;; - predicate syntaxes
;; - constructor syntaxes
;; - selector syntaxes
;; - mutator syntaxes where applicable


;; Research & fix:
;: Local functions aren't being found!!
;; Put them in a module and import them
;; for syntax??

#;(define-syntax (struct/macro/define stx0)
  (syntax-case stx0 ()
    [(_ id fields . config)
     (identifier? #'id) ; racket-specific??
     #;(list? #'fields) ; okay??
     #`(define-syntax (#,(syntax->datum #'id) stx)
                         (define (predicate-name stx)
                           (string->symbol (string-append (symbol->string (syntax->datum #'id)) "?")) )
                         (define (name->index stx)
                           (let ( [goal (syntax->datum stx)] )
                             (let loop ( [flist (syntax->datum #'fields)]
                                         [i 0] )
                               (cond [(not (pair? flist)) (error #'id "bad field list ~a" #'fields)]
                                     [(or (eq? (car flist) goal)
                                          (and (pair? (car f)) (eq? (caar f) goal)) ) i]
                                     [else (loop (cdr flist) (+ 1 i))] ) ) ) )
                         (syntax-case stx ()
                           [(_ #:new . vals) (let ([v (vector-immutable . #'vals)])
                                               (unless ('#,(predicate-name #'id) v)
                                                 (error #'id "expected a ~a, got ~a" #'id v) )
                                               v) ]
                           [(_ vec field)
                            (identifier? field)
                            (let ([v vec])
                              (unless ('#,(predicate-name #'id) v)
                                (error #'id "expected a ~a, got ~a" #'id v) )
                              (vector-index v #,(name->index #'field)) ) ] ) ) ] ) )

;; ** struct/macro

;; ** struct/macro plan
;; - define hidden predicate function
;; - use hidden predicate to
;;   - implement macro predicate
;;   - check ctor validity with a value contract on the vector
;; - check mutations with a value contract on proposed new value

(define-syntax (struct/macro stx)
  (syntax-case stx ()
    [(_ id fields . config)
     (identifier? #'id) ; racket-specific??
     #;(list? #'fields) ; okay??
     #'(begin (struct/macro/predicate id fields . config)
            (struct/macro/define id fields . config) ) ] ) )
 
(syntax->datum #'(struct/macro foobar (foo bar)))
 
(syntax->datum (expand-once #'(struct/macro foobar (foo bar))))

;; ** Racket Source References

;; https://github.com/racket/racket/blob/master/racket/collracket/collects/racket/private/define-struct.rkt
;; racket/collects/racket/private/kernstruct.rkt
;; racket/collects/racket/private/struct.rkt
;; racket/collects/racket/private/struct.rkt
       
;; ** Failed Experiments

#;(define-syntax (struct/macro stx0)
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
