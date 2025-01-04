;; #lang racket
#lang racket/base

(require  syntax/parse
          racket/function
          racket/stream
          racket/vector
          syntax/parse
          racket/contract
          )

(provide syntax-case
         identifier?
         quasisyntax
         syntax
         )

;; Racket Macro Support
;; (require (for-syntax "racket-macro-support.rkt"))

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

(define (predicate-name stx)
  (string->symbol (string-append (symbol->string (syntax->datum #'id)) "?")) )

;; Get this modular!
#;(define (name->index stx)
    (let ( [goal (syntax->datum stx)] )
      (let loop ( [flist (syntax->datum #'fields)]
                  [i 0] )
        (cond [(not (pair? flist)) (error #'id "bad field list ~a" #'fields)]
              [(or (eq? (car flist) goal)
                   (and (pair? (car f)) (eq? (caar f) goal)) ) i]
              [else (loop (cdr flist) (+ 1 i))] ) ) ) )
