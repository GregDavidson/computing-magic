#lang racket/base

(require rackunit)

(require (for-syntax
          racket/base ) )

(require
          racket/stream
          racket/syntax
          racket/vector
          racket/contract ) ; )

;; New ideas:
;; 0. All keyword "flags" are syntactic
;;    sugar for keyword "options" which
;;    are keywords ending in a : suffix.
;; 1. Transform "special flags" into
;;    the value of a special option,
;;    e.g. #:list --> (#:scheme: . #:list)
;; 2. Transform amy other flag #:foo
;;    into an option (#:foo: . #t)
;; 3. (struct/macro id ...) starts a transformation cascade:
;;    (struct/macro/ id (SPECIAL-OPTION: VALUE) (field-names...) (field-specs...) (options...))
;;    (struct/macro/SPECIAL-OPTION VALUE id  (field-names...) (field-specs...) (options...))
;;    which eventually leads to a macro which generates the macro which generates the
;;    macros and functions which implement the concrete representation of the protocol.
;; -- The intermediate macros are subject to tweaking if it factors out some of the common
;;    work of the descendant macros.

; (begin-for-syntax

(define struct/macro-special-flags
  '( (#:scheme: #:vector #:list #:record)
     (#:racket: #:racket/struct #:racket/contract #:racket/serializable #:racket/prefab #:racket/class)
   ) )

(define (struct/macro-special flag [specials struct/macro-special-flags])
  (ormap (λ (lst) (if (memq flag (cdr lst)) (car lst) #f)) specials) )

(check-equal? (struct/macro-special '#:list) '#:scheme:)
(check-equal? (struct/macro-special '#:racket/prefab) '#:racket:)
(check-false (struct/macro-special '#:mutable))

; )
                                     
;; All of this goes away shortly

; (begin-for-syntax

  (define (struct/macro-keyword? k ends-with-colon)
    (if (syntax? k)
        (struct/macro-keyword? (syntax-e k) ends-with-colon)
        (and (keyword? k)
             (let ( [s (keyword->string k)] )
               (eq? ends-with-colon (char=? #\: (string-ref s (- (string-length s) 1))) ) ) ) ) )
    
  (define (struct/macro-flag-type? key) (struct/macro-keyword? key #f))
  (define (struct/macro-option-type? key) (struct/macro-keyword? key #t))
 
  (define (struct/macro-flag-type: key)
    (unless (struct/macro-flag-type? key) (error #f "expected struct/macro flag" key))
    key )
   (define (struct/macro-option-type: key)
    (unless (struct/macro-option-type? key) (error #f "expected struct/macro option" key))
    key )
  
  (define (struct/macro-flags: flags) (map struct/macro-flag-type: flags))
  (define (struct/macro-options: options) (map struct/macro-option-type: options))
  
  ;; there may be exactly 1 representation-key in any struct/macro definition
  (define struct/macro-default-representation
    (struct/macro-flag-type: '#:scheme/record) )
  (define struct/macro-representations
    (struct/macro-flags:
     '(
       ;; standard scheme representations
       #:vector #:list #:record
       ;; racket representations
       #:racket/struct #:racket/contract #:racket/serializable #:racket/prefab #:racket/class
       ;; other representations might be added, e.g. dictionaries, et al.
       ) ) )
  (define struct/macro-field-flags (struct/macro-flags: '(#:mutable)))
  (define struct/macro-flags (struct/macro-flags: (append struct/macro-field-flags
                                                          struct/macro-representations
                                                          '(#:transparent) )))
  
  (define struct/macro-field-options (struct/macro-options:'() ))
  (define struct/macro-options (struct/macro-options: (append struct/macro-field-options
                                                              '(#:super: #:alias: #:ctor:) )))
  ;; assert??
  ;; - (element-of struct/macro-default-representation struct/macro-representations)
  ;; - (subset struct/macro-field-flags struct/macro-flags)
  ;; - (subset struct/macro-representations struct/macro-flags)
  ;; - (subset struct/macro-field-options struct/macro-options) ; should both exist
  ; )

;; A struct/macro form expands to a struct/macro/ form
;; A struct/macro form must match
;; (struct/macro id {field-spec|option-keyword|option-keyword value}...)
;; - there must be at least one field-spec
;; - elements after id may be in any order
;; - option-keyword names must end with a : iff they take a value
;; a field-spec must match (id contract field-option...)
;; - we check that
;;   - id is a symbol
;;   - some expression is in the contract position
;; - we do NOT check
;;   - the contract is a valid racket contract
;;   - the field options are valid -- see parse-field-spec
;; EXPANSION TO AN INTERMEDIATE FORM (not to be written by humans)
;; (struct/macro/ identifier (field-name...) (field-spec...) (option-keyword...) ({option-keyword value}...))
;; - the arguments are in this fixed order
;; - the field names have been separated from the field specifications
(define #;-syntax (struct/macro stx)
  ;; stx or anything ending in -stx is a syntax object
  ;; stxs or anything ending in -stxs is a list of syntax objects
  (let* ( [this 'struct/macro]
          [args-stxs (cdr (syntax->list stx))]
          [id-stx (car args-stxs)]
          [spec-stxs (cdr args-stxs)] )
    (printf "~a: ~a --> ~a\n" this 'id-stx id-stx)
    (printf "~a: ~a --> ~a\n" this 'spec-stx spec-stxs)
    ;; parse-field-spec takes a list of syntax objects specifying one field
    ;; returns
    ;; - a syntax object specifying the field name
    ;; - a syntax object containing the rest of the field specification
    (define (parse-field-spec stxs)
      (define this 'parse-field-specs)
      ;; stxs must be a non-empty list
      (unless (and (pair? stxs) (list? stxs)) (raise-syntax-error #f "unknown struct specification" stx stxs))
      (let ( [stx-1 (car stxs)]
             [stxs-1 (cdr stxs)] )
        (unless (identifier? stx-1) (raise-syntax-error #f "expected field name" stx stxs))
        (unless (pair? stxs-1) (raise-syntax-error #f "expected field contract" stx stxs))
        (values stx-1 (datum->syntax stx-1 stxs-1)) ) )
    ;; parse takes
    ;; - a list of syntax objects specifying a struct
    ;; and returns four syntax objects containing lists; let's call them
    ;; - field-names field-specs options option-value-pairs
    ;; the lists within field-names and within field-specs correspond 1-1 and are non-empty
    ;; - the list elements within field-names must be identifiers
    ;; - the field-specs should start with a contract possibly followed by additional optional elements
    ;; - we're currently only checking that there's at least one element following the field name
    ;; the lists within the option objects might be empty
    (define (parse-specs stxs [field-names-rev '()] [field-specs-rev '()] [flags-rev '()] [options-rev '()])
      (define this 'parse-specs)
      (printf "~a: ~a~a ~a~a ~a~a  ~a~a\n" this 'names field-names-rev 'specs field-specs-rev 'flags flags-rev 'options options-rev)
      ;; anything ending in -rev is a list of syntax objects in reverse order to their original appearance in stxs
      (if (null? stxs)
          ;; then return the four values, if valid
          (begin
            (unless (pair? field-specs-rev) (raise-syntax-error #f "expected field specification" stx))
            (let* ( [field-names-stxs (reverse field-names-rev)] ; non-empty
                    [field-specs-stxs (reverse field-specs-rev)] ; non-empty
                    [flags-stxs (reverse flags-rev)]
                    [options-stxs (reverse flags-rev)] )
              ;; return the lists as syntax objects using
              ;; (datum->syntax context-syntax-object list-of-syntax-objects)
              (values (datum->syntax (car field-names-stxs) field-names-stxs)
                      (datum->syntax (car field-specs-stxs) field-specs-stxs)
                      (datum->syntax (if (pair? flags-stxs) (car flags-stxs) id-stx) flags-stxs)
                      (datum->syntax (if (pair? options-stxs) (car options-stxs) id-stx) options-stxs) ) ) )
          ;; else continue parsing the specification
          (let ( [stx-1 (car stxs)]       ; the 1st syntax object
                 [stxs-1 (cdr stxs)] )    ; the rest of the spec (list of syntax objects)
            (printf "~a: ~a --> ~a\n" this 'stx-1 stx-1)
            (cond [(struct/macro-flag-type? stx-1)
                   ;; then continue with this flag
                   (parse-specs stxs-1 field-names-rev field-specs-rev (cons stx-1 flags-rev) options-rev) ]
                  [(struct/macro-option-type? stx-1)
                   (unless (pair? stxs-1) (raise-syntax-error #f "missing option value" stx stx-1 stxs-1))
                   (let ( [stx-2 (car stxs-1)] ; the option value?
                          [stxs-2 (cdr stxs-1)] ) ; the rest of the spec
                     ;; currently all valid option-values are identifiers
                     (unless (identifier? stx-2) (raise-syntax-error #f "expected option + identifier" stx stx-1 stx-2))
                     (parse-specs stxs-2 field-names-rev field-specs-rev flags-rev (cons (cons stx-1 stx-2) options-rev)) ) ]
                  [else ; it "should" be a field specifier list - but is it?
                   (let-values ( [(name-stx spec-stx) (parse-field-spec (syntax-e stx-1))] )
                     ;; continue to accumulate syntax
                     (parse-specs stxs-1 (cons name-stx field-names-rev) (cons spec-stx field-specs-rev) flags-rev options-rev) ) ] ) ) ) )
    (let-values ( [(names specs flags options) (parse-specs spec-stxs)] )
      #`(struct/macro/ #,id-stx #,names #,specs #,flags #,options) ) ) )

;; Do we want struct/macro to separate out contracts as well???
;; Do we want struct/macro to put the unique representation type option before the other options???

(define-syntax (struct/macro/ stx)
  (let ( [this 'struct/macro/] )
    (printf "~a stx ~a\n" stx)
    (printf "~a form ~a\n" (syntax-e stx))
    #'(void)
    ) )

#;(struct/macro foo [a integer?] [b string?])

(struct/macro #'(struct/macro foo [a integer?] [b string?]))

#|
#; (struct/macro foo (a b))
#; (define s (foo #f 1 2)) ; ctor
#; (check-true (foo? s))
#; (check-equal? (check-true (foo? s)) (check-true (foo s #t)))
#; (check-equal? (check-false (foo? 1)) (check-true (foo s #f)))
#; (check-equal? (foo s a) 1)
#; (check-equal? (foo s b) 2)
#; (check-equal? (call-with-values (λ () (foo s a b)) list) (list (foo s a) (foo s b)))
#; (check-exn exn:fail? (λ () (foo "furble" a)))
|#