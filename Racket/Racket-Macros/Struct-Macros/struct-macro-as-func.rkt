#lang racket/base

;; * (struct/macro ...) development

;; We're going to develop our eventual macros
;; - i.e. syntax-phase macro transformers
;; as regular runtime functions for easier development.
;; Once they work, we'll shift them into syntax-phase.

;; Some of these functions are currently written to accept either
;; syntax objects or unwrapped expressions.  Once development is
;; complete, these will be macros which will only receive syntax
;; objects and the code allowing otherwise may go away without notice!

;; for tracing
(define-syntax-rule (show exp)
  (letrec (
           [exp-val exp]
           [bare (λ (e) (cond
                          [(syntax? e) (syntax-e e)]
                          [(list? e) (map bare e)]
                          [else e] ))]
           )
    (printf "~a --> ~a\n" (bare 'exp) (bare exp-val))
    exp-val ) )
 
;; ** Phase Control Meta-Macros

;; Allow us to write code which can either happen at
;; syntax-phase or runtime-phase

;; Needed now for meta-macros, likely needed anyway
;; when our runtime transformers become macros.
(require (for-syntax
          racket/base
          racket/syntax
          ) )

;; To control the phase, switch which macro is commented-out
;; in each of the following pairs.

#;(define-syntax (maybe-for-testing stx)
  (syntax-case stx ()
    [(_ args ...)
     #'(void) ]))

(define-syntax (maybe-for-testing stx)
  (syntax-case stx ()
    [(_ args ...)
     #`(begin args ...) ]))

(maybe-for-testing (require rackunit))

#;(define-syntax (maybe-require-for-syntax stx)
  (syntax-case stx ()
    [(_ args ...)
     #`(require (for-syntax args ...)) ]))

(define-syntax (maybe-require-for-syntax stx)
  (syntax-case stx ()
    [(_ args ...)
     #`(require args ...) ]))

#;(define-syntax (maybe-begin-for-syntax stx)
  (syntax-case stx ()
    [(_ args ...)
     #`(begin-for-syntax args ...) ]))

(define-syntax (maybe-begin-for-syntax stx)
  (syntax-case stx ()
    [(_ args ...)
     #`(begin args ...) ]))

#;(define-syntax (maybe-define-syntax stx)
  (syntax-case stx ()
    [(_ args ...)
     #`(define-syntax args ...) ]))

(define-syntax (maybe-define-syntax stx)
  (syntax-case stx ()
    [(_ args ...)
     #`(define args ...) ]))

;; ** Requirements, Key ideas, Global Lists

(maybe-require-for-syntax
          racket/stream
          racket/syntax
          racket/vector
          racket/contract )

;; *** Key ideas

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

;; *** Keywords and Options

(maybe-begin-for-syntax

;; be sure these all begin with struct/macro
 
 ;; A list of lists of keywords describing special struct/macro options.
 ;; These are the possible data-structure representations for a struct/macro,
 ;; so only one of these options will be allowed!
 ;; Each sublist begins with a colon prefixed option name
 ;; followed by the non-colon-prefixed flags which can both
 ;; abbreviate such and serve as their values.
 ;; All the keywords need to be unique!
 ;; ==> add checks for these things!!
 (define struct/macro-special-options
   '( (#:scheme: #:vector #:list #:record)
      (#:racket: #:racket/struct #:racket/contract #:racket/serializable #:racket/prefab #:racket/class)
      ) )

 ;; if argument is a keyword (possibly wrapped in a syntax object)
 ;; return that keyword (unwrapped); otherwise #f
 (define (struct/macro-keyword? k)
   (cond [(syntax? k) (struct/macro-keyword? (syntax-e k))]
         [(keyword? k) k]
         [else #f] ) )

 ;; return k if it is an option name at the head of one of the sublists, #f otherwise
 (define (struct/macro-special? k [sublists struct/macro-special-options])
   (if (syntax? k)
       (struct/macro-special? (syntax-e k) sublists)
       (let ( [sublist (assq k sublists)] )
         (if sublist (car sublist) #f) ) ) )
 
 ;; return colon-prefixed special option name for given flag, or #f if none
 (define (struct/macro-special-name flag [sublists struct/macro-special-options])
   (ormap
    (λ (sublist) (if (memq flag (cdr sublist)) (car sublist) #f))
    sublists ) )

;; Given a keyword (possibly wrapped in a syntax object)
;; returns whether the spelling of that keyword object ends in a : (colon).
;; - deprecated feature: also works on strings
(define (struct/macro-has-colon-suffix? k #:this [this 'struct/macro-has-colon-suffix?])
  (cond [(syntax? k) (struct/macro-has-colon-suffix? (syntax-e k))]
        [(keyword? k) (struct/macro-has-colon-suffix? (keyword->string k))]
        [(string? k) (char=? #\: (string-ref k (sub1 (string-length k))))]
        [else (raise-syntax-error this "unknown value ~a" k)] ) )

;; Given a keyword (possibly wrapped in a syntax object)
;; which does NOT have a colon suffix
;; returns the same type but with the keyword now sporting a colon suffix.
;; - deprecated feature: also works on strings
(define (struct/macro-add-colon-suffix k #:this [this 'struct/macro-add-colon-suffix])
  (unless (not (struct/macro-has-colon-suffix? k)) (raise-syntax-error this  "expected plain keyword, got ~a" k))
  (cond [(syntax? k) (syntax (struct/macro-add-colon-suffix (syntax-e k)))]
        [(keyword? k) (string->keyword (struct/macro-add-colon-suffix (keyword->string k)))]
        [(string? k)
         (unless (not (struct/macro-has-colon-suffix? k)) (raise-syntax-error this  "expected plain flag, got ~a" k))
         (string-append k ":") ]
        [else (raise-syntax-error this "unknown value ~a" k)] ) )

;; DEVELOPMENT FEATURE
;; an alternative to datum->syntax
;;   where if (1) exp is already syntax
;;      or if (2) stx isn't syntax
;;   we return exp as is.
;; Once development is complete, this may go away without notice!
(define (struct/macro-stx stx exp)
  (cond [(syntax? exp) exp]
        [(syntax? stx) (datum->syntax stx exp)]
        [else exp] ) )

;; Given a flag: a non-colon-suffixed keyword (possibly wrapped as syntax),
;; return an option-pair wrapped in the same syntax, if any.
(define (struct/macro-flag-to-option-pair
         flag                   ; a keyword without a : suffix
         #:check-special [check-special #f] ; should we consider special keywords?
         #:this [this 'struct/macro-flag-to-option-pair] )
  (let ( [k (struct/macro-keyword? flag)] )
    (unless (not (struct/macro-has-colon-suffix? k)) (raise-syntax-error this "expected plain flag, got ~a" flag))
    (let* (
           [special-name (and check-special (struct/macro-special-name k))]
           [option-name (or special-name (struct/macro-add-colon-suffix k))]
           [option-val (if special-name k #t)] )
      (eprintf "[flag '~a] [k '~a] [special-name '~a] [option-name '~a] [option-val '~a]\n"
               flag k special-name option-name option-val )
      (struct/macro-stx flag (cons option-name option-val)) ) ) )

;; Given:
;; - stx a syntax item
;; - stxs a list of the syntax objects following stx
;; returns three values
;; 1. the struct/macro option pair at stx stxs
;; 2. a special option-name keyword or #f if none
;; 3. the rest of the list to process
;; Note: We're consuming 1 item from the givens if we have an abbreviated option
;;       otherwise we're consuming two items.
 (define (struct/macro-option-values stx stxs #:this [this 'struct/macro-option-values])
   (let ( [k (struct/macro-keyword? stx)] )
     (unless k (raise-syntax-error this "expected keyword, not ~a" stx))
     (cond
       [(not (struct/macro-has-colon-suffix? k))
        (values (struct/macro-flag-to-option-pair stx #:check-special #t)
                (struct/macro-special-name k)
                stxs ) ]
       [else (unless (pair? stxs) (raise-syntax-error this "option ~a needs value" k))
             (values (struct/macro-stx stx (cons stx (car stxs)))
                     (struct/macro-special-name k)
                     (cdr stxs) ) ] ) ) )

)

(maybe-for-testing

 ;; Given
 ;;   p: a procedure which returns any number of values
 ;;   args: suitable arguments for p
 ;; Returns
 ;;   a list of the values p returned,
 ;;   with any that are syntax objects unwrapped
 (define (evalues->list p . args)
   (show p)
   (show args)
   (call-with-values (λ () (apply p args)) ; maybe producing multiple values
                     ;; convert all arguments into a list, unwrapping any syntax objects
                     (λ lst (map (λ (stx) (if (syntax? stx) (syntax-e stx) stx)) lst)) ) )
 
 (check-eq? (struct/macro-keyword? '#:list) '#:list)
 (check-eq? (struct/macro-keyword? #'#:list) '#:list)
 (check-false (struct/macro-keyword? 'list))

 (check-true (not (not (struct/macro-special? '#:scheme:))))
 (check-false (not (not (struct/macro-special? '#:list))))
 (check-false (struct/macro-special-name '#:mutable))
 
 (check-eq? (struct/macro-special-name '#:list) '#:scheme:)
 (check-eq? (struct/macro-special-name '#:racket/prefab) '#:racket:)
 (check-false (struct/macro-special-name '#:mutable))

 (check-equal? (struct/macro-flag-to-option-pair '#:list) '(#:scheme: . #:list))
 (check-equal? (struct/macro-flag-to-option-pair '#:mutable) '(#:mutable: . #t))

 (let* ( [key-1 '#:mutable]
         [key-2 '#:list]
         [key-3 '#:mutable:]
         [stxs-1 (list #'#t #'#:maybe #'#:more)]
         [stxs-3 (cdr stxs-1)] )
   (check-equal? (evalues->list struct/macro-option-values key-1 stxs-1) (list (cons '#:mutable:  #t) #f stxs-1))
   (check-equal? (evalues->list struct/macro-option-values key-2 stxs-1) (list (cons '#:scheme: '#:list)  '#:scheme: stxs-1))
   ;; this one isn't checking: !!
   #;(check-equal? (evalues->list struct/macro-option-values key-3 stxs-1) (list (cons '#:mutable: #f) #f stxs-3))
   )
  
 )
                                     
;; ** struct parsing functions and struct/macro

;; *** TODO parse-field-spec

(maybe-begin-for-syntax

 ;; Given a list of syntax-encoded options
 ;; Return a list of syntax-encoded option-pairs
 (define (struct/macro-expand-options stxs [option-pairs-rev '()] #:this [this 'struct/macro-expand-options])
   (if  (null? stxs)
        (reverse option-pairs-rev)
        (let-values ( [(option-pair is-special stxs-rest) (struct/macro-option-values (car stxs) (cdr stxs))] )
          (when is-special (raise-syntax-error this "unexpected special option at ~a" stxs))
          (struct/macro-expand-options stxs-rest (cons option-pair option-pairs-rev)) ) ) )
 
 ;; parse-field-spec takes a list of syntax objects specifying one field
 ;; (field-name contract {FIELD-OPTIONS}...?)
 ;; returns
 ;; - a syntax object specifying the field name
 ;; - a syntax object containing the rest of the field specification
 ;; Development feature you should not count on:
;;  if the stxs are not wrapped as syntax objects we will return regular values!
 (define (struct/macro-parse-field-spec stxs #:this [this 'struct/macro-parse-field-spec])
   (unless (and (list? stxs)
                (>= (length stxs) 2))
     (raise-syntax-error this "expected structure field specification" stxs))
   (let* (
          [field-name-stx (car stxs)]
          [field-name (if (syntax? field-name-stx) (syntax-e field-name-stx) field-name-stx)]
          [contract-stx (cadr stxs)]
          [contract (if (syntax? contract-stx) (syntax-e contract-stx) contract-stx)]
          [options-stx (struct/macro-expand-options (cddr stxs))] )
     (unless (and (symbol? field-name) ; scheme
                  #;(identifier field-name-stx) ; racket
                  ;; contract isn't bound, so these won't work:
                  #;(procedure? contract) ; scheme
                  #;(contract? (cadr stxs)) ; racket -- requires a provide!
                  )
       (raise-syntax-error this "invalid structure field specification" stxs) )
     (values field-name-stx (struct/macro-stx field-name-stx (cons contract-stx options-stx))) ) )

)
 
;; *** TODO parse-specs

(maybe-begin-for-syntax

 ;; parse-specs takes
 ;; - id-stx :: structure id stx for error context
 ;; - stxs :: a list of syntax objects specifying a struct
 ;; and returns four syntax objects, containing:
 ;; - field-names (list of symbols)
 ;; - field-specs (list beginning with contract followed by options)
 ;; - special-option-pair/#f
 ;; - option-values (list of pairs - the car of which is a colon-suffixed keyword)
 ;; the lists within field-names and within field-specs correspond 1-1 and are non-empty
 ;; - we're currently only checking that field-specs are non-empty!!
 (define (parse-specs id-stx stxs [field-names-rev '()] [field-specs-rev '()] [special-option #f] [options-rev '()] #:this [this 'parse-specs])
   (show this)
   (show id-stx)
   (show stxs)
   (show field-names-rev)
   (show field-specs-rev)
   (show special-option)
   (show options-rev)
   ;; anything ending in -rev is a list of syntax objects in reverse order to their original appearance in stxs
   (if (null? stxs)
       ;; then return the four values, if valid
       (begin
         ;; field-specs-rev musn't be empty as we require at least one field specification
         (unless (pair? field-specs-rev) (raise-syntax-error this "missing field specifications" id-stx))
         (let* ( [field-names-stxs (reverse field-names-rev)] ; non-empty
                 [field-specs-stxs (reverse field-specs-rev)] ; non-empty
                 [options-stxs (reverse options-rev)] )
           ;; return the lists as syntax objects
           (values (datum->syntax (car field-names-stxs) field-names-stxs)
                   (datum->syntax (car field-specs-stxs) field-specs-stxs)
                   special-option
                   (datum->syntax (if (pair? options-stxs) (car options-stxs) id-stx) ; syntax context
                                  options-stxs ) ) ) )
       ;; else continue parsing the specification
       (let ( [first-stx (car stxs)] 
              [rest-stxs (cdr stxs)] )
         (show first-stx)
         (show rest-stxs)
         (if (struct/macro-keyword? first-stx)
             (let-values ( [(option-pair maybe-special? more-stxs) (struct/macro-option-values first-stx rest-stxs)] )
               (if maybe-special?
                   (begin
                     (when special-option (raise-syntax-error this
                                                              "only 1 special option allowed for ~a, already have ~a, now seeing ~a"
                                                              id-stx special-option option-pair ))
                     (parse-specs id-stx more-stxs field-names-rev field-specs-rev option-pair options-rev #:this this) )
                   (parse-specs id-stx more-stxs field-names-rev field-specs-rev special-option (cons option-pair options-rev) #:this this) ) )
             ;; else, first-stx "should" be a field specifier list - but is it?
             (let-values ( [(name-stx spec-stx) (struct/macro-parse-field-spec (syntax-e first-stx))] )
               (parse-specs id-stx rest-stxs
                            (cons name-stx field-names-rev)
                            (cons spec-stx field-specs-rev)
                            special-option options-rev #:this this ) ) ) ) ) )
 
 )

(maybe-begin-for-syntax

 ;; *** TODO struct/macro

 ;; The specification of struct/macro may change to facilitate
 ;; the implementation of the downstream macros!
 
 ;; Input:: (struct/macro id {field-spec | option | flag} ...)
 ;; Output: (struct/macro/ { special-option }? id ( field-name ... ) ( field-spec ... } option-pair ...)
 ;; id: symbol
 ;; option: option-name option-value
 ;; option-name: keyword ending in :
 ;; option-value: symbol, keyword, boolean, tbd??
 ;; flag: a keyword NOT ending in : as an abbreviation for an option
 ;; - see (struct/macro-flag-to-option-pair flag) above
 ;; special-option: option-pair
 ;; option-pair: ( option-name . option-value )
 ;; field-name: symbol
 ;; field-spec: list of ( contract field-option ...?  )
 ;; contract: type | predicate function | racket contract
 ;; field-option: tbd??
 (maybe-define-syntax (struct/macro stx #:this [this 'struct/macro])
                      ;; stx or anything ending in -stx is a syntax object
                      ;; stxs or anything ending in -stxs is a list of syntax objects
                      (let*  ( [args-stxs (cdr (syntax->list stx))]
                               [id-stx (car args-stxs)]
                               [spec-stxs (cdr args-stxs)] )
                        (unless (identifier? id-stx)
                          (raise-syntax-error this "expected structure id" stx) )
                        (show this)
                        (show id-stx)
                        (show spec-stxs)
                        (let-values ( [(names specs special-option options) (parse-specs id-stx spec-stxs)] )
                          (if special-option
                              #`(struct/macro/ #,special-option #,id-stx #,names #,specs #,options)
                              #`(struct/macro/ #,id-stx #,names #,specs #,options) ) ) ) )

)

;; ** struct/macro/

;; Do we want struct/macro to separate out contracts as well???

(define-syntax (struct/macro/ stx #:this [this 'struct/macro/])
    (printf "~a stx ~a\n" stx)
    (printf "~a form ~a\n" (syntax-e stx))
    #'(void)
  )

;; These examples should be automatically checked, yes???

(maybe-for-testing
 (show (struct/macro #'(struct/macro foo [a integer?] [b string?])))
 )

(maybe-for-testing
 (show (struct/macro #'(struct/macro foo #:list [a integer?] [b string?])))
 )

;; ** Representations

;; *** Scheme Lists??

;; Shall we start with this or another one??

;; ** Generic Testing

#;(maybe-for-testing
 (struct/macro foo (a b))
 (define s (foo #f 1 2)) ; ctor
 (check-true (foo? s))
 (check-eq? (foo? s) (foo s #t))
 (check-eq? (foo? 1) (foo s #f))
 (check-=? (foo s a) 1)
 (check-=? (foo s b) 2)
 (check-equal? (call-with-values (λ () (foo s a b)) list) (list (foo s a) (foo s b)))
 (check-exn exn:fail? (λ () (foo "furble" a)))
 )
