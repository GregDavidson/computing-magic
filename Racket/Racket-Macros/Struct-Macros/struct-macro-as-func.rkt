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
  (let ( [exp-val exp] )
    (printf "~a --> ~a\n" 'exp exp-val)
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

 ;; return truthy if k is an option name at the head of one of the lists, #f otherwise
 (define (struct/macro-special? k [specials struct/macro-special-options])
   (assq k specials) )
 
 ;; return colon-prefixed special option name for given flag, or #f if none
 (define (struct/macro-special flag [specials struct/macro-special-options])
   (ormap (λ (lst) (if (memq flag (cdr lst)) (car lst) #f)) specials) )

;; Given a keyword (possibly wrapped in a syntax object)
;; returns whether the spelling of that keyword object ends in a : (colon).
;; - deprecated feature: also works on strings
(define (struct/macro-has-colon-suffix? k)
  (cond [(syntax? k) (struct/macro-has-colon-suffix? (syntax-e k))]
        [(keyword? k) (struct/macro-has-colon-suffix? (keyword->string k))]
        [(string? k) (char=? #\: (string-ref k (sub1 (string-length k))))]
        [else (error #f "unknown value ~a" k)] ) )

;; Given a keyword (possibly wrapped in a syntax object)
;; which does NOT have a colon suffix
;; returns the same type but with the keyword now sporting a colon suffix.
;; - deprecated feature: also works on strings
(define (struct/macro-add-colon-suffix k)
  (unless (not (struct/macro-has-colon-suffix? k)) (error #f  "expected plain keyword, got ~a" k))
  (cond [(syntax? k) (syntax (struct/macro-add-colon-suffix (syntax-e k)))]
        [(keyword? k) (string->keyword (struct/macro-add-colon-suffix (keyword->string k)))]
        [(string? k)
         (unless (not (struct/macro-has-colon-suffix? k)) (error #f  "expected plain flag, got ~a" k))
         (string-append k ":") ]
        [else (error #f "unknown value ~a" k)] ) )

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
;; return two values to use as the unabbreviated option name and value
;; both wrapped as syntax if flag was.
(define (struct/macro-flag-to-option-values flag)
  (let ( [k (struct/macro-keyword? flag)] )
    (unless (not (struct/macro-has-colon-suffix? k)) (error #f "expected plain flag, got ~a" flag))
    (let* ( [special (struct/macro-special k)]
            [option-name (or special (struct/macro-add-colon-suffix k))]
            [option-val (if special k #t)] )
      (printf "[flag '~a] [k '~a] [special '~a] [option-name '~a] [option-val '~a]\n"
              flag k special option-name option-val )
          (values (struct/macro-stx flag option-name)
                  (struct/macro-stx flag option-val) ) ) ) )

;; Given:
;; - stx a syntax item
;; - stxs a list of the syntax objects following stx
;; returns three values
;; 1. the struct/macro option pair at stx stxs
;; 2. a boolean value: is-special-option?
;; 3. the rest of the rest of list to process
;; Note: We're consuming 1 item from the givens if we have an abbreviated option
;;       otherwise we're consuming two items.
 (define (struct/macro-option-values stx stxs)
   (let ( [k (struct/macro-keyword? stx)] )
     (cond [(not k) (values stx #f #f stxs)]
           [ (not (struct/macro-has-colon-suffix? k))
             (let-values ( [(option-key option-value) (struct/macro-flag-to-option-values k)] )
               (struct/macro-option-values (struct/macro-stx stx option-key) ; for context
                                           (cons (struct/macro-stx stx option-value) stxs) ) ) ]
           [else (unless (pair? stxs) (error #f "option ~a needs value" k))
                 (values (struct/macro-stx stx (cons stx (car stxs)))
                         (not (not (struct/macro-special? k)))
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
 (check-false (struct/macro-special '#:mutable))
 
 (check-eq? (struct/macro-special '#:list) '#:scheme:)
 (check-eq? (struct/macro-special '#:racket/prefab) '#:racket:)
 (check-false (struct/macro-special '#:mutable))

 (check-equal? (evalues->list struct/macro-flag-to-option-values '#:list) '(#:scheme: #:list))
 (check-equal? (evalues->list struct/macro-flag-to-option-values '#:mutable) '(#:mutable: #t))

 (let* ( [key-1 '#:mutable]
         [key-2 '#:list]
         [key-3 '#:mutable:]
         [stxs-1 (list #'#t #'#:maybe #'#:more)]
         [stxs-3 (cdr stxs-1)] )
   (check-equal? (evalues->list struct/macro-option-values key-1 stxs-1) (list (cons '#:mutable:  #t) #f stxs-1))
   (check-equal? (evalues->list struct/macro-option-values key-2 stxs-1) (list (cons '#:scheme: '#:list)  #t stxs-1))
   ;; this one isn't checking: !!
   #;(check-equal? (evalues->list struct/macro-option-values key-3 stxs-1) (list (cons '#:mutable: #f) #f stxs-3))
   )
  
 )
                                     
;; ** struct parsing functions and struct/macro

;; *** TODO parse-field-spec

(maybe-begin-for-syntax

 ;; TODO Replace this with real code!!
 (define (struct/macro-expand-options stxs) stxs)
 
 ;; parse-field-spec takes a list of syntax objects specifying one field
 ;; (field-name contract {FIELD-OPTIONS}...?)
 ;; returns
 ;; - a syntax object specifying the field name
 ;; - a syntax object containing the rest of the field specification
 ;; Development feature you should not count on:
;;  if the stxs are not wrapped as syntax objects we will return regular values!
 (define (struct/macro-parse-field-spec stxs)
   (unless (and (list? stxs)
                (>= (length stxs) 2))
     (raise-syntax-error #f "expected structure field specification" stxs))
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
       (raise-syntax-error #f "invalid structure field specification" stxs) )
     (values field-name-stx (struct/macro-stx field-name-stx (cons contract-stx options-stx))) ) )

)
 
;; *** TODO parse-specs

(maybe-begin-for-syntax

 ;; TODO need to pull out the code for expand-options
 ;; and move it to the placeholder above!!
 
 ;; parse-specs takes
 ;; - start-stxs :: a list of syntax objects specifying a struct
 ;;   - this is used for context in errors
 ;; - stxs :: a list of syntax objects specifying a struct
 ;;   - this starts out eq? to start-stxs
 ;; and returns four syntax objects, containing:
 ;; - field-names (list of symbols)
 ;; - field-specs (list beginning with contract followed by options)
 ;; - special-option-pair/#f
 ;; - option-values (list of pairs - the car of which is a colon-suffixed keyword)
 ;; the lists within field-names and within field-specs correspond 1-1 and are non-empty
 ;; - we're currently only checking that field-specs are non-empty!!
 (define (parse-specs start-stxs stxs [field-names-rev '()] [field-specs-rev '()] [special-option #f] [options-rev '()])
   (define this 'parse-specs)
   (show this)
   (show field-names-rev)
   (show field-specs-rev)
   (show special-option)
   (show options-rev)
   ;; anything ending in -rev is a list of syntax objects in reverse order to their original appearance in stxs
   (if (null? stxs)
       ;; then return the four values, if valid
       (begin
         (unless (pair? field-specs-rev) (raise-syntax-error #f "expected field specification" start-stxs))
         (let* ( [field-names-stxs (reverse field-names-rev)] ; non-empty
                 [field-specs-stxs (reverse field-specs-rev)] ; non-empty
                 [options-stxs (reverse options-rev)] )
           ;; return the lists as syntax objects using
           ;; (datum->syntax context-syntax-object list-of-syntax-objects)
           (values (datum->syntax (car field-names-stxs) field-names-stxs)
                   (datum->syntax (car field-specs-stxs) field-specs-stxs)
                   special-option
                   (datum->syntax (car (if (pair? options-stxs) options-stxs start-stxs)) ; syntax context
                                  options-stxs ) ) ) )
       ;; else continue parsing the specification
       (let ( [stx-1 (car stxs)]       ; the 1st syntax object
              [stxs-1 (cdr stxs)] )    ; the rest of the spec (list of syntax objects)
         (printf "~a: ~a --> ~a\n" this 'stx-1 stx-1)
         (if (struct/macro-keyword? stx-1)
             (let-values ( [(option special? rest) (struct/macro-option-values stx-1 stxs-1)] )
               (if special?
                   (begin
                     (when special-option (raise-syntax-error #f "only 1 special option permitted" special-option stx-1 stxs))
                     (parse-specs stxs rest field-names-rev field-specs-rev option options-rev) )
                   (parse-specs stxs rest field-names-rev field-specs-rev special-option (cons option options-rev)) ) )
             ;; else, stx-1 "should" be a field specifier list - but is it?
             (let-values ( [(name-stx spec-stx) (struct/macro-parse-field-spec (syntax-e stx-1))] )
               ;; continue to accumulate syntax
               (parse-specs stxs stxs-1 (cons name-stx field-names-rev) (cons spec-stx field-specs-rev) special-option options-rev) ) ) ) ) )
 
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
 ;; - see (struct/macro-flag-to-option-values flag) above
 ;; special-option: option-pair
 ;; option-pair: ( option-name . option-value )
 ;; field-name: symbol
 ;; field-spec: list of ( contract field-option ...?  )
 ;; contract: type | predicate function | racket contract
 ;; field-option: tbd??
 (maybe-define-syntax (struct/macro stx)
                      ;; stx or anything ending in -stx is a syntax object
                      ;; stxs or anything ending in -stxs is a list of syntax objects
                      (let* ( [this 'struct/macro]
                              [args-stxs (cdr (syntax->list stx))]
                              [id-stx (car args-stxs)]
                              [spec-stxs (cdr args-stxs)] )
                        (unless (identifier id-stx)
                          (raise-syntax-error #f "expected structure id" stxs) )
                        (show this)
                        (show id-stx)
                        (show spec-stxs)
                        (let-values ( [(names specs special-option options) (parse-specs spec-stxs spec-stxs)] )
                          (if special-option
                              #`(struct/macro/ #,special-option #,id-stx #,names #,specs #,options)
                              #`(struct/macro/ #,id-stx #,names #,specs #,options) ) ) ) )

)

;; ** struct/macro/

;; Do we want struct/macro to separate out contracts as well???

(define-syntax (struct/macro/ stx)
  (let ( [this 'struct/macro/] )
    (printf "~a stx ~a\n" stx)
    (printf "~a form ~a\n" (syntax-e stx))
    #'(void)
    ) )

;; These examples should be automatically checked, yes???

;; This one is fine:
(maybe-for-testing
 (struct/macro #'(struct/macro foo [a integer?] [b string?]))
 )

;; This one reveals an error!!!
(maybe-for-testing
 (struct/macro #'(struct/macro foo #:list [a integer?] [b string?]))
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
