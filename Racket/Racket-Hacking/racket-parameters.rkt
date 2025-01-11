#lang racket

;; ** Racket Parameters

;; Racket has a built-in feature for parameters
;; https://docs.racket-lang.org/reference/parameters.html

;; It would be nice if they were
;; - Write once
;; - Had a trace feature

;; Let's try to write something better!

;; ** Simple Param

;; We're using the term parameter to refer to a named value
;; which should be set once and should not change thereafter.
;; ---> This is NOT what Racket calls parameters!

;; These are handy for parameters we might fetch from the
;; command line or for parameters we might receive from
;; the server when we initially connect to it.

;; We'll implement parameters as functions which yield
;; the value of the parameter when called with no arguments
;; and set the value of the parameter when called with one argument.
;; Our parameters will have names, a guard function and an optional
;; parsing function for converting an initialization value into a
;; proper value.

;; Returns a function which
;; - manages a single value (the parameter)
;; guard? : any/c -> boolean? - validates initializations
;; parse : any/c -> guard?    - parses raw value
;; trace : boolean?           - displays value when initialized
(define (simple-parm name guard?
                    #:parse [parse identity]
                    #:trace [trace #f] )
  (let ( [value (void)] [this 'parameter] )
    (case-lambda
      [() (when guard? (error this "~a not set" name))
          value ]
      [(arg) (let ( [v (parse arg)] )
               (unless guard? (error this "~a is set, rejecting ~a" name arg))
               (unless (guard? v) (error this "~a: guard rejects ~a" name v))
               (set! value v)
               (set! guard? #f) ; release any procedure reference
               (set! parse #f)  ; release any procedure reference
               (when trace (eprintf "~a ~a: ~a\n" this name value)) )] ) ) )

;; ** make-param+ version 1.0

;; Let's try making a fancier kind of parameter.

;; Parameters have a primary value called "the parameter"
;; Parameters may accompanied by "properties"
;; - arbitrary values indexed by unique symbols

;; The parameter and any properties are monotonic
;; - i.e. the parameter and any property can only be set once.
;; - i.e. they can be initialized but not mutated

;; The parameter always has
;; - a name for debugging
;; - a guard to validate initialization
;; Optionally we can set
;; - a parsing function to process the initialization value
;;   - only for the main value, not for property values
;; - an optional tracing feature to show initializations
;;   - parameter AND property initializations
;;   - written to the standard error stream

;; Syntax
;; - The returned function can accept multiple arguments
;; - Commands can be chained until one returns a value
;;   - no arguments - returns the main value
;;   - 'value - also returns the main value
;;   - 'init value - initializes the parameter with given value
;;     - throws an error if already initialized
;;     - value with be processed with any provided parse function
;;     - throws an error if violates guard? contract function
;;   - 'get key - returns value of parameter key
;;     - throws error if uninitialized
;;   - 'get key default - returns value of parameter key
;;     - returns default value if uninitialized
;;   - 'put key value - initializes parameter key with value
;;     - throws an error if already initialized
;; See the examples below!

;; Returns a function which
;; - manages a single value (the parameter)
;; Along with an optional list of properties
;; - properties are indexed by symbols
;; guard? : any/c -> boolean?
;; parse : any/c -> guard?
(define (make-param+ name guard?
                    #:parse [parse identity]
                    #:props [properties '()]
                    #:trace [trace #f] )
  (letrec ( [value 'tbd]
            [get-prop
             (λ (key default)
               (let ( [found (assq key properties)] )
                 (values found (if found (cdr found) default)) ) ) ]
            [return-value
             (λ ()
               (when guard? (error 'parameter "~a not set" name))
               value ) ]
            [show-value
             (λ () (if guard?
                       (eprintf "Parameter ~a not set\n" name)
                       (eprintf "Parameter ~a: ~a\n" name value) )) ]
            [show-prop
             (λ (key)
               (let-values ( [(found val) (get-prop key #f)] )
                 (if found
                     (eprintf "Parameter ~a property ~a: ~a\n" name key val)
                     (eprintf "Parameter ~a property ~a not set\n" name key) ) ) ) ]
            [parameter/process
             (λ args ; a function taking a list of 1 or more commands
               (define this 'parameter/process)
               (match args
                 [(list 'value) (return-value)]
                 [(cons 'init (cons arg more-args))
                  (let ( [v (parse arg)] )
                    (unless guard?
                      (error this "~a is set, rejecting ~a" name arg ) )
                    (unless (guard? v)
                      (error this "~a: guard rejects ~a" name v ) )
                    (set! value v)
                    (set! guard? #f) ; release any procedure reference
                    (set! parse #f)  ; release any procedure reference
                    (when trace (show-value)) )
                  (process-args more-args) ]
                 [(cons 'show more-args) (show-value) (process-args more-args)]
                 [(cons 'show-prop (cons key more-args)) (show-prop key) (process-args more-args)]
                 [(cons 'put (cons key (cons val more-args)))
                  (let-values ( [(found val0) (get-prop key #f)] )
                    (cond [(not found) (set! properties (cons (cons key val) properties))]
                          [(equal? val val0)]
                          [else (error this "~a already set to ~a, rejecting new value ~a" key val0 val)] )
                    (when trace (show-prop key)) )
                  (process-args more-args) ]
                 [(list 'get key)
                  (let-values ( [(found val) (get-prop key #f)] )
                    (if found val (error this "No key ~a" key)) ) ]
                 [(list 'get key default)
                  (let-values ( [(found val) (get-prop key #f)] )
                    (if found val default) ) ]
                 [args (error this "unknown args ~a" args)] ) ) ]
            [process-args
             (λ (args) (when (pair? args) (apply parameter/process args))) ]
            [the-parameter
             (λ args ; a function taking a list of 0 or more commands
               (if (null? args)
                   (return-value) ; default command
                   (process-args args) ) ) ] )
    the-parameter ) )

;; ** Example(s)

(define param1 (make-param+ 'param1 natural? #:parse string->number #:trace #t))

(param1 'show 'init "123")
(eprintf "param1 is set to ~a\n" (param1))
(param1 'put 'unit 'gallons)
(eprintf "param1 property ~a is ~a\n" 'unit (param1 'get 'unit))
(param1 'show-prop 'unit 'show 'put 'cost 123)


;; ** Comments

;; The simplest way to get the current value is a fast path.

;; Are there two many features?

;; Other features that might be nice
;; - toggling trace
;; - omit calling parse if guard? is already true?
