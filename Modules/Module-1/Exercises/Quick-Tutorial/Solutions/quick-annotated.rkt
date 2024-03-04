#lang slideshow

;; This code is from the Racket Tutorial
;; https://docs.racket-lang.org/quick/index.html
;; ==> You should do the tutorial first!
;; and then perhaps read this annotated reiteration
;; of the code.

;; Comments convention
;; - Comments using double semi-colons typically refers
;;   to whatever follows such comments.
;; - Comments using a single semi-colon typically refers
;;   to the code before the comment.
;; - The Racket reader macro #; turns the immediately
;;   following symbolic expression into a comment
;;   which is a great way of commenting out code.

;; It's often helpful to first look at the code following a block
;; of comments giving explanations before reading those comments
;; and then read those comments while referencing the code.

;; The Environment

;; An Environment is a Set of Bindings.  A Binding
;; is a Symbol paired with a Value.  Evaluating a symbol
;; in the Environment where it is bound yields its value.

;; Initially we have the Global or Top-Level Environment,
;; not nested inside of any other Environment.

;; Each define form extends the Environment with a new Binding.
;; Let's add a couple of new bindings:

;; A pixel is a picture element (color dot) of the screen.
;; The size of a pixel is dependent on the screen's resolution.
;; A point is an old but still popular unit = 1/72 inch.
;; The size of a point should be the same on any screen.
;; What unit is being used by circle and rectangle?

(define c (circle 10))
(define r (rectangle 10 20))

;; The define form is a macro which adds
;; syntactic sugar for esthetics and convenience.
;; We need to keep eval, the Lisp Evaluator, from
;; trying to access the symbol's value before
;; we've given it one!

;; In some Lisps a simple define form will
;; "macro expand" into a set form.  A set form
;; evaluates both arguments whereas a define form
;; implicitly quotes its first argument.  The macro
;; lets us break the rule that all arguments
;; of a form are evaluated!

;; Macros are better explained towards the end
;; of the tutorial.  For now, think of them as
;; abbreviations which are automatically
;; translated into lower-level code which does
;; the actual work.

#;(define x 10) ; might "macro expand" into
#;(set 'x 10) ; notice we have to quote the symbol

;; Another nice feature of the define macro
;; is simplifying creation of new procedures.

;; A procedure has (1) a list of parameter
;; symbols to bind in a new environment and
;; (2) one or more forms to evaluate in that
;; environment.

;; bind square to a procedure which expects
;; its parameter n to be a valid height and
;; width value and creates a square shape
;; using filled-rectangle
(define (square n)
  (filled-rectangle n n) )

;; A square form must supply a value (called
;; an argument for n).  When that form is evaluated
;; a new environment will be created with a
;; binding for n.  The new environment will be
;; nested inside the definition environment present
;; when the square procedure was defined (created).
;; All other bindings in the definition environment
;; are also available in the nested environment.

;; We can call the new environment a "local" enviroment
;; which extends an "outer" or "more global" environment
;; with one or more new bindings and "inheriting" all
;; of the other bindings of the outer environment.
;; The local bindings "shadow" or "hide" any bindings
;; of the same name so they are not available in the
;; local environment.

;; When this form is evaluated a new environment is
;; created with n bound to 10.  The body of the
;; procedure bound to square 
(square 10)

;; We can use define without the syntactic sugar
;; defining procedures by making the creation
;; of the procedure as a value explicit with λ.

;; λ forms (aka lambda forms) are the fundamental forms
;; for creating all procedures in Racket and other Lisps.

;; A λ form takes a list of Parameters which extend
;; the environment and a Body of forms to evaluate
;; within that extended environment.  A square-too
;; form must 
;; i.e. evaluate a square-too form
;; the calling form must supply arguments for the
;; parameter n.  Both square-too and filled-rectangle
;; are bound to procedures in the enclosing global environment.

;; Local environments are nested inside of 

(define square-too
  (λ (n) (filled-rectangle n n)) )

;; The local environment extended with the parameter bindings
;; can be further extended locally with additional define forms.
;; These new bindings only extend the local environment; they
;; are inaccessible outside of the scope (context) indicated by
;; the parentheses surrounding the definition of the procedure.

(define (four p) ; p will be bound in the local environment
  (define two-p (hc-append p p)) ; two-p bound in the local environment
  (vc-append two-p two-p) ) ;; end of local environment scope

;; Racket [square brackets] are equivalent (to parentheses)
;; and may help human eyes track nesting structure.  They
;; are not usually used for forms or lists but rather for
;; for structured data within such.

(define (checker p1 p2)
  ;; A let form takes a list of bindings and a body to evaluate
  ;; within a new environment extended with those bindings
  ;; and evaluates the body inside of that nested environment.
  ;; Using a 
  ;; extended with two new bindings:
  (let ( [p12 (hc-append p1 p2)]
         [p21 (hc-append p2 p1) ] )
    (vc-append p12 p21) ) )

(define (checkerboard p)
  (let* ( [rp (colorize p "red")]
          [bp (colorize p "black")]
          [c (checker rp bp)]
          [c4 (four c) ] )
    (four c4) ) )

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)) )

(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue"))) ) )

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple") ) )

;; two lumps of sugar
(define ((rect1 wt) ht) (filled-rectangle wt ht))
;; only one lump of sugar
(define (rect2 wt) (λ (ht) (filled-rectangle wt ht)))
;; no sugar
(define rect3 (λ (wt) (λ (ht) (filled-rectangle wt ht))))
