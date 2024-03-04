#lang racket/base
(require pict)

;; * Quick: An Introduction to Racket with Pictures

;; This code is from the Racket Tutorial
;; https://docs.racket-lang.org/quick/index.html
;; ==> You should do the tutorial first!
;; and then perhaps read the annotated version
;; which should accompany this file.

;; ** Section 1 Ready

;; ==> The annotated version of this file will explain
;; environments, bindings, macros and syntactic sugar
;; which get only minimal mentions here.

;; ** Section 2 Ready

;; Comments convention
;; - Comments using double semi-colons typically refers
;;   to whatever follows such comments.
;; - Comments using a single semi-colon typically refers
;;   to the code before the comment.
;; - The Racket reader macro #; turns the immediately
;;   following symbolic expression into a comment
;;   which is a great way of commenting out code.

;; ** Section 3 Go!

;; Show some values

"this string, then an integer"
5

;; A pixel is a picture element (color dot) of the screen.
;; The size of a pixel is dependent on the screen's resolution.
;; A point is an old but still popular unit = 1/72 inch.
;; The size of a point should be the same on any screen.
;; What unit is being used by the picture constructors, e.g.
;; circle and rectangle, in this package?

"some pictures"
(circle 10)
(rectangle 10 20)
(hc-append (circle 10) (rectangle 10 20))
(hc-append 6 (circle 10) (rectangle 10 20))

;; ** Section 4 Definitions

;; Let's extend the global environment with some new bindings:

;; define a couple of pictures
;; i.e. create such and bind them to the symbols 'c and 'r
;; Note: ussing such short names is bad style for global symbols!
(define c (circle 10))
(define r (rectangle 10 20))

;; Define is really a macro which creates some pleasant
;; syntactic sugar.  Macros and syntactic sugar will be
;; covered towards the end of the tutorial and in the
;; annotated

;; define a functional procedure that can make squares
;; i.e. create such and bind it to the symbol 'square
;; The syntax (square n) is sugar for a λ (lambda) form
(define (square n)
  (filled-rectangle n n) )

;; define a similar procedure without syntactic sugar
(define square-too
  (λ (n) (filled-rectangle n n)) )

"two identical squares"
(square 10)
(square-too 10)

;; ** Section 5 Local Binding

(define (four p)
  (define two-p (hc-append p p))        ; bind two-p in scope of procedure
  (vc-append two-p two-p) )

;; a cleaner way to write four
(define (four-too p)
  (let ( [two-p (hc-append p p)] )      ; bind two-p in scope of let
    (vc-append two-p two-p) ) )

"two identical 2x2 groups of circles"
(four (circle 10))
(four-too (circle 10))

(define (checker p1 p2)
  (let ( [p12 (hc-append p1 p2)]        ; let bindings are independent
         [p21 (hc-append p2 p1) ] )
    (vc-append p12 p21) ) )

"a 2x2 checkerboard"
(checker (colorize (square 10) "red")
         (colorize (square 10) "black") )

(define (checkerboard p)
  (let* ( [rp (colorize p "red")]       ; let* bindings are incremental
          [bp (colorize p "black")]
          [c (checker rp bp)]           ; use of earlier let* bindings
          [c4 (four c) ] )              ; use of earlier let* binding
    (four c4) ) )

"a big checkerboard"
(checkerboard (square 10))

;; ** Section 6 Functions are Values

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)) )

"some series"
(series circle)
(series square)
(series (λ (size) (checkerboard (square size))))

;; ** Section 7 Lexical Scope

(define (rgb-series mk)
  (vc-append
   (series (λ (sz) (colorize (mk sz) "red")))
   (series (λ (sz) (colorize (mk sz) "green")))
   (series (λ (sz) (colorize (mk sz) "blue"))) ) )

"some rgb-series"
(rgb-series circle)
(rgb-series square)

;; ** Section 8 Lists

"some lists"
(list "red" "green" "blue")
(list (circle 10) (square 10))

(define (rainbow p)
  (map (λ (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple") ) )

"a rainbow of squares"
(apply vc-append (rainbow (square 5)))

;; ** Section 9 Modules

;; This section is quite lame.
;; You can study Modules in the Racket Guide
;; https://docs.racket-lang.org/guide/modules.html

;; ==> You don't need to know much to simply use modules!

;; You can postpone learning about Modules
;; until you need to create large, complex projects!

;; Using Modules

;; Use require to "bring in" the bindings exported by a module
(require pict/flash)

;; Those bindings are now part of your program

"a filled-flash"
(filled-flash 40 30)

;; The require will fail is the module has not been
;; installed on your computer.
;; You can install it with the raco command line tool
;; or with DrRacket.

;; ** Section 10 Macros

;; Another lame section!

;; You can study Racket Macros in the Racket Guide
;; https://docs.racket-lang.org/guide/macros.html

;; ==> You don't need to know much to simply use macros!
;; ==> Avoid creating macros, prefer ordinary procedures!

;; A form is a macro if
;; - the first element of the form is a symbol
;; - which was defined to be a macro
;; Macro forms are evaluated before normal execution
;; - there's a procedure associated with the macro
;; - which translates the macro form into a different expression!

;; Why do this?
;; - Macro forms can deviate from normal execution

;; Normal Form Evaluation:
;;   - FIRST: All elements of a form are evaluated
;;     - The first element must evaluate to a procedure
;;     - The rest evaluate to the arguments for the procedure
;;   - THEN that procedure is "applied" to its arguments
;;     - a temporary nested environment is extended with
;;     - the procedure's parameters bound to the arguments
;;     - the procedure's body is evaluated in that environment

;; Special Form Evaluation:
;; - The compiler notices that the first element of a form is special
;;   - Either a macro
;;   - Or one of the very few special forms built in to Racket
;; - Because each special form works in a special way
;;   - They make it harder to understand how programs work!
;;   - So please avoid creating more of them without great need!

;; Example: define forms

;; If define were a regular procedure in
#;(define c (circle 10))
;; then c would be evaluated BEFORE define was applied to it
;; - We'd be trying to evaluate c BEFORE it was defined!
;; So define has to suppress the evaluation of its first argument

;; When we define a procedure, e.g.
#;(define (square n) (filled-rectangle n n))
;; The macro procedure for define translates it to
#;(define square (λ (n) (filled-rectangle n n)))

;; What other Special Forms do you need to understand?

;; Consider λ, if, cond forms
;; - do they evaluate normally?
;; - Which ones might be macros?
;; - Any others have to be built-in to Racket itself!

;; *** Reader Macros

;; There are also a few Reader Macros
;; - Special characters which expand into special forms
;; - by the read procedure.

;; Single Quote is a Reader Macro for quote
;; 'x expands to (quote x) for any expression x
;; Is (quote x) a special form?

"suppressing evaluation of some expressions"
'hello
(quote hello)
'(one two (eyes of blue))
(quote (one two (eyes of blue)))

;; ** Section 11 Objects

;; Another lame section!

;; This section is about
;; - Special Racket-Specific support for
;; - The Object-Oriented Programming Paradigm (OOP)

;; It doesn't explain that
;; - Lisp systems don't have much need for OOP
;; - Rackets support for OOP is weird!

;; You can study Racket's support for OOP in the Racket Guide
;; https://docs.racket-lang.org/guide/classes.html

;; *** Why Learn and Use Racket's OOP?

;; Many important Racket facilities, e.g. their implementation of Graphics and
;; Graphical Interfaces are implemented using Racket's OOP system so it may be
;; useful to learn how to use it.

;; We recommend avoiding designing any of your own Racket programs around
;; Racket's OOP system for numerous reasons. We refer you to our opinionated
;; article on the subject
;; file:racket-oop.org
