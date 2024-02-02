#lang racket

;; * Imperative Mutation and Iteration Examples

;; It's usually better to program functionally, transforming data structures
;; into new data structures without any mutation. Knowing that existing data
;; structures never change simplifies reasoning and reduces opportunities to
;; make bugs. Sometimes, however, it's more convenient or significantly more
;; efficient to program imperatively, mutating existing data structures. The
;; Lisp Family provides features for both styles of computation.

;; Imperative mutation and iteration produce no values;
;; only side-effects will be observable.

;; The convention in the Scheme Family of Lisps is to give
;; procedures which cause mutation names ending in "!".
;; This is intended as a warning.  Mutation should be used
;; sparingly and should always be well documented!

;; ** define and set!

;; set! is the infamous procedure which changes an existing binding.
;; set! can cause surprises and is best avoided in large
;; scopes, e.g. global bindings!

(define greet 'hello)

(printf "greet is bound to ~a\n" greet)

(set! greet 'goodbye)

(printf "greet is now bound to ~a\n" greet)

(define count 0)

(printf "count is bound to ~a\n" count)

(set! count (+ 1 count))

(printf "count is now bound to ~a\n" count)

;; ** Immutable Structures

;; Structures in Racket are immutable by default, e.g. once you construct a
;; structure with particular values, those values will never change. If you want
;; a structure with different values, you simply create a new structure.

(struct hold-number (num)
  #:transparent  ; nice for debugging
  #:guard        ; ensure we have a number
  (λ (num name)
    (unless (number? num) (error "Not a number!"))
    num ) )

;; hold1 and hold2 will never change once constructed
;; although the bindings can be changed.
(define hold1 (hold-number 1))
(printf "hold1 is bound to ~a\n" hold1)

(define hold2 (hold-number (+ 1 (hold-number-num hold1))))
(printf "hold2 is bound to ~a\n" hold2)

(printf "hold1 is still bound to ~a\n" hold1)

;; These lists and the structures they contain will never change.
(define list1 (map hold-number '(1 2 3)))
(printf "list1 is bound to\n\t~a\n" list1)
(define list2 (map (λ (n) (hold-number (+ 1 (hold-number-num n)))) list1))
(printf "list2 is bound to\n\t~a\n" list2)

(printf "list1 is still bound to\n\t~a\n" list1)

;; ** Mutable Structures

;; The #:mutable option is nicely up-front to prevent surprises.

;; Prefer using struct/contract with mutable structures.
;; The #:guard clause in a regular struct doesn't check mutations!
;; Contracts established with struct/contract are always checked,
;; and the syntax is nicer too!

;; used for keeping a count
(struct/contract tally ([count natural?])
                 #:mutable      ; warn everyone!
                 #:transparent  ; nice for debugging
                 )

;; tally-inc increments the count within a tally
(define (tally-inc! t) (set-tally-count! t (+ 1 (tally-count t))))

(define a-tally (tally 0))

(printf "a-tally is bound to ~a\n" a-tally)

(tally-inc! a-tally)

(printf "a-tally is now bound to ~a\n" a-tally)

;; ** A Sequence Of Mutable Objects

;; In this case, a list, but the other sequence types
;; can be accommodated similarly.

(define tally-list (map tally '(1 2 3)))

(printf "tally-list is bound to\n\t~a\n" tally-list)

;; ** A Tail-Recursive Procedure

;; Apply predicate p to each element of list l
;; Returns nothing, i.e. (void)
(define (foreach p l)
  (when (pair? l)
    (p (car l))
    (foreach p (cdr l)) ) )

(foreach tally-inc! tally-list)

(printf "tally-list is now bound to\n\t~a\n" tally-list)

;; ** A Degenerate foldl

(foldl (λ (t _) (tally-inc! t)) #f tally-list)

(printf "tally-list is now bound to\n\t~a\n" tally-list)

;; ** The Racket Interation Form for

;; Using in-list allows the compiler to produce fast code

(for ( [t (in-list tally-list)] ) (tally-inc! t))

(printf "tally-list is now bound to\n\t~a\n" tally-list)
