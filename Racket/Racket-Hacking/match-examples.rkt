#lang racket

;; These examples contrast the use of the Racket macro match
;; with traditional dynamic code.  Key takeaways
;; (1) The solutions using match are often easier to understand
;; (2) The match macro might not fit your needs.  In particular
;;     it requires that the pattern be fully known at compile time,
;;     i.e. it can't be input or constructed at runtime.
;; (3) When match fits your needs it will be more efficient
;;     because the macro transformer alalyzes the pattern
;;     at compile time and generates efficient code for matching
;;     the data and extracting the desired elements from it at
;;     runtime.
;; (4) Syntax problems with the pattern code and pattern variables
;;     will be reported at comile time, rather than crashing your
;;     program at runtime.

;; So if match is so great, why are we doing this?
;; - There's a lot of value in comparing and contrasting different
;;   approaches to solving the same problem.
;; - We want to demystify how to pattern matching works, helping you
;;   understand match and other pattern maching techniques better.
;; - When match is not a good fit for your situation, we want you
;;   to understand how you can do pattern matching with more explicit
;;   programming techniques.  You should be able to solve any problem
;;   fairly well by writing your own procedures based on Scheme primitives.

(require racket/match)
;; see https://docs.racket-lang.org/guide/match.html
;; and https://docs.racket-lang.org/reference/match.html

;; ** Exaample 1: 'tis a gift to be simple!

;; notice the pattern variable v in the pattern expressions
(define (classify1 x)
    (match x
      [(list 'a v) (string-append "A value: " (number->string v))]
      [(list 'b v) (string-append "B value: " (number->string v))]
      [_ "Unknown"]) )

;; Notice how _ in a pattern acts as a "wildcard", i.e. it will match
;; anything in that location.  Patterns are checked sequentially.
;; After one of the patterns matches, no more patterns will be checked,
;; so be sure to put clauses with more general patterns AFTER clauses
;; with more specific patterns.

;; Note: An exception will be raised if none of the clauses matches
;;       the data.  If this would not be an error, then end with a
;;       wildcard pattern as in our example.

(classify1 '(a 10)) ; => "A value: 10"
(classify1 '(b 5))  ; => "B value: 5"
(classify1 '(z 1))  ; => "Unknown"

;; Here's literal translation.
;; Notice how the pattern variable v gets bound.
(define (classify2 x)
  (cond [(and (list? x) (= 2 (length x)) (eq? 'a (car x)))
         (let ( [v (cadr x)] )
           (string-append "A value: " (number->string v)) ) ]
        [(and (list? x) (= 2 (length x)) (eq? 'a (car x)))
         (let ( [v (cadr x)] )
           (string-append "A value: " (number->string v)) ) ]
        [#t "Unknown"] ) )

;; Notice how #t (true) in a cond clause will always succeed.
;; Cond clauses are checked sequentially and only the first
;; one whose test succeeds will be evaluated.  So be sure to
;; put clauses with more general tests AFTER the clauses
;; with more specific tests.
;; If no tests succeed, no actions will happen and no errors
;; will be reported.
;; If you expect at least one of your tests ought to succeed,
;; add a final clause with #t and report an error if that
;; is the clause which succeeds.

(classify2 '(a 10)) ; => "A value: 10"
(classify2 '(b 5))  ; => "B value: 5"
(classify2 '(z 1))  ; => "Unknown"

;; Here's a more efficient version.
;; We could have used v but x2 seems to go with this code better.
(define (classify3 x)
  (define (no-match) "Unknown")
  (if (pair? x)
      (let ( [x1 (car x)] )
        (if (pair? (cdr x))
            (let* ( [x-rest (cdr x)] [x2 (car x-rest)] )
              (cond [(eq? 'a x1) (string-append "A value: " (number->string x2))]
                    [(eq? 'b x1) (string-append "B value: " (number->string x2))]
                    [#t (no-match)] ) )
            (no-match) ) )
      (no-match) ) )

(classify3 '(a 10)) ; => "A value: 10"
(classify3 '(b 5))  ; => "B value: 5"
(classify3 '(z 1))  ; => "Unknown"

;; A good scheme compiler is likely to optimize classify2 into classify3.
;; Programmers should prioritize clarity over micro-optimization.

;; The match form makes it easier to deal with all possible match failures
;; in one place.

;; ** Example 2: Nested Patterns and Unification

;; pattern variables can be nested  at any depth in the pattern
(define (area1 x)
  (match x
    [(list 'rectangle (list 'width w) (list 'height h)) (* w h)]
    [(list 'triangle (list 'base b) (list 'height h)) (* 1/2 b h)]
    ;; an equilateral triangle of side length a
    [(list 'triangle (list 'side a) (list 'side a) (list 'side a))
     (* (sqrt 3) 1/4 a a) ] ; a is being squared
    ;; a general triangle -- would work for equilaterals too
    [(list 'triangle (list 'side a) (list 'side b) (list 'side c))
     (let* ( [perimeter (+ a b c)] [s (/ perimeter 2)] )
       (printf "semi-perimeter = ~a\n" s)
       (sqrt (* s (- s a) (- s b) (- s c))) ) ]
    [(list 'triangle (list 'side a) (list 'side b) (list 'radians α))
     (* 1/2 a b (sin α)) ]
    [(list 'triangle (list 'side a) (list 'side b) (list 'degrees α))
     (area1 `(triangle (side ,a) (side ,b) (radians ,(* 2 pi (/ α 360))))) ]
    [#t #f] ) )

;; Note: In the equilateral triangle pattern we simply use the same
;; pattern variable more than once.  The pattern will only match if
;; the same value can be bound to all occurrances of the same pattern
;; variable.  This is called Unification.

;; Also note: The more general formula world work for equilateral
;; triangles too, but it won't be considered since only the first
;; clause which matches will be evaluated.

(area1 '(rectangle (width 5) (height 10)))
(area1 '(triangle (base 10) (height 10)))
(area1 '(triangle (side 10) (side 10) (side 10))) ; equilateral
(area1 '(triangle (side 3) (side 4) (side 5)))
;; note quasi-quote ` instead of quote '
(area1 `(triangle (side 3) (side 4) (radians ,(/ pi 2)))) ; right triangle
(area1 '(triangle (side 3) (side 4) (degrees 90))) ; same

;; Creative alternative without match.  This is doing a lot more
;; processing at runtime than match requires.  But the patterns
;; could be read in or computed at runtime.
(define (area2 data)
  ;; local bindings
  (define this 'area2)
  ;; it's an error if the pattern isn't a list of symbols
  ;; -- a required type name followed by 1 or more field names
  (define (assert-well-formed-pattern pattern)
    (define this 'area2--assert-well-formed-pattern)
    (unless (and (list? pattern) (<= 2 (length pattern))) (error this "expected pattern, got ~a" pattern))
    (unless (andmap symbol? pattern) (error this "expected symbols ~a" pattern)) )
  ;; test if data is a well-formed list of
  ;; a type name followed by 1 or more pairs pairs
  ;; where each pair is a list of a field name and a numeric value
  (define (well-formed-data? data)
    (and (list? data)
         (symbol? (car data)) ; the type name
         (list? (cdr data)) ; the fields
         (andmap (λ (field) (and (list? field) (= 2 (length field)) (symbol? (car field)) (number? (cadr field)))) (cdr data)) ) )
  (define (assert-well-formed-data data)
    (define this 'area2--assert-well-formed-data)
    (unless (well-formed-data? data) (error this "expected well-formed-data, got ~a" data)) )
  (define (matches? pattern data)
    (assert-well-formed-pattern pattern) ; data's already been checked
    (and (= (length pattern) (length data))
         (eq? (car pattern) (car data)) ; the types match
         ;; all the field names match
         (andmap (λ (name pair) (eq? name (car pair)))
                 (cdr pattern) (cdr data) ) ) )
  ;; get-values returns all fields -- as multiple values!
  (define (get-values data) (apply values (map cadr (cdr data))))
  ;; check the data
  (assert-well-formed-data data)
  ;; action
  (cond [(matches? '(rectangle width height) data)
         (let-values ( [(w h) (get-values data)] ) (* w h)) ]
        [(matches?'(triangle base height) data)
         (let-values ( [(b h) (get-values data)] ) (* 1/2 b h)) ]
        [(matches?'(triangle side side side) data)
         (let-values ( [(a b c) (get-values data)] )
           (if (= a b c) ; equilateral triangle
               (* (sqrt 3) 1/4 a a); a is being squared
               (let* ( [perimeter (+ a b c)] [s (/ perimeter 2)] )
                 (printf "semi-perimeter = ~a\n" s)
                 (sqrt (* s (- s a) (- s b) (- s c))) ) ) ) ]
        [(matches? '(triangle side side radians) data)
         (let-values ( [(a b α) (get-values data)] )
           (* 1/2 a b (sin α)) ) ]
        [(matches? '(triangle side side degrees) data)
         (let-values ( [(a b α) (get-values data)] )
           (area2 `(triangle (side ,a) (side ,b) (radians ,(* 2 pi (/ α 360))))) ) ]
        [#t #f] ) )

(area2 '(rectangle (width 5) (height 10)))
(area2 '(triangle (base 10) (height 10)))
(area2 '(triangle (side 3) (side 4) (side 5)))
(area1 '(triangle (side 10) (side 10) (side 10))) ; equilateral
;; note quasi-quote ` instead of quote '
(area2 `(triangle (side 3) (side 4) (radians ,(/ pi 2)))) ; right triangle
(area2 '(triangle (side 3) (side 4) (degrees 90))) ; same

;; ** Example 3: Patterns with Guards

;; Sometimes you want to use a match form with patterns to easily
;; match a desired structure, but you also want to call a general
;; predicate.  You can add runtime guard expressions to a match.

(define (new-price1 old-price report)
  ;; adjust our price towards theirs by half the difference
  (define (adjust-towards ours theirs) (+ ours (/ (- theirs ours) 2)))
  (match report
    ;; when others match our price, keep our price
    [(list _ price) #:when (= old-price price) old-price]
    ;; when others have an item on sale, match it
    [(list _ price 'sale) #:when (> old-price price) price]
    ;; our price should never be higher than Macy's
    [(list 'macys price) #:when (> old-price price) price]
    ;; if Macy's is greater than our price, move towards theirs
    [(list 'macys price) (adjust-towards old-price price)]
    ;; our price should never be lower than Gimbel's
    [(list 'gimbels price) #:when (< old-price price) price]
    ;; otherwise keep our old-price
    [_ old-price] ) )

;; the idea of pulling values out of lists, vectors, structures, etc.
;; with pattern matching is awkward for general computing.  It's usually
;; better to pull out all the values of interest (possibly using
;; pattern matching) as soon as we receive the data and make those
;; values separate parameters in the procedures that do the computing.

(define (new-price2 old-price report)
  (define (new-price competitor price [on-sale #f])
    (define diff (- price old-price))
    (define adjusted (+ old-price (/ diff 2)))
    (cond [(zero? diff) old-price]
          [on-sale price]
          [(and (eq? competitor 'macys) (negative? diff)) price]
          [(and (eq? competitor 'macys) (positive? diff)) adjusted]
          [(and (eq? competitor 'gimbels) (positive? diff)) price]
          [#t old-price] ) )
  (match report
    [(list who price) (new-price who price)]
    [(list who price 'sale) (new-price who price #t)]
    [#t (eprintf "new-price-2: unknown report format ~a\n" report) old-price] ) )

;; When the input data is strings rather than lists, use regular expressions
;; see https://docs.racket-lang.org/guide/regexp.html
