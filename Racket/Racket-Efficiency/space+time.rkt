#lang racket

;; Space and Time Management

; Raise a number to an exact integer power
; Linear time, linear space
(define (x-to-i-v1 x i)
  (define (to-i i)
    (cond [(zero? i) 1]
          [(negative? i) (/ 1 (to-i (- i)))]
          [(positive? i) (* x (to-i (sub1 i)))] ) )
  (to-i i) )

; Raise a number to an exact integer power
; Linear time, linear space
; refactored a bit nicer
(define (x-to-i-v2 x i)
  (define (n-to-i n i)
    (if (zero? i) 1 (* n (n-to-i n (sub1 i)))) )
  (if (negative? i) (/ 1 (n-to-i x (- i))) (n-to-i x i)) )

; Raise a number to an exact integer power
; O(log n) time, O(log n) space
; With type checking
(define (x-to-i-v3 x i)
  (define (n-to-i n i)
    (cond [(zero? i) 1]
          [(even? i) (let ( [x1 (n-to-i n (/ i 2))] ) (* x1 x1))]
          [else (* n (n-to-i n (sub1 i)))] ) )
  (when (not (and (integer? i) (exact? i)))
    (raise-argument-error 'x-to-i "exact integer" 2) )
  (if (negative? i) (/ 1 (n-to-i x (- i))) (n-to-i x i)) )

; Raise a number to an exact integer power
; Linear time, constant space (tail call iterative)
(define (x-to-i-v2b x i)
  (define (n-to-i n i accum)
    (if (zero? i)
        accum
        (n-to-i n (sub1 i) (* n accum)) ) )
  (if (negative? i) (/ 1 (n-to-i x (- i) 1)) (n-to-i x i 1)) )

; Raise a number to an exact integer power
; O(log n) time, constant space (tail call iterative)
; With type checking
; ==> YOUR CODE HERE <==

; Exact fibonacci numbers, where
; fib(0) = 0
; fib(1) = 1
; fib(n>1) = fib(n-1) + fib(n-2)
; ==> YOUR CODE HERE <==

; Solution of the Towers of Hanoi problem
; https://en.wikipedia.org/wiki/Tower_of_Hanoi
; solution with steps shown by side-effects
(define (towers-of-hanoi n source dest temp)
 (when (> n 0)
   (towers-of-hanoi (sub1 n) source temp dest)
   (printf "Move disk from ~a to ~a\n" source dest)
   (towers-of-hanoi (sub1 n) temp source dest) ) )
; Is tail call optimization possible?
; How can it be made into a pure function?