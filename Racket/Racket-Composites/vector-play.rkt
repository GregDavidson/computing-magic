#lang racket
(require rackunit) ; unit testing for examples

;; Assuming a vector is already sorted, according to some sort
;; of total order based on explicit or implicit keys, how can
;; we efficiently locate something by an explicit key

;; We require a function (order vector-element explicit-key)
;; which cam compare any element with a key and return
;; -1 when the element is "less than" the key
;; 0 when the element is "equal to" the key
;; 1 when the element is "greater than" the key

; scheme procedures can return more than one value using (values ...)
; return (values element index) if present, otherwise (values #f and #f)
(define (sorted-vector+key->value+index
               v ; totally sorted array
               key ; key to search for
               order ; (order elem key) => -1 0 1
               )
  (define (search lo hi)
    (if (<= hi lo) (values #f #f)
        (let* ( [mid-i (quotient (- hi lo) 2)]
                [mid-val (vector-ref v mid-i) ] )
          (match (order mid-val key)
            [0 (values mid-val mid-i)]
            [0 mid-val]
            [-1 (search v lo mid-val)]
            [1 (search v mid-val hi)] ) )) )
  (search 0 (sub1 (vector-length v))) )

; return the element if present, otherwise #f
(define (sorted-vector+key->value v key order)
  (let-values ( [(value index) (sorted-vector+key->value+index v key order)] ) value) )

; return the index of the element if present, otherwise #f
(define (sorted-vector+key->index v key order)
  (let-values ( [(value index) (sorted-vector+key->value+index v key order)] ) index) )

(define test-vector #( (1 . 'red) (3 . 'blue) (7 . green) ))
(define (test-vector-order elem key2)
  (let ( [key1 (car elem)] )
    (cond [(< key1 key2) -1]
          [(< key2 key1) 1]
          [else 0] ) ) )

(check-equal? '(3 . 'blue) (sorted-vector+key->value test-vector 3 test-vector-order))
(check-equal? 1 (sorted-vector+key->index test-vector 3 test-vector-order))

;; Let's learn how to sort a vector!

;; The vector needs to be mutable
;; (copy it if it isn't, and sort the copy)
;; vector-swap! will do all of the mutations

; swap the values at two indices in a mutable vector
(define (vec-swap! v i1 i2)
  (let ( [hold (vector-ref v i1)] )
    (vector-set! v i1 (vector-ref v i2))
    (vector-set! v i2 hold) ) )

;; We need a predicate function
;; (less [v : vector] [i1 : index] [i2 : index])
;; which establishes a total ordering on v
;; by returning true if and only if
;; (vector-ref v i1) is less than (vector-ref v i2)

;; Given a mutable vector v
;; with two valid indices lo < hi
;; return a median index lo <= med < hi
;; and ensure that
;; (nor (less v med lo) (less v hi med))
;; by swapping values as necessary
(define (vec-get-median! v lo hi less)
  (let ( [mid (quotient (- hi lo) 2)] )
    (when (less v hi mid)
      (vec-swap! v hi mid) )
    (when (less v mid lo)b
      (vec-swap! v mid lo) ) ) )

; given a range within a mutable vector v
; between indices lo and hi
; partition it's values around a selected value mid
; such that all values between lo..mid <= the value at mid
; and all values between mid..hi >= the value at mid
; swapping values on either side of mid to make it so!
; returning the median index
(define (vec-partition! vector v lo hi less)
  (when (< lo hi) ; otherwise we're done!
    (let ([mid (vec-get-median! v lo hi less)])
      (vec-partition! v (add1 lo) (sub1 hi))
       
(define (vec-sort! v lo hi less)
  (when (< lo hi) ; otherwise we're done
    (let ([mid (vec-get-median! v lo hi less)])
      (vec-sort! v lo (sub1 mid) less)
      (vec-sort! v (add1 mid) hi less) ) ) )