#lang racket
(require rackunit) ; unit testing for examples
(require srfi/43) ; scheme extended vector library

(define four-bit-color-names ; a vector, i.e. a contiguous 1-dimensional array of elements
  #("black" "navy" "green" "teal"
    "maroon" "purple" "olive" "silver"
    "gray" "blue" "lime" "aqua"
    "red" "fuchsia" "yellow" "white" ) )

(define (color-name-by-code code) ; O(1) small k
  (vector-ref four-bit-color-names code) )

;; show examples of color-name-by-code - and check them!
(check-equal? "black" (color-name-by-code 0))
(check-equal? "white" (color-name-by-code 15))
(check-exn exn:fail? (λ () (color-name-by-code -1))) ; invalid index!
(check-exn exn:fail? (λ () (color-name-by-code 16))) ; invalid index!

(define (color-code-by-name:linear name) ; O(n) small k
  (vector-index (λ (color) (equal? color name)) four-bit-color-names) )

;; show examples of color-code-by-name:linear - and check them!
(check-equal? 0 (color-code-by-name:linear "black"))
(check-equal? 15 (color-code-by-name:linear "white"))
(check-pred false? (color-code-by-name:linear "hello"))

;; Let's create a vector of pairs, sorted by the codes

(define four-bit-color-pairs-by-code ; vector of (name . code) pairs
   (vector-map (λ (i x) (cons x i)) four-bit-color-names) )

;; Now one with the same pairs but sorted by the names

(define four-bit-color-pairs-by-name ; vector of (name . code) pairs
  (vector-sort four-bit-color-pairs-by-code string<? #:key car) )

(define (color-pair-by-name:log name) ; O(log n) smallish k
  (let ([index (vector-binary-search
                four-bit-color-pairs-by-name
                name
                (λ (color-code-pair string2)
                  (let ( [string1 (car color-code-pair)] )
                    (if (string<? string1 string2) -1 (if (string=? string1 string2) 0 1)) ) ) ) ])
    (and index (vector-ref four-bit-color-pairs-by-name index)) ) )

;; show examples of color-pair-by-name:log - and check them!
(check-equal? '("black" . 0) (color-pair-by-name:log "black"))
(check-equal? '("white" . 15) (color-pair-by-name:log "white"))
(check-pred false? (color-pair-by-name:log "hello"))

;; Finally, let's build a hash table from the same data
;; make-hash expects the data as a list of pairs
;; it will store it via a hash based on the car of the pairs

(define four-bit-color-pairs-hashed-by-name
  (make-hash (vector->list four-bit-color-pairs-by-code)) )

(define (color-pair-by-name:hash name) ; O(1) medium k
  (hash-ref four-bit-color-pairs-hashed-by-name name #f) ) ; return #f on failure

;; show examples of color-pair-by-name:hash - and check them!
(check-equal? 0 (color-pair-by-name:hash "black"))
(check-equal? 15 (color-pair-by-name:hash "white"))
(check-pred false? (color-pair-by-name:hash "hello"))
