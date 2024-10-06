#lang racket/base

;; * An example of association Lists aka alists in Racket

;; alists map keys to arbitrary values
;; alists of lists map keys to lists of values

;; Our situation
;; - the alists are stored in a field of a mutable structure
;;   the field "more" of structure type "sprite"
;; - the alist keys are symbols which we can compare with eq?
;; - the alist values are lists of arbitrary values

;; Notes:
;; 1. (list) is the same as '()
;;    (list 'this 'that) is the same as '(this that)
;;    and also the same as (cons 'this (cons 'that '()))
;; 2. (eq? v1 v2) tests to see if v1 and v2 are identical
;;    - do the values have the same bit-pattern?
;;    - if they're pointers, do they point to the same place?
;; 3. (equal? v1 v2) tests to see if v1 and v2 are "equivalent"
;;    - if they're eq? then they're equal?
;;    - if they're two collections of the same type
;;        - e.g. two lists or two vectors
;;      then they're equal? if they're recursively equal?, i.e.
;;          they have the same number of elements
;;          their corresponding elements are equal?
;;    - if they're of the same atomic datatype
;;      and that datatype provides an equality predicate
;;      and that predicate says that they're equal
;; 4. member uses equal? for comparisons

;; ** Setup

;; In our real program sprite has other fields;
;; here we're only concerned with its field "more"
(struct sprite (more) #:transparent #:mutable)

;; As examples, and for tests:
(define s0 (sprite (list))) ; same as (sprite '())
(define s1 (sprite (list '(fruits orange banana) '(vegetables beans cabbage))))

;; ** Utility Functions

;; ensure a value is a member of a list
;; return the original list if the specified value is already present
;; otherwise return a list extended with the specified value
(define (ensure-member val lst)
  (if (member val lst) lst (cons val lst)) )

;; drop any member of an alist with the specified key
(define (alist-drop alist key)
  (remove key alist (λ (v lst) (eq? v (car lst)))) )

;; ** Desired Functions

;; return the list of values associated with key or '() if none.
(define (sprite-more-values s key)
  ;; assq will return the whole list or #f
  (let ( [items (assq key (sprite-more s))] )
    (if items (cdr items) '()) ) )

;; ensure a value is a member of a sprite's alist under a specified key
;; leave the sprite unchanged if it is already present
;; otherwise add it to the sprite's alist <- mutation!
;; return that alist's list of values for convenience
(define (sprite-more-key-value! s key val)
  (define this 'sprite-more-key-value!)
  (let* ( [old-alist (sprite-more s)]
          [old-vals (sprite-more-values s key)]
          [new-vals (ensure-member val old-vals)] )
    (unless (eq? old-vals new-vals)
      (set-sprite-more! s 
                        ;; put the new alist in front
                        (cons (cons key new-vals) ; the new alist
                              (alist-drop old-alist key) ) )
      (printf "! ~a ~a\n" this (sprite-more s)) )
    new-vals ) )

;; demo and test

(require rackunit)
(require racket/set)

(printf "Start with:\n")
(printf "~a: ~a\n" 's0 (sprite-more s0))
(printf "~a: ~a\n" 's1 (sprite-more s1))

(define original-s1 (struct-copy sprite s1))

(newline)
(printf "Expect no change:\n")
(check-equal? (sprite-more-values original-s1 'fruits)
             (sprite-more-key-value! s1 'fruits 'orange) )
(check-equal? (sprite-more-values original-s1 'vegetables)
             (sprite-more-key-value! s1 'vegetables 'cabbage) )

(check-equal? original-s1 s1)

;; check that the alists in s0 and s1 are identical
;; except for any alists with key k
(define (check-other-alists-unchanged s0 s1 not-this-key)
  (define this 'check-other-alists-unchanged)
  (let ( [s0-keys (remove not-this-key (map car (sprite-more s0)))]
         [s1-keys (remove not-this-key (map car (sprite-more s1)))] )
    #;(check-true (null? (set-subtract s0-keys s1-keys)))
    (unless (null? (set-subtract s0-keys s1-keys))
      (error this "s0-keys~a s1-keys~a\n" s0-keys s1-keys))
    (for-each (λ (k)
                (unless (eq? k not-this-key)
                  (check-eq? (sprite-more-values s0 k)
                             (sprite-more-values s1 k) ) ) )
              s0-keys ) ) )

;; check that
;; (1) new-vals contains v
;; (2) if old-vals contained v then then new-vals is identical to old-vals
;; (3) otherwise, new-vals is just old-vals extended with v in front
(define (check-value-added old-vals new-vals v)
  (if (member v old-vals)
      (check-eq? old-vals new-vals "originally present yet not identical")
      ;; new-vals should just be old-vals extended with v in front
      (and (check-eq? v (car new-vals) "not car of new vals")
           (check-eq? old-vals (cdr new-vals)) "not extension of old vals" ) ) )

(define (add-vals name s k . vals)
  (let ( [s-before (struct-copy sprite s)] )
    (for ( [v vals] )
      (let ( [vals-before (sprite-more-values s k)] )
        (newline)
        (printf "> ~a: ~a\n" name (sprite-more s))
        (printf "| ~a add ~a -> ~a\n" name k v)
        (sprite-more-key-value! s k v)
        (printf "< ~a: ~a\n" name (sprite-more s))
        (check-other-alists-unchanged s-before s k)
        (check-value-added vals-before (sprite-more-values s k) v) ) ) ) )

(newline)
(printf "Expect changes:\n")

(add-vals 's0 s0 'fruits 'plum)
(add-vals 's0 s0 'fruits 'apple)

(add-vals 's0 s0 'vegetables 'okra)

(add-vals 's1 s1 'fruits 'plum)

(add-vals 's1 s1 'vegetables 'okra)

;; ** Convenience Functions

;; We can define convenience functions for keys of interest

(define (sprite-killboxes s) (sprite-more-values s 'killboxes) )

(define (sprite-hurtboxes s) (sprite-more-values s 'hurtboxes) )

(define (set-sprite-killboxes! s b) (sprite-more-key-value! s 'killboxes b) )

(define (set-sprite-hurtboxes! s b) (sprite-more-key-value! s 'hurtboxes b) )
