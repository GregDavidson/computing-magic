#lang racket/base

(require (file "/home/greg/Gits/Learner-Gits/Eti/Multiplayer-Flightlander/sprites-client-framework.rkt"))

(require racket/stream)

;; ** Use a structure providing gen:stream

;; https://docs.racket-lang.org/reference/sequences.html

;; We should design our error messages to
;; be consistent with this model:
;; > (stream-rest (sequence->stream #()))
;; stream-rest: contract violation
;; expected: (and/c stream? (not/c stream-empty?))
;; given: #<stream>
;; > (stream-first (sequence->stream #()))
;; stream-first: contract violation
;; expected: (and/c stream? (not/c stream-empty?))
;; given: #<stream>

;; Need to ensure this invariant everywhere!!
;; Invariant: Each world must have at least one sprite
;; or it would have been deleted.

 ;; Use external constructor make-all-sprites
(struct all-sprites (worlds sprites)
  #;(: worlds (streamof gvector?))
  #;(: sprites (streamof sprite?))
  #:constructor-name raw-all-sprites
  #:methods gen:stream
  [(define (stream-empty? iter)
     (let ( [worlds (all-sprites-worlds iter)]
            [sprites (all-sprites-sprites iter)] )
       (and (stream-empty? worlds)
            (stream-empty? sprites) ) ) )
   (define (stream-first iter)
     (let ( [worlds (all-sprites-worlds iter)]
            [sprites (all-sprites-sprites iter)] )
     (cond [(not (stream-empty? sprites)) (stream-first sprites)]
           [(not (stream-empty? worlds)) (stream-first (stream-first worlds))]
           [else (error 'all-sprites-first "no sprites left")] ) ) )
   (define (stream-rest iter)
     (let ( [worlds (all-sprites-worlds iter)]
            [sprites (all-sprites-sprites iter)] )
     (cond [(not (stream-empty? sprites)) (make-all-sprites worlds (stream-rest sprites))]
           [(not (stream-empty? worlds)) (make-all-sprites (stream-rest worlds) (stream-rest (gvec-stream (stream-first worlds))))]
           [else (error 'all-sprites-rest "no sprites left")] ) ) )
   ] )

(define (make-all-sprites worlds [sprites empty-stream])
  (define this 'make-all-sprites)
  (cond [(stream? worlds)
         (unless (stream? sprites) (error this "expected stream ~a" sprites))
         (raw-all-sprites worlds sprites) ]
        [(universe? worlds) ; we're nice!
         (unless (eq? sprites empty-stream) (error this "expected empty-stream ~a" sprites))
         (raw-all-sprites worlds sprites) ]
        [(client-state? worlds) ; are we too nice?
         (make-all-sprites (client-state-worlds-sprites worlds) sprites) ]
        [else (error this "Not a universe ~a" worlds)] ) )