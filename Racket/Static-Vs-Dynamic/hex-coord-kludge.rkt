#lang racket

(define hex-vertices
  '( 550 500
     550 450
     455 519
     491 631
     609 631
     645 519 ) )

(define (accum-pair-diffs x y accum pairs)
  (if (null? pairs)
      accum
      (let ( [next-x (car pairs)] [next-y (cadr pairs)] [next-pairs (cddr pairs)] )
        (accum-pair-diffs next-x next-y (cons (list (- next-x x) (- next-y y)) accum) next-pairs) ) ) )

(define (pair-diffs pairs)
  (reverse (accum-pair-diffs (car pairs) (cadr pairs) '() pairs)) )

(pair-diffs hex-vertices)

