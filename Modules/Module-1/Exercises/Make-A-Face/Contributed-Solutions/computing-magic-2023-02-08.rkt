#lang slideshow

(define (square n)
  (filled-rectangle n n) )

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p) )

(define (checker p1 p2)
  (let ( [p12 (hc-append p1 p2)]
         [p21 (hc-append p2 p1)] )
    (vc-append p12 p21) ) )

(define pretty-picture
  (checker (colorize (square 10) "red")
           (colorize (square 10) "black") ) )

(define big 100)

(square big)

(define things '(1 2 three four 3/4 "hello!"))

(define (append-to-end item alist)
  (if (null? alist)
      (list item)
      (cons (car alist) (append-to-end item (cdr alist))) ) )

(define (naive-reverse alist)
  (if (null? alist)
      '()
      (append-to-end (car alist) (reverse (cdr alist))) ) )

(define (reverse-accum alist accum)
  (if (null? alist)
      accum
      (reverse-accum (cdr alist) (cons (car alist) accum)) ) )

(define (reverse alist)
  (reverse-accum alist '()) )

; How do we solve big complicated problems?

(define (solve-big-problem big-prob)
  (if (trivial? big-prob)
      (trivial-solution big-prob)
      (let-values ( [(piece1 piece2 piece3) (breakup-problem big-problem)] )
        (assemble-sub-solutions (solve-piece piece1)
                                (solve-big-problem piece2)
                                (solve-piece piece3) ) ) ) )
