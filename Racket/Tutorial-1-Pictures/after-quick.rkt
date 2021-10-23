#lang slideshow

;; AFTER exploring the Racket Tutorial
;; Quick: An Introduction to Racket with Pictures
;; https://docs.racket-lang.org/quick/
;; This provides some useful additional exploration!

(define c (circle 10))
(define r (rectangle 10 20))

;; (define (square n) (filled-rectangle n n))
;; λ is lambda, Racket takes either
(define square (λ (n) (filled-rectangle n n)))

(define hc-append-three (λ (c) (hc-append 22 c c c)))

;; five versions of procedure two-by-two do the same thing

(define (two-by-two-v1 p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(define (two-by-two-v2 p)
  (let ( [two-p (hc-append p p)] )
    (vc-append two-p two-p) ) )

;; there are two separate things named p
(define (two-by-two-v3 p)
  (let ( [vc-two (λ (p) (vc-append p p))] )
    (vc-two (hc-append p p)) ) )

;; there are two separate things named p
(define (two-by-two-v4 p)
  ( (λ (p) (vc-append p p)) (hc-append p p) ) )

;; introducing a "combinator"
(define (two-by-two-v5 p)
  (let ( [double (λ (f x) (f x x))] )
    (double vc-append (double hc-append p)) ) )

;; the tutorial wants one called four
(define four two-by-two-v5)

; how could this be written using λ instead of let?
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21) ) )

; how could this be written using nested let forms instead of let*?
(define (checkerboard p)
  (let* ( [rp (colorize p "red")]
          [bp (colorize p "black")]
          [c (checker rp bp)]
          [c4 (four c)] )
    (four c4) ) )

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)) )

(define checkerboard-series (series (λ (size) (checkerboard (square size)))))
