#lang slideshow

(define (boxed-string s #:width [width #f] #:height [height #f])
  (let* ( [p (text s)]
          [w (or width (pict-width p))]
          [h (or height (pict-height p))] )
    (cc-superimpose (text s) (rectangle (+ 2 w) h)) ) )
(boxed-string "hello")

(define (boxed-null-byte #:width [width #f] #:height [height #f])
  (boxed-string "\\0" #:width width #:height height) )
(boxed-null-byte)

(define (boxed-char c #:width [width #f] #:height [height #f])
  (boxed-string (string c)) )
(boxed-char #\X)

;; how/when to add a null-byte to the end of the string
;; how to map over a string
(define (string-diagram s)
  (let ( [height (pict-height (text s))] )
    (a #:width [width #f] #:height [height #f]pply hc-append
           (sequence->list (sequence-map (lambda (c) (boxed-char c #:height height))
                                         (in-string (string-append s "\\0")) )) ) ) )
(string-diagram "Hello!")

