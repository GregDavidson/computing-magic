#lang racket
(require 2htdp/image)
(define (sierpinski levels [base (λ () (triangle 2 'solid 'red))])
  (if (zero? levels)
    (base)
    (let ([t (sierpinski (- levels 1))])
      (freeze (above t (beside t t))) ) ) )