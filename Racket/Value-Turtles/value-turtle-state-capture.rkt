#lang racket
(require graphics/value-turtles)

;; Given a list of states
;; each of which is a vector of #(x y orientation)
;; return a list of (dx dy) sublists
;; where the (dx dy) pairs are the differences between the
;; current x and y values and the first x and y values.
;; The first pair will always be (0 0).
(define (state-xy-diffs states [this 'state-xy-diffs])
  (define (v- i v1 v0) (- (vector-ref v1 i) (vector-ref v0 i)))
  (unless (pair? states) (error this "states is null"))
  (let ( [s0 (car states)] )
    (let loop ( [ss states] [accum '()] )
      (if (null? ss)
          (reverse accum)
          (let ( [s1 (car ss)] )
            (let ( [dx (v- 0 s1 s0)] [dy (v- 1 s1 s0)] )
              (loop (cdr ss) (cons (list dx dy) accum)) ) ) ) ) ) )

(define (regular-polygon num-sides side-size [t0 (turtles 600 600)])
  (let ( [angle (/ 360 num-sides)] )
    (let loop ( [n num-sides] [states'()] [t t0] )
      (if (zero? n)
          (values (reverse states) t)
          (loop (sub1 n)
                (cons (car (turtle-state t)) states)
                (turn angle (draw side-size t)) ) ) ) ) )

(regular-polygon 6 100)

(let-values ( [(states _) (regular-polygon 5 40)] )
  (for/list ( [s (state-xy-diffs states)]
              [color (in-cycle (list 'red 'green 'blue))] )
    (cons 'ColorXY (cons color s)) ) )
                               