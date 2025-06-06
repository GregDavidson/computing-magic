#lang racket

(require racket/draw)

(define the-dc (new pdf-svg% [output "hello-world-diagram.svg"]))

(require metapict)

;; (def N 18)
;; (set-curve-pict-size 300 300)
;; (define the-picture
;;   (with-window (window -0.1 1.1 -0.1 1.1)
;;     (defv (A B C) (values (pt 0 0) (pt@d 1 60) (pt 1 0)))
;;     (first-value
;;      (for/fold ([drawing (draw)] [A A] [B B] [C C]) ([n N])
;;        (def triangle (curve A -- B -- C -- cycle))
;;        (def shade    (color-med (expt (/ n N) 0.4) "red" "yellow"))
;;        (def filled   (color shade (fill triangle)))
;;        (values (draw drawing filled triangle)
;;                (med 0.12 A B) (med 0.12 B C) (med 0.12 C A) ) ) ) ) )

; Draw a character in a box
(define (char-diagram c)
  )

;; how/when to add a null-byte to the end of the string
;; how to map over a string
(define (string-diagram s)
  (apply hc-append (map char-diagram s))
)

(send the-dc start-doc "Working on it...")
(send the-dc start-page)
(draw-pict the-picture the-dc 0 0)
(send the-dc end-page)
(send the-dc end-doc)
