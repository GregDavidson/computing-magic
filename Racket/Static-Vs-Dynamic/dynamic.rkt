#lang racket
(require 2htdp/image)

(define (disk #:size [size 30] #:color [color "black"])
  (circle size 'solid color) )
;; color components are integers in the range 0..255
(define max 255)
(define translucent 127)
(define red (color max 0 0 translucent))
(define green (color 0 max 0 translucent))
(define blue (color 0 0 max translucent))

(overlay/offset
 (overlay/offset (disk #:color 'red)
                 26 0
                 (disk #:color 'green) )
 0 26
 (disk #:color 'blue) )

(foldl (λ (color x-offset y-offset image)
         (overlay/offset image x-offset y-offset (disk #:color color)) )
       empty-image
       (list red green blue)
       (list 0 26 0)
       (list 0 0 26) )

(struct/contract ColorXY ([color color?] [x real?] [y real?]))

(foldl (λ (cxy image)
         (overlay/offset image (ColorXY-x cxy) (ColorXY-y cxy) (disk #:color (ColorXY-color cxy))) )
       empty-image
       (list (ColorXY red 0 0) (ColorXY green 26 0) (ColorXY blue 0 26)) )