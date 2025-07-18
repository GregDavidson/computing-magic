#lang typed/racket
(require typed/2htdp/image)

;; What does the type checking in this program
;; - provide for us?
;; - cost us?

(: disk (->* () (#:size Nonnegative-Real #:color Color) Image))
(define (disk #:size [size 30] #:color [color (color 0 0 0 0)])
  (circle size 'solid color) )
;; color components are of type Byte, values 0..255
(define max 255)
(define translucent 127)
(define red (color max 0 0 translucent))
(define green (color 0 max 0 translucent))
(define blue (color 0 0 max translucent))

(overlay/offset
 (overlay/offset (disk #:color red)
                 26 0
                 (disk #:color green) )
 0 26
 (disk #:color blue) )

(foldl (λ ([color : Color] [x-offset : Real] [y-offset : Real] [image : Image])
         (overlay/offset image x-offset y-offset (disk #:color color)) )
       empty-image
       (list red green blue)
       (list 0 26 0)
       (list 0 0 26) )

(struct ColorXY ([color : Color] [x : Real] [y : Real]))

(foldl (λ ([cxy : ColorXY] [image : Image])
         (overlay/offset image (ColorXY-x cxy) (ColorXY-y cxy) (disk #:color (ColorXY-color cxy))) )
       empty-image
       (list (ColorXY red 0 0) (ColorXY green 26 0) (ColorXY blue 0 26)) )