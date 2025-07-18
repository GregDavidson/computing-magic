#lang typed/racket
(require typed/2htdp/image)

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
       #; '(red green blue) ; doesn't compile! why?
       (list red green blue) ; works!
       '(0 26 0)
       '(0 0 26) )

(foldl (λ ([cxy : (List Color Real Real)] [image : Image])
         (overlay/offset image (second cxy) (third cxy) (disk #:color (first cxy))) )
       empty-image
       #; '((red 0 0) (green 26 0) (blue 0 26)) ; doesn't compile! why?
       `((,red 0 0) (,green 26 0) (,blue 0 26)) ; works!
       )