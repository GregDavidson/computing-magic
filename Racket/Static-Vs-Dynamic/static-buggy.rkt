#lang typed/racket
(require typed/2htdp/image)

(: disk (-> Nonnegative-Real Color Image))
(define (disk size color)
  (circle size 'solid color) )
;; color components are of type Byte, values 0..255
(define max 255)
(define translucent 127)
(define red (color max 0 0 translucent))
(define green (color 0 max 0 translucent))
(define blue (color 0 0 max translucent))

(overlay/offset
 (overlay/offset (disk 30 red)
                 26 0
                 (disk 30 green) )
 0 26
 (disk 30 blue) )

(foldl (λ ([color : Color] [x-offset : Real] [y-offset : Real] [image : Image])
         (overlay/offset image x-offset y-offset (disk 30 color)) )
       empty-image
       #; '(red green blue) ; doesn't compile! why?
       (list red green blue) ; works!
       '(0 26 0)
       '(0 0 26) )
