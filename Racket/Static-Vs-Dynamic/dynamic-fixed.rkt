#lang racket
(require 2htdp/image)

;; Once you understand why and how the fixes work,
;; - what do you think of the fixes?
;; - can you make it better?

(define (disk size color)
  (circle size 'solid color) )
;; color components are integers in the range 0..255
(define max 255)
(define translucent 127)
(define red (color max 0 0 translucent))
(define green (color 0 max 0 translucent))
(define blue (color 0 0 max translucent))

;; this does what we want, but is a bit ugly
(overlay/offset
 (overlay/offset (disk 30 red)
                 26 0
                 (disk 30 green) )
 0 26
 (disk 30 blue) )

(foldl (λ (color x-offset y-offset image)
         (overlay/offset image x-offset y-offset (disk 30 color)) )
       empty-image
       #; '(red green blue) ; this doesn't quite work -- why?
       `(,red ,green ,blue) ; this works -- why?
       '(0 26 0)
       '(0 0 26) )

;; this doesn't quite work -- why? -- how to fix?
(foldl (λ (cxy image)
         (overlay/offset image (second cxy) (third cxy) (disk 30 (first cxy))) )
       empty-image
       #; '((red 0 0) (green 26 0) (blue 0 26))   ; this doesn't quite work -- why?
       `((,red 0 0) (,green 26 0) (,blue 0 26)) ) ; this works -- why?