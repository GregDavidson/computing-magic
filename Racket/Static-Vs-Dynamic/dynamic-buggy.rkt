#lang racket
(require 2htdp/image)

(define (disk size color)
  (circle size 'solid color) )
;; color components are integers in the range 0..255
;; giving degrees of red, green, blue, alpha (transparency)
(define red (color 255 0 0 127))
(define green (color 0 255 0 127))
(define blue (color 0 0 255 127))

;; this does what we want, but is buggy
(overlay/offset
 (overlay/offset (disk 30 red)
                 26 0
                 (disk 30 green) )
 0 26
 (disk 30 blue) )

;; this doesn't quite work -- why? -- how to fix?
(foldl (λ (color x-offset y-offset image)
         (overlay/offset image x-offset y-offset (disk 30 color)) )
       empty-image
       '(red green blue)
       '(0 26 0)
       '(0 0 26) )

;; this doesn't quite work -- why? -- how to fix?
(foldl (λ (cxy image)
         (overlay/offset image (second cxy) (third cxy) (disk 30 (first cxy))) )
       empty-image
       '((red 0 0) (green 26 0) (blue 0 26)) )