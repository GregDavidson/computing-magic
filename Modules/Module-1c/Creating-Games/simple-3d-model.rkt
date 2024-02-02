#lang typed/racket
(require pict3d)

;; pict3d
;; https://github.com/jeapostrophe/pict3d/blob/067944944d8c4b24893a48e0bcb188751e264183/README.md
;; Notes:
;;  pict3d seems to be the best Racket 3D drawing library
;;    especially because, like pict, it's functional,
;;  Alas, pict2d is not building with the latest Racket Racket 8.11, end of 2023
;;    and is therefore not included on the official Racket website.
;;  Fortunately, pict3d is building just fine with Racket 8.6 on my Linux Mint platform.

(define cylinder1 (combine (cylinder origin 1/2)
                           (basis 'top (point-at (pos 0 0 1/2) +z)) ))

(define cone1 (cone origin 1/2))

(define rocket1 (join cylinder1 '(top)
                      (combine cone1 (basis 'bot (point-at (pos 0 0 -1/2) +z)))
                      '(bot) ) )

rocket1