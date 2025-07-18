#lang typed/racket
(require typed/2htdp/image)

;; What does the type checking in this program
;; - provide for us?
;; - cost us?

(define-type Image-Maker (-> Nonnegative-Real Color Image))
(: disk Image-Maker)
(define (disk size color)
  (circle size 'solid color) )
;; color components are of type Byte, values 0..255
(define max 255)
(define translucent 127)
(define red (color max 0 0 translucent))
(define green (color 0 max 0 translucent))
(define blue (color 0 0 max translucent))

(: three-disks (->  Nonnegative-Real Real Image-Maker Image))
(define (three-disks size displacement image-maker)
  (overlay/offset
   (overlay/offset (image-maker size red)
                   displacement 0
                   (image-maker size green) )
   0 displacement
   (image-maker 30 blue) ) )

(three-disks 30 26 disk)

(: half (-> (U Real Nonnegative-Real) Nonnegative-Real))
(define (half x) (abs (/ x 2)))

(let ( [size 30] [displacement 30] )
  (three-disks size (* displacement 3) (λ (size color) (three-disks size displacement disk))) )

(struct ColorXY ([color : Color] [x : Real] [y : Real]))

(: overlay-images (-> Nonnegative-Real Image-Maker ColorXY * Image))

;; overlay any number of images
;; aligned to their lower left corners
;; displaced by the given dx dy pairs.
(define (overlay-images image-size image-maker . image-data)
  (foldl (λ ([cxy : ColorXY] [image : Image])
           (overlay/align/offset "left" "bottom" image
                                 (ColorXY-x cxy) (ColorXY-y cxy)
                                 (image-maker image-size (ColorXY-color cxy)) ) )
         empty-image
         image-data ) )

(overlay-images 30 disk (ColorXY red 0 0) (ColorXY green 26 0) (ColorXY blue 0 26))

(overlay-images 30 disk
              (ColorXY red 0 0)
              (ColorXY green 40 0)
              (ColorXY blue 52.36067977499789 -38.042260651806146)
              (ColorXY red 20.0 -61.55367074350508)
              (ColorXY green -12.360679774997891 -38.042260651806146) )
