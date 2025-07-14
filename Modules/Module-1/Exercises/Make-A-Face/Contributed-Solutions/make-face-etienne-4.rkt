#lang slideshow
;; Make a Face
;; Etienne c-gamer

;; Third refactoring:
;; - Abstraction:
;;   - Turn expressions into functional procedures!
;; What becomes possible?
;;   - Easier experimentation with Tribal Heads
;;   - Other head designs: Animal, Anime, etc.
;;   - Incorporating heads into scaled bodies
;;   - Etc., etc.!

;; bring in a library for color
(require pict/color)

;;; Building A Face

;; Generic Functions

(define (make-head head-height mk-face mk-ear)
  (hc-append (mk-ear head-height 'left)
             (mk-face head-height)
             (mk-ear head-height 'right) ) )

(define (make-face height mk-eye mk-nose mk-mouth
                   #:color [color "white"]
                   #:width-to-height [width-to-height (/ 162 189)]
                   #:eye-spacing-ratio [eye-spacing-to-width (/ 25 162)]
                   #:nose-mouth-ratio [nose-mouth-to-height (/ 12 189)] )
  (let* ( [width (* width-to-height height)]
          [eye-spacing (* eye-spacing-to-width width)]
          [nose-mouth-spacing (* nose-mouth-to-height height)] )
    (cc-superimpose
     (filled-ellipse width height #:color color)
     (vc-append nose-mouth-spacing
                (vc-append
                 (hc-append eye-spacing (mk-eye width height 'left) (mk-eye width height 'right))
                 (mk-nose width height) )
                (mk-mouth width height) ) ) ) )

(define (make-eye w h
              #:iris [iris .7]
              #:pupil [pupil .5]
              #:color [color (light "aquamarine")] )
  (let ( [iris-size (* iris h)] )
    (cc-superimpose [filled-ellipse w h #:color "white"]
                    [disk iris-size #:color color]
                    [disk (* iris-size pupil)] ) ) )

;; Tribal Functions

(define (make-tribal-eye face-width face-height which-eye
                         #:color [color (light (light "brown"))]
                         #:width-to-face [width-to-face (/ 50 162)]
                         #:height-to-width [height-to-width (/ 30 50)]
                         #:iris-ratio [iris-size-to-height .85]
                         #:pupil-ratio [pupil-ratio .45] )
  (let* ( [width (* width-to-face face-width)]
          [height (* height-to-width width)] )
    (make-eye width height #:color color
              #:iris iris-size-to-height #:pupil pupil-ratio ) ) )

(define (make-tribal-nose face-width face-height
                          #:color [color "brown"]
                          #:upper-to-face [upper-height-to-face (/ 50 189)]
                          #:upper-ratio [upper-width-to-height (/ 30 50)]
                          #:lower-to-face [lower-width-to-face (/ 50 162)]
                          #:lower-ratio [lower-height-to-width (/ 20 50)]
                          #:nostril-ratio [nostril-size-to-lower-height (/ 15 20)] )
  (let* ( [upper-height (* upper-height-to-face face-height)]
          [upper-width (* upper-width-to-height upper-height)]
          [lower-width (* lower-width-to-face face-width)]
          [lower-height (* lower-height-to-width lower-width)]
          [nostril-size (* nostril-size-to-lower-height lower-height)]
          [upper-nose (filled-ellipse upper-width upper-height #:color color)]
          [lower-nose (rb-superimpose
                       (lb-superimpose
                        (filled-ellipse lower-width lower-height #:color color)
                        (disk nostril-size) )
                       (disk nostril-size) )] )
    (cb-superimpose upper-nose lower-nose ) ) )

(define (make-tribal-mouth face-width face-height
                           #:color [color (dark "red")]
                           #:width-to-face [width-to-face (/ 80 162)]
                           #:height-to-face [height-to-face (/ 20 189)]
                           #:lip-width-ratio [lip-width-to-mouth (/ 75 80)] )
  (let* ( [width (* width-to-face face-width)]
          [height (* height-to-face face-height)]
          [lip-width (* lip-width-to-mouth width)]
          [lip-height (/ height 2)]
          [lip (filled-ellipse lip-width lip-height #:color color)] )
    (cc-superimpose
     (filled-ellipse width height #:color color)
     (vc-append lip lip) ) ) )

(define (make-tribal-face height)
  (make-face height
             make-tribal-eye make-tribal-nose make-tribal-mouth
             #:color (dark "goldenrod") ) )

(define (make-tribal-ear face-height [which-ear #f]
                         #:color [color "brown"]
                         #:height-to-face [height-to-face (/ 60 189)]
                         #:width-to-height [width-to-height (/ 14 60)]
                         #:upper-disk-ratio [upper-disk-ratio (/ 16 14)]
                         #:lower-disk-ratio [lower-disk-ratio (/ 21 14)] )
  (let* ( [height (* height-to-face face-height)]
          [width (* height width-to-height)]
          [upper-disk-size (* upper-disk-ratio width)]
          [lower-disk-size (* lower-disk-ratio width)] )
    ( (cond ((eq? 'left which-ear) vr-append)
            ((eq? 'right which-ear) vl-append)
            (#t vc-append) )
      (disk upper-disk-size #:color color)
      (filled-ellipse width height #:color color)
      (disk lower-disk-size #:color color) ) ) )

;; Show a result

(make-head 189 make-tribal-face make-tribal-ear)
