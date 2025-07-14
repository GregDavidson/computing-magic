#lang slideshow
;; Make a Face
;; Etienne c-gamer

;; This second refactoring may seem to make things worse!
;; Have courage, it will start getting better after this!

;; Second refactoring:
;; - Abstraction:
;;   - Turn fixed sizes into ratios
;;   - Be explicit about what the ratios are ratios of!
;;   - i.e. width-to-height or whatever!
;; - Renaming:
;;   - Currently spelling out width and height
;;   - Might later abbreviate to wt and ht
;; - Tree Shaking:
;;   - Unused code removed from this version
;;   - Still preserved in earlier version!

;; bring in another library for color
(require pict/color)

;; The Parameters

;; How should feature sizes scale with head and face sizes?
;; - Height, width, area or more complex functions?
;; Let's experiment and observe the effect for different sizes!
;; - Face Height = Head Height
;; - Have everything else scale by either face-height or face-width

(define head-height 189)

(define face-color (dark "goldenrod"))
(define face-width-to-height (/ 162 189)) ; face width to face height ratio
(define face-width (* face-width-to-height head-height))
(define face-height head-height)

(define ear-color "brown")
(define ear-height-to-head-height (/ 60 189))
(define ear-width-to-height (/ 14 60))
(define ear-height (* ear-height-to-head-height head-height))
(define ear-width (* ear-height ear-width-to-height))
(define upper-disk-to-ear-width (/ 16 14)) ; upper ear disk size to ear width ratio
(define lower-disk-to-ear-width (/ 21 14)) ; lower ear disk size to ear width ratio
(define upper-ear-disk-size (* upper-disk-to-ear-width ear-width))
(define lower-ear-disk-size (* lower-disk-to-ear-width ear-width))

(define eye-color (light (light "brown")))
(define eye-width-to-face-width (/ 50 162)) ; eye width to face width ratio
(define eye-height-to-width (/ 30 50))      ; eye height to eye width ratio
(define eye-width (* eye-width-to-face-width face-width))
(define eye-height (* eye-height-to-width eye-width))
(define iris-size-to-eye-height .85)
(define pupil-ratio .45)                ; pupil to iris ratio
(define eye-spacing-to-face-width (/ 25 162))
(define eye-spacing (* eye-spacing-to-face-width face-width))

(define nose-color "brown")
(define upper-nose-height-to-face-height (/ 50 189))
(define upper-nose-width-to-height (/ 30 50))
(define upper-nose-height (* upper-nose-height-to-face-height face-height))
(define upper-nose-width (* upper-nose-width-to-height upper-nose-height))
(define lower-nose-width-to-face-width (/ 50 162))
(define lower-nose-height-to-width (/ 20 50))
(define lower-nose-width (* lower-nose-width-to-face-width face-width))
(define lower-nose-height (* lower-nose-height-to-width lower-nose-width))
(define nostril-size-to-lower-nose-height (/ 15 20))
(define nostril-size (* nostril-size-to-lower-nose-height lower-nose-height))

(define mouth-color (dark "red"))
(define nose-mouth-spacing-to-face-height (/ 12 189))
(define nose-mouth-spacing 12)
(define mouth-width-to-face-width (/ 80 162))
(define mouth-height-to-face-height (/ 20 189))
(define mouth-width (* mouth-width-to-face-width face-width))
(define mouth-height (* mouth-height-to-face-height face-height))
(define lip-width-to-mouth-width (/ 75 80))
(define lip-width (* lip-width-to-mouth-width mouth-width))
(define lip-height (/ mouth-height 2))

;;; Building A Face

;; eyes

(define (eye2 w h
              #:iris [iris .7]
              #:pupil [pupil .5]
              #:color [color (light "aquamarine")] )
  (let ( [iris-size (* iris h)] )
    (cc-superimpose [filled-ellipse w h #:color "white"]
                    [disk iris-size #:color color]
                    [disk (* iris-size pupil)] ) ) )


(define tribal-eye
  (eye2 eye-width eye-height #:color eye-color
        #:iris iris-size-to-eye-height #:pupil pupil-ratio ) )
(define tribal-eyes (hc-append eye-spacing tribal-eye tribal-eye))

;; nose

(define lower-nose-part-1
  (lb-superimpose (filled-ellipse lower-nose-width lower-nose-height #:color nose-color)
                  (disk nostril-size) ) )
(define lower-nose (rb-superimpose lower-nose-part-1
                                   (disk nostril-size) ) )
(define tribal-nose (cb-superimpose
                     (filled-ellipse upper-nose-width upper-nose-height #:color nose-color)
                     lower-nose ) )

;; eyes and nose

(define tribal-eyes-and-nose (vc-append tribal-eyes tribal-nose))

;; mouth

(define tribal-lips
  (vc-append (filled-ellipse lip-width lip-height #:color mouth-color)
             (filled-ellipse lip-width lip-height #:color mouth-color) ) )
(define tribal-mouth
  (cc-superimpose (filled-ellipse mouth-width mouth-height #:color mouth-color)
                  tribal-lips ) )

;; eyes, nose, mouth

(define tribal-face-contents-1
  (vc-append nose-mouth-spacing tribal-eyes-and-nose tribal-mouth) )

;; face with eyes, nose, mouth

(define tribal-face-1
  (cc-superimpose (filled-ellipse face-width face-height #:color face-color)
                  tribal-face-contents-1 ) )

;; ears

(define tribal-left-ear-1
  (vr-append (disk upper-ear-disk-size #:color ear-color)
             (filled-ellipse ear-width ear-height #:color ear-color)
             (disk lower-ear-disk-size #:color ear-color) ) )
(define tribal-right-ear-1
  (vl-append (disk upper-ear-disk-size #:color ear-color)
             (filled-ellipse ear-width ear-height #:color ear-color)
             (disk lower-ear-disk-size #:color ear-color) ) )

;; head = face with ears

(define tribal-head-1
  (hc-append tribal-left-ear-1 tribal-face-1 tribal-right-ear-1) )

;; Show a result

tribal-head-1
