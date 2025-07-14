#lang slideshow
;; Make a Face
;; Etienne c-gamer

;; First refactoring:
;; - Working from bottom to top
;; - More whitespace for clarity
;; - Abstraction: Arguments (parameter values) become global bindings
;; - Renaming:
;;   - kebab-case instead of camelCase or snake_case
;;   - change to tribal for politeness
;; - Tree Shaking:
;;   - Commenting out code not used for tribal-head-1
;;   - Nothing removed!

;; bring in another library for color
(require pict/color)

;; The Parameters

(define ear-color "brown")
(define ear-width 14)
(define ear-height 60)
(define upper-ear-disk-size 16)
(define lower-ear-disk-size 21)

(define eye-color (light (light "brown")))
(define eye-width 50)
(define eye-height 30)
(define iris-ratio .85)
(define pupil-ratio .45)
(define eye-spacing 25)

(define nose-color "brown")
(define upper-nose-width 30)
(define upper-nose-height 50)
(define lower-nose-width 50)
(define lower-nose-height 20)
(define nostril-size 15)

(define mouth-color (dark "red"))
(define nose-mouth-spacing 12)
(define lip-width 75)
(define lip-height 10)
(define mouth-width 80)
(define mouth-height 20)

(define face-color (dark "goldenrod"))
(define face-width 162)
(define face-height 189)

;;; Explorations not used in tribal face

#;(define left-ear
  (vc-append [ellipse ear-width ear-height] [circle ear-lobe-size]) )
#;(define (eye1 w h iris)
  (hc-append [ellipse w h] [disk (* iris h)] [ellipse w h]))
#;(define eye (eye1 30 20 4/5))
#;(define nose-mouth(vc-append [ellipse 30 70] [ellipse 60 10]))
#;(define face(ht-append 12 left-ear eye nose-mouth eye left-ear))

#;(define eye-realistic (cc-superimpose [ellipse 80 20]
                                      [disk 20
                                            #:color "Chartreuse"
                                            #:border-color "Medium Aquamarine"
                                            #:border-width 2 ]
                                      [disk 12] ) )

#;(define faceWReal-eye
  (ht-append 12 left-ear eye-realistic nose-mouth eye-realistic left-ear) )
#;(define eye-realistic-brown
  (cc-superimpose [ellipse 80 20]
                  [disk 20 #:color "Brown" #:border-color "Goldenrod" #:border-width 2]
                  [disk 12] ) )
#;(define faceWRealBrown-eye
  (ht-append 12 left-ear eye-realistic-brown nose-mouth eye-realistic-brown left-ear) )

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
        #:iris iris-ratio #:pupil pupil-ratio ) )
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
