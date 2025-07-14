#lang slideshow
;; Make a Face
;; Etienne c-gamer

;;bring in another library for color
(require pict/color)

(define left_ear(vc-append [ellipse 10 60] [circle 9]))
(define (eye1 w h iris)
  (hc-append [ellipse w h] [disk (* iris h)] [ellipse w h]))
(define eye (eye1 30 20 4/5))
(define nose-mouth(vc-append [ellipse 30 70] [ellipse 60 10]))
(define face(ht-append 12 left_ear eye nose-mouth eye left_ear))
(define (eye2 w h
              #:iris [iris .7]
              #:pupil [pupil .5]
              #:color [color (light "aquamarine")])
  (let ([iris-size (* iris h)])
   (cc-superimpose
    [filled-ellipse w h #:color "white"]
    [disk iris-size #:color color]
    [disk (* iris-size pupil)] ) ) )

(define eye-realistic(cc-superimpose [ellipse 80 20] [disk 20 #:color "Chartreuse" #:border-color "Medium Aquamarine" #:border-width 2] [disk 12]))
(define faceWRealEye(ht-append 12 left_ear eye-realistic nose-mouth eye-realistic left_ear))
(define eye-realistic-brown(cc-superimpose [ellipse 80 20] [disk 20 #:color "Brown" #:border-color "Goldenrod" #:border-width 2] [disk 12]))
(define faceWRealBrownEye(ht-append 12 left_ear eye-realistic-brown nose-mouth eye-realistic-brown left_ear))

;;;;;;;;;
(define eye-custom1(eye2 50 30 #:color(light(light "brown")) #:iris .85 #:pupil .45))
(define two-eyes(hc-append 25 eye-custom1 eye-custom1))
(define noseP1 (lb-superimpose (filled-ellipse 50 20 #:color "brown") (disk 15)))
(define noseP2 (rb-superimpose noseP1 (disk 15)))
(define noseP3 (cb-superimpose (filled-ellipse 30 50 #:color "brown") noseP2))

(define customNoseAndEye(vc-append two-eyes noseP3))
(define mouthAfrican1(vc-append (filled-ellipse 75 10 #:color (dark "red")) (filled-ellipse 75 10 #:color (dark "red"))))
(define mouthAfrican1p2(cc-superimpose (filled-ellipse 80 20 #:color (dark "red")) mouthAfrican1))
(define africanFaceLift1(vc-append 12 customNoseAndEye mouthAfrican1p2))
(define africanFace1(cc-superimpose(filled-ellipse 162 189 #:color(dark "goldenrod")) africanFaceLift1))

(define africanLeftEar1(vr-append(disk 16 #:color "brown") (filled-ellipse 14 60 #:color "brown") (disk 21 #:color "brown")))
(define africanRightEar1(vl-append(disk 16 #:color "brown") (filled-ellipse 14 60 #:color "brown") (disk 21 #:color "brown")))
(define africanHeadBase1(hc-append africanLeftEar1 africanFace1 africanRightEar1))

;; Show a result
africanHeadBase1