#lang slideshow
;; Make a Face
;; Playing with Jeff
;;bring in another library for color
(require pict/color)

(define (make-eye size
                  #:eye-ratio [ratio 1/2] #:eye-color [color "blue"]
                  #:sclera-color [sclera (light "yellow")]
                  #:pupil-ratio [pupil 1/3] #:iris-ratio [iris 1/3])
  (cc-superimpose (filled-ellipse size (* ratio size) #:color sclera)
                  (disk (* size pupil) #:color color)
                  (disk (* size pupil iris)) ) )

#;(define make-eye
  (λ (size
      #:eye-ratio [ratio 1/2] #:eye-color [color "blue"]
      #:sclera-color [sclera (light "yellow")]
      #:pupil-ratio [pupil 1/3] #:iris-ratio [iris 1/3])
    (cc-superimpose (filled-ellipse size (* ratio size) #:color sclera)
                    (disk (* size pupil) #:color color)
                    (disk (* size pupil iris)) ) ) )

(make-eye 100)