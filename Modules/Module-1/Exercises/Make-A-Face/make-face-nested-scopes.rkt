#lang slideshow

;; * Make A Face

;; Names in local scopes don't have to be
;; as descriptive as names in global scopes
;; since their usage context is smaller.

(define (make-face face-height
                   #:ratio [face-ratio 3/4]
                   #:color [color "tan"]
                   #:border-width [border-width 2]
                   #:nose-mouth-ratio [nose-mouth-ratio 1/8]
                   #:content-ratio [content-ratio 1/3]
                   #:face-eye-ratio [face-eye-ratio 1/7]
                   #:eye-ratio [eye-ratio 2]
                   #:eye-spacing-ratio [eye-spacing-ratio 1/3]
                   #:iris [iris-ratio 4/5]
                   #:pupil [pupil-ratio 2/5]
                   #:eye-color [eye-color "brown"]
                   #:sclera-color [sclera-color "lightyellow"]
                   #:nose-ratio [nose-ratio 1/8]
                   #:eye-nose-spacing [eye-nose-spacing 0]
                   #:face-mouth-ratio [face-mouth-ratio 1/8]
                   #:mouth-ratio [mouth-ratio 1/4]
                   #:mouth-color [mouth-color "red"] )

  (define (make-eye eye-height)
    (let* ( [eye-width (* eye-ratio eye-height)]
            [iris-size (* iris-ratio eye-height)]
            [pupil-size (* pupil-ratio iris-size)] )
      (cc-superimpose
       (filled-ellipse eye-width eye-height #:color sclera-color)
       (disk iris-size #:color eye-color)
       (disk pupil-size #:color "black") ) ) )

  (define (make-eyes eye-height)
    (let ( [eye (make-eye eye-height)] )
      (hc-append (* eye-spacing-ratio eye-ratio eye-height) eye eye) ) )

  (define (make-nose size) (disk size))

  (define (make-mouth width)
    (filled-ellipse width (* mouth-ratio width) #:color mouth-color) )

  (define (make-eyes+nose)
    (vc-append eye-nose-spacing
               (make-eyes (* face-height face-eye-ratio))
               (make-nose (* face-height nose-ratio)) ) )

  (define (make-eyes+nose+mouth)
    (let* ( [spacing (* nose-mouth-ratio face-height)] )
      (vc-append spacing
                 (make-eyes+nose)
                 (make-mouth (* face-height face-mouth-ratio) ) ) ) )

  ;; Body of make-face
  (let* ( [width (* face-ratio face-height)]
          [content (make-eyes+nose+mouth)]
          [margin (/ (- width (pict-width content)) 2)] )
    (pin-over
     (filled-ellipse width face-height #:color color #:border-width border-width)
     margin (* content-ratio face-height) content ) ) )

;; Example Call
(make-face 100)
