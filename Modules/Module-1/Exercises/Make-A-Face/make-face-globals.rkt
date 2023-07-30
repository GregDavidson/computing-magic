#lang slideshow

;; * Make A Face

;; Result of interactive development process
;; all bindings are still global.

;;bring in a simple library for color
(require pict/color)

;; ** Global Bindings

;; Define defaults for eye procedures
(define eye-ratio 2)
(define eye-iris-ratio 4/5)
(define eye-pupil-ratio 2/5)
(define eye-color "brown")
(define eye-sclera-color "lightyellow")
(define eye-spacing-ratio 1/3)
;; Define defaults for eyes+nose+mouth
(define eye-nose-spacing 0)
(define face-eye-ratio 1/7)
(define face-nose-ratio 1/8)
(define face-mouth-ratio 1/3)
(define face-nose-mouth-ratio 1/8)
(define face-content-ratio 1/3)
(define mouth-ratio 1/4)
(define mouth-color "red")
;; Define defaults for face
(define face-color "tan")
(define face-border-width 2)
(define face-border-color "black")
(define face-ratio 3/4)

;; ** Global Functions

(define (make-eye eye-height)
  (let* ( [eye-width (* eye-ratio eye-height)]
          [iris-size (* eye-iris-ratio eye-height)]
          [pupil-size (* eye-pupil-ratio iris-size)] )
    (cc-superimpose
     (filled-ellipse eye-width eye-height #:color eye-sclera-color)
     (disk iris-size #:color eye-color)
     (disk pupil-size #:color "black") ) ) )

(define (make-eyes eye-height)
  (let ([eye (make-eye eye-height)])
    (hc-append (* eye-spacing-ratio eye-ratio eye-height) eye eye) ) )

(define (make-nose size) (disk size))

(define (make-eyes+nose face-height)
  (printf "~a\n" `(vc-append ,eye-nose-spacing
                             (make-eyes ,(* face-height face-eye-ratio))
                             (make-nose ,(* face-height face-nose-ratio)) ))
  (vc-append eye-nose-spacing
             (make-eyes (* face-height face-eye-ratio))
             (make-nose (* face-height face-nose-ratio)) ) )
    
(define (make-mouth width)
  (filled-ellipse width (* mouth-ratio width) #:color mouth-color) )

(define (make-eyes+nose+mouth face-height)
  (let* ( [spacing (* face-nose-mouth-ratio face-height)] )
    (vc-append spacing
               (make-eyes+nose face-height)
               (make-mouth (* face-height face-mouth-ratio)) ) ) )

(define (make-face height)
  (let* ( [width (* face-ratio height)]
          [content (make-eyes+nose+mouth height)]
          [margin (/ (- width (pict-width content)) 2)] )
    (printf "~a\n" `(filled-ellipse ,width ,height #:color ,face-color #:border-width ,face-border-width))
    (printf "dx: ~s, dy: ~s\n" margin (* face-content-ratio height))
    (pin-over
     (filled-ellipse width height #:color face-color #:border-width face-border-width)
     margin (* face-content-ratio height) content ) ) )

(make-face 100)
