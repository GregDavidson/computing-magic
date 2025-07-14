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

;; ** How can we draw a better nose?
;; Good noses in 2D require art + tech!

;; Tech:
;; 1. Use lower-level racket/draw library
;; 2. Use Bézier curves in Racket MetaPict library

;; *** Method 1: Use racket/draw
;; Explicit dc (drawing context) gives fine control
;; see: https://docs.racket-lang.org/draw/index.html

(require racket/draw)

(define (make-fancy-triangle w h)
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send dc set-brush
              (new brush% [style 'fdiagonal-hatch]
                   [color "darkslategray"] ) )
        (send dc set-pen
              (new pen% [width 3] [color "slategray"]) )
        (define path (new dc-path%))
        (send path move-to 0 0)
        (send path line-to 50 0)
        (send path line-to 25 50)
        (send path close)
        (send dc draw-path path dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen) )
      w h ) )

;; racket/draw objects are compatible with regular picts

(hc-append 30 (make-fancy-triangle 50 50) (make-fancy-triangle 50 50))

;; *** Method 2: Use MetaPict Bézier Curves
;; see: https://docs.racket-lang.org/metapict/index.html#%28part._ref-bez%29

(require metapict)

;; Play with the points to get the desired shape
;; then scale the points and window with size;
;; consider using MetaPict vec and pt+ procedures.
(define (bezier-blob size)
  (let* ( [p0 (pt 0   0)]
          [p1 (pt 60 40)]
          [p2 (pt 40 90)]
          [p3 (pt 10 70)]
          [p4 (pt 30 50)] )
    (with-window (window -20 100 -20 100)
      (draw (curve p0 .. p1 .. p2 .. p3 .. p4 .. cycle)) ) ) )

;; Metapict objects are still picts:

(hc-append (bezier-blob 10) (bezier-blob 10))
