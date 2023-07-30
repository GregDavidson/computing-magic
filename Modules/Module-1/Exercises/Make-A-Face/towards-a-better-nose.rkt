#lang slideshow

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
