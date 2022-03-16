#lang slideshow

(define (boxed-pict p #:width [width #f] #:height [height #f] #:skosh [extra-width 0])
  (let* ( [p-w (pict-width p)]
          [p-h (pict-height p)]
          [w (if (and width (> width p-w)) width p-w)]
          [h (if (and height (> height p-h)) height p-h)]
          [picture (inset p (- w p-w) (- h p-h))] )
    (cc-superimpose picture (rectangle (+ extra-width w) h)) ) )
(boxed-pict (text "hello\\0"))

(define (max-picts-width-height pict-seq)
  (let ( [max-width 0] [max-height 0] )
    (sequence-map (λ (p)
                    (let ( [w (pict-width p)] [h (pict-height p)] )
                      (when (> w max-width) (set! max-width w))
                      (when (> h max-height) (set! max-height h)) ) ) pict-seq)
    (values max-width max-height) ) )

; Explode a string into a sequence of characters
; and transform those into a sequence of picts
(define (picts-explode-string s)
  (sequence-map (λ (c) (text (string c))) (in-string s)) )

; Explode a string into a sequence of boxed characters
; of the same width and height
; - the max of the sizes of their constituents
; - plus a skosh around the width inside the boxes
(define (string-diagram str #:null [add-null #f])
  (let* ( [s (if add-null (string-append str "\\0") str)]
          [picts (picts-explode-string s)] )
    (let-values (  [(w h) (max-picts-width-height picts)] )
      (let ( [boxed-picts (sequence-map (λ (p) (boxed-pict #:width w #:height #:skosh 2 h p)) picts)] )
        (apply hc-append (sequence->list boxed-picts)) ) ) ) )
(string-diagram "Hello!" #:null #t)

