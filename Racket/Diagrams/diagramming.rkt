#lang slideshow
(require racket/format)

;; * diapic - a Racket diagramming toolkit

; Shall we use lists to structure our diagrams?
; Shall we use a purely functional approach?
; Shall we use typed/racket?
; Or shall we be more eclectic?
; Let's do everything 2 or 3 ways and learn!

; Wrap a pic in a box
; Either sized just right or of the specified sizes
; and perhaps with a label outside of the box.
; Without explicit sizing,
;   the box will be sized to the interior picture
;   and will ignore the size of the label.
(define (boxed-pic p #:width [width #f] #:height [height #f] #:skosh [extra-width 0] #:label [label #f])
  (let* ( [and-max (λ (i j) (if (and i (> i j)) i j))]
          [w (and-max width (pict-width p))]
          [h (and-max height (pict-height p))]
          [box (cc-superimpose p (rectangle (+ extra-width w) h))] )
    (if label (vl-append box (text label)) box) ) )
; Mystery: When the vl-append arguments are reversed, the top line
; of the boxes does not show, even in tests where there should be no label!

(define (test-boxed-pic [p (text "Hello world!")] #:width [width #f] #:height [height #f] #:skosh [extra-width 0] #:label [label #f])
  (boxed-pic p  #:width #f #:height #f #:skosh 3 #:label "value") )

(test-boxed-pic)

; Append a list of pictures into a new picture
(define (append-list pics #:append [f hc-append] #:skosh [extra-width 0])
  (apply f (cons extra-width pics)) )

; Append a sequence of pictures into a new picture
(define (append-seq pics #:append [f hc-append] #:skosh [extra-width 0])
  (append-list (sequence->list pics) #:append f #:skosh extra-width) )

; Find the maximum width and height of a sequence of pictures
(define (max-pic-seq-width-height pic-seq #:width [width 0] #:height [height 0])
  (sequence-for-each
   (λ (p) (let ( [w (pict-width p)] [h (pict-height p)] )
            (when (> w width) (set! width w))
            (when (> h height) (set! height h)) ) )
   pic-seq )
  (values width height) )

(define (test-max-pic-seq-width-height [pic-seq (in-list (list (circle 10)))] #:width [width 0] #:height [height 0])
  (let-values ( [(w h) (max-pic-seq-width-height pic-seq)] )
    (printf "width ~a height ~a\n" w h) ) )

(test-max-pic-seq-width-height)

; Find the maximum width and height of a list of pictures
(define (max-pic-list-width-height pics #:width [width 0] #:height [height 0])
  (define (max i j) (if (> i j) i j))
  (if (null? pics)
      (values width height)
      (max-pic-list-width-height
       (cdr pics)
       #:width (max width (pict-width (car pics)))
       #:height (max height (pict-height (car pics))) ) ) )

(define (test-max-pic-list-width-height [pics (list (circle 10))] #:width [width 0] #:height [height 0])
  (let-values ( [(w h) (max-pic-list-width-height pics)] )
    (printf "width ~a height ~a\n" w h) ) )

(test-max-pic-list-width-height)

; Convert a sequence to a list
; Possibly transforming through a mapping function
; which might possibly want an index value
; Possibly appending to an existing list
(define (seq-to-list seq #:map [f #f] #:tail [tail '()] #:index [index #f])
  (stream-to-list (sequence->stream seq) #:map f #:tail tail #:index index) )

(define (stream-to-list ss #:map [f #f] #:tail [tail '()] #:index [index #f])
  (if (stream-empty? ss)
      tail
      (let ( [first (stream-first ss)] [rest (stream-rest ss)] )
        (cons
         (if f (if index (f first index) (f first)) first)
         (stream-to-list rest #:map f #:tail tail #:index (and index (+ 1 index))) ) ) ) )

; Explode a string into a sequence
; Possibly transforming through a mapping function
; Possibly appending to an existing sequence
(define (seq-explode-string s #:map [f #f] #:tail [tail #f])
  (let* ( [chars (in-string s)]
          [mapped-chars (if f (sequence-map f chars) chars)]
          [joined-chars (if tail (sequence-append mapped-chars tail) mapped-chars) ] )
          joined-chars ) )
(append-seq (seq-explode-string
             "Hello!"
             #:map (λ (c) (boxed-pic (text (string c)) #:skosh 3))
             #:tail (in-list (list (boxed-pic (text "\\0") #:skosh 3))) ))

; Explode a string into a list
; Possibly transforming through a mapping function
; Possibly appending to an existing list
(define (list-explode-string s #:map [f #f] #:tail [tail #f])
  (seq-to-list (in-string s) #:map f #:tail tail) )
(append-list (list-explode-string "Hello!"
                                  #:map (λ (c) (boxed-pic (text (string c)) #:skosh 3))
                                  #:tail (list (boxed-pic (text "\\0") #:skosh 3)) ))

; Explode a string into a list of boxed characters
; of the same width and height
; - the max of the sizes of their constituents
; - plus a skosh around the width inside the boxes
(define (boxed-chars-list str #:null [add-null #f] #:index [index #f])
  (let* ( [pic-seq (seq-explode-string str #:map (λ (c) (text (string c))))]
          [tail (if add-null (in-list (list (text "\\0"))) #f)]
          [elements (if tail (sequence-append pic-seq tail) pic-seq)] )
    (let-values ( [(w h) (max-pic-seq-width-height elements)] )
      (let ( [f (if index
                    (λ (p i) (boxed-pic p #:width w #:height h #:skosh 2 #:label (~a i)))
                    (λ (p) (boxed-pic p #:width w #:height h #:skosh 2)) )] )
        (seq-to-list elements #:map f #:index index) ) ) ) )

(define (string-diagram s #:null [add-null #f] #:index [index #f])
  (append-list (boxed-chars-list s #:null add-null #:index index)) )

(string-diagram "Hello!" #:null #t)
(string-diagram "Hello!" #:null #t #:index 0)
