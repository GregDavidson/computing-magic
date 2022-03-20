#lang slideshow
(require racket/format)

;; * diapic - a Racket diagramming toolkit

; Shall we use lists to structure our diagrams?
; Shall we use a purely functional approach?
; Let's use Streams:
; - Lists are automatically accepted as Streams
; - Arrays, Strings, Sequences & more can be wrapped as Streams

; Shall we use typed/racket?
; Maybe eventuallly, but not yet!

;; ** Calculating Sizes

(define (max1 x1 x2) (if (> x1 x2) x1 x2) )
                          
; Find the maximum width, height, length of a stream of pictures
(define (max-pics-width-height pics #:width [width 0] #:height [height 0] #:len [length 0])
  (if (stream-empty? pics)
      (values width height length)
      (let ( [first (stream-first pics)] [rest (stream-rest pics)] )
        (max-pics-width-height
         rest
         #:width (max1 width (pict-width first))
         #:height (max1 height (pict-height first))
         #:len (+ 1 length)) ) ) )

(define (test-max-pics-width-height [pics (list (circle 10))] #:width [width 0] #:height [height 0])
  (let-values ( [(w h n) (max-pics-width-height pics)] )
    (printf "width ~a height ~a length ~a\n" w h n) ) )

(test-max-pics-width-height)

;; ** Labels for Objects

; We want labels to be able to label
; - a complete object
; - a field of a structure
; - an element of a vector

(define (label-pic labels)
  (cond [(pict? labels) labels]
        [(char? labels) (label-pic (string labels))]
        [(string? labels) (text labels)]
        [(number? labels) (label-pic (~a labels))]
        [(stream? labels) (label-pic (stream-first labels))]
        [t #f] ) )

(define (label-next labels)
  (cond [(number? labels) (+ 1 labels)]
        [(not (stream? labels)) labels]
        [t (sequence-tail labels 1)] ) )

;; ** Composing Objects

; Wrap a pic in a box
; Either sized just right or of the specified sizes
; and perhaps with a label outside of the box.
; Without explicit sizing,
;   the box will be sized to the interior picture
; A wider label may boost the size!
(define (boxed-pic p #:width [width #f] #:height [height #f] #:skosh [extra-width 0] #:label [label #f])
  (let* ( [and-max (λ (i j) (if (and i (> i j)) i j))]
          [gloss (and label (label-pic label))]
          [gloss-width (and gloss (pict-width gloss))]
          [w (and-max gloss-width (and-max width (pict-width p)))]
          [h (and-max height (pict-height p))]
          [box (cc-superimpose p (rectangle (+ extra-width w) h))] )
    (if gloss (vc-append box gloss) box) ) )
; Mystery: When the vl-append arguments are reversed,
; the top line of the boxes does not show,
; even in tests where there should be no label!

(define (test-boxed-pic [p (text "Hello world!")] #:width [width #f] #:height [height #f] #:skosh [extra-width 0] #:label [label #f])
  (boxed-pic p  #:width #f #:height #f #:skosh 3 #:label "value") )

(test-boxed-pic)

; Append a Stream of Pictures into a new composite Picture
(define (append-pics pics #:append [f hc-append] #:skosh [extra-width 0])
  (let ( [args (stream->list pics)] )
    (apply f (if extra-width (cons extra-width args) args)) ) )

;; ** Transforming Sequences

; Recursively rebuild a stream
; Possibly transforming through a mapping function
;   which function might possibly want an index or label
; Possibly appending to an existing stream
(define (stream-rebuild ss #:map [f #f] #:tail [tail (stream)] #:index [index #f])
  (if (stream-empty? ss)
      tail
      (let ( [first (stream-first ss)] [rest (stream-rest ss)] )
        (stream-cons
         (if f (if index (f first index) (f first)) first)
         (stream-rebuild rest #:map f #:tail tail #:index (label-next index)) ) ) ) )

; Explode a string into a Stream
; Possibly transforming through a mapping function
; Possibly appending to an existing list
(define (explode-string s #:map [f #f] #:tail [tail #f])
  (stream-rebuild (sequence->stream (in-string s)) #:map f #:tail tail) )

(append-pics (explode-string "Hello!"
                             #:map (λ (c) (boxed-pic (text (string c)) #:skosh 3))
                             #:tail (list (boxed-pic (text "\\0") #:skosh 3)) ))

;; Building Composite Objects

; Transform a sequence of elements into a sequence of boxed pics
; of the same width and height
; - the max of the sizes of their constituents
; - plus a skosh around the width inside the boxes
; - optionally adding indices underneath
(define (boxed-elements elements #:index [index #f] #:skosh [extra-width 0])
  (let-values ( [(w h n) (max-pics-width-height elements)] )
    (let ( [f (if index
                  (λ (p i) (boxed-pic p #:width w #:height h #:skosh extra-width #:label i))
                  (λ (p) (boxed-pic p #:width w #:height h #:skosh 2)) )] )
      (stream-rebuild elements #:map f #:index index) ) ) )

; Transform a string into a sequence of boxed elements
(define (boxed-chars str #:null [add-null #f] #:index [index #f] #:skosh [extra-width 2])
  (let* ( [tail (and add-null (stream (text "\\0")))]
          [elements (explode-string str #:map (λ (c) (text (string c))) #:tail tail)] )
    (boxed-elements elements #:index index #:skosh extra-width) ) )

; Transform a string into a contiguous array of boxed characters as obove
(define (string-diagram s #:null [add-null #f] #:index [index #f])
  (append-pics (boxed-chars s #:null add-null #:index index)) )

(string-diagram "Hello!" #:null #t)
(string-diagram "Hello!" #:null #t #:index 0)

(define (boxed-fields fields #:labels [labels #f] #:skosh [extra-width 0])
  (let-values ( [(w h n) (max-pics-width-height fields)] )
    (let ( [f (if labels
                  (λ (p i) (boxed-pic p #:height h #:skosh extra-width #:label i))
                  (λ (p) (boxed-pic p #:height h #:skosh extra-width)) ) ] )
      (stream-rebuild fields #:map f #:index labels) ) ) )

(define (record-diagram fields #:labels [labels #f] #:skosh [extra-width 0])
  (append-pics (boxed-fields fields #:labels labels #:skosh extra-width)) )

(define field-names (stream "greeting" "storage"))

(record-diagram (stream (text "hello") (string-diagram "Hello!" #:null #t)) #:labels field-names)
(record-diagram (stream (text "hello") (string-diagram "Hello!" #:null #t #:index 0)) #:labels field-names)

;; ** What's next?

; Taking into account the width of labels in computing the width of array elements

; Pointers

; Constraint Satisfaction
