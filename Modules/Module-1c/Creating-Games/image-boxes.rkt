#lang racket

;; * Some Example Code

(require 2htdp/image)
(require rackunit) ; for checks

;; We would like sprites to represent image-graphs at a location.

;; We would like image-graphs to represent a tree of component parts which are
;; either images or labels.

;; ** Our Type Functions and Structures

;; A ratio is an integer, rational or real number in [0..1)
(define (ratio? x) (and (real? x) (>= x 0) (< x 1)))

;; A part contains a list of subparts, i.e. component parts
(struct/contract
 part
#; ( [parts (recursive-contract part-list? #:flat #:extra-delay)] )
 ( [parts part?] )
 #:transparent )

(define (part-list? x) (and (list? x) (andmap part? x)))

;; A named-part provides a name for a part
(struct/contract named-part part ( [name symbol?] ) #:transparent)

;; An offset-part provides an x/y offset for a part relative to the parent
(struct/contract offset-part part ( [x natural?] [y natural?] ) #:transparent)

;; An image-part provides an image as a part
;; Should component parts be restricted to fitting within the image's area?
(struct/contract image-part part ( [image image?] ) #:transparent)

;; An area-part identifies a special area as a part
;; - This could be, e.g. a hurtbox or a killbox
;; - Should component parts be restricted to fitting within the image's area?
(struct/contract area-part part (
                  [width natural?] [height natural?] )
                 #:transparent )

;; A part-ref references a part node with a sprite-relative offset.
(struct/contract part-ref ( [xoff natural?] [yoff natural?] [part part?] )
                 #:transparent )

;; Sprite area overlap detection needs to be global.
;; For efficiency and convenience we use part-ref structures for hotboxes.
;; Possible collisions might be delegated to methods of extended hotboxes.

(define (hotbox? x) (and (part-ref? x) (area-part? (part-ref-part x))))

(define (hotbox-list? x) (and (list? x) (andmap hotbox? x)))

;; A sprite inherits a tree of parts within a bounding area
;; Sprite methods provide behavior, including mutation
(struct/contract sprite area-part (
                   [x natural?] [y natural?] ; global location
                   [dx integer?] [dy integer?] ; velocity
                   ;; methods
                   [on-tick procedure?] ; movement, etc. - likely mutator!
                   [on-key procedure?] ; interactive behavior - likely mutator!
                   [place procedure?] ; draw ourself on a canvas
                   [hurtboxes hotbox-list?] [killboxes hotbox-list?]
                 ) #:transparent #:mutable )

;; ** Our Methods

;; We'll want to have an approprite
;; on-key and on-tick method for each kind
;; of sprite to do the right thing in response.

;; For some sprites we might want to do nothing,
;; so let's start with those two handlers - and
;; we can use them as our default handlers.

;; Do nothing and return the sprite
(define (on-key-do-nothing a-sprite a-key) a-sprite)

;; Do nothing and return the sprite
(define (on-tick-do-nothing a-sprite) a-sprite)

;; ** place-image method

(define (half x) (quotient x 2))

;; Like place-image, but relative to the lower-left corner
;; rather than the x and y centers of the image being placed.
#;(define (place-sprite sprite canvas)
  (let* ( [image (sprite-image sprite)]
          [center-x (+ (sprite-x sprite) (half (image-width image)))]
          [center-y (- (sprite-y sprite) (half (image-height image)))] )
    (place-image image center-x center-y canvas) ) )

;; ** Creating A sprite The Hard Way

;; With only this setup, we can create a sprite
;; with hurtboxes and killboxes but notice that
;; it's a lot of work

(define SCENE-WIDTH 800)
(define SCENE-HEIGHT 500)

;; Get an image and create a sprite using it
(define balloon-image (bitmap/file "image/balloon-small.png"))
;; Where will our balloon appear when we place it?
#;(define balloon (sprite balloon-image
                        (- SCENE-WIDTH (image-width balloon-image))
                        (image-height balloon-image)
                        place-sprite
                        1 1
                        '() '()
                        on-tick-do-nothing on-key-do-nothing ))

;; Now that we have a sprite, we can create some lists of hotboxes
#;(define some-hurtboxes (list (hotbox balloon 0 0 10 10)
                             (hotbox balloon 5 5 10 10) ))
#;(define some-killboxes (list (hotbox balloon 10 10 5 5)))

;; Now we can use our mutators to put our lists into our sprite
#;(set-sprite-hurtboxes! balloon some-hurtboxes)
#;(set-sprite-killboxes! balloon some-killboxes)

;; Show the result
#;balloon

;; ** Making Creating Sprites Easier

;; Question: How can we make creating sprites easier?
;; Answer: Define smarter constructor procedures!

;; Here are our smarter constructor procedures

#;(define (make-sprite image x y ; required arguments
                     ; positional arguments must be next if supplied
                     [dx 0] [dy  0]
                     ; keyword arguments can be given in any order
                     #:hurtboxes [hurtboxes '()]
                     #:killboxes [killboxes '()]
                     #:on-tick [on-tick on-tick-do-nothing]
                     #:on-key [on-key on-key-do-nothing] )
  ;; What will happen if image is neither a string? nor an image?
  (let ( [the-image (if (string? image) (bitmap/file image) image)] )
    (sprite the-image
            ;; why are we adjusting y but not x?
            x (if (= 0 y) (image-height the-image) y)
            place-sprite
            dx dy hurtboxes killboxes on-tick on-key ) ) )

#;(define (add-hurtbox sprite x y width height)
  ;; create a hotbox
  (let ( [b (hotbox sprite x y width height)] )
    ;; add it to the front of its sprite's hurtbox list
    (set-sprite-hurtboxes! sprite (cons b (sprite-hurtboxes sprite))) ) )

#;(define (add-killbox sprite x y width height)
  ;; create a hotbox
  (let ( [b (hotbox sprite x y width height)] )
    ;; add it to the front of its sprite's killbox list
    (set-sprite-killboxes! sprite (cons b (sprite-killboxes sprite))) ) )

;; ** And now constructing sprites is easier

#;(define plane (make-sprite "image/airplane-small-clipped-alpha.png" 0 0))
#;(add-hurtbox plane 0 0 10 10)
#;(add-hurtbox plane 5 5 10 10)
#;(add-killbox plane 10 10 5 5)

;; Show the result
#;plane

;; ** Could we make our constructors even smarter?

;; How could we change them to allow these forms to work:

#;(define balloon (make-sprite "image/balloon-small.png"
                               #:right-margin 10 #:y 'centered
                               #:hurtboxes '([0 0 10 10] [5 5 10 10])
                               #:killboxes '([10 10 5 5]) ))

#;(define plane (make-sprite "image/airplane-small-clipped-alpha.png"
                             #:x 0 #:y 'top
                             #:hurtboxes '([0 0 10 10] [5 5 10 10])
                             #:killboxes '([10 10 5 5]) ))

;; ** Have we left anything out that could cause trouble?  Yes we have!!!

;; Currently we're checking to make sure that the sprite field in every hotbox
;; really is a sprite? but we're not checking that the sprite fields in the
;; hurtboxes and killboxes refer to the sprites which they've been assigned to.

;; What might happen if a hotbox in one of plane's lists thought it was part
;; of balloon instead of plane???  Where could we put in a check to make sure
;; that such a mistake can't happen???

;; Where might we put in a check to prevent this from happening??
