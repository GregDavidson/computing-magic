#lang racket
;; * Multiple Worlds Multiple Sprites Client

;; The file sprites-words-games.rkt provides
;; - a description of the game
;; - struct sprite-proxy
;; - additional require forms
(require 2htdp/universe)
(require rackunit) ; assertions
(require racket/serialize)
(require uuid) ; univerally unique identifiers
(require "sprites-worlds-game.rkt")
(require 2htdp/image)
(require (except-in racket/draw make-color make-pen))

;; ** Client-Side Library Types

;; The WorldState is defined by the Client
;; and passed to and received from all handler
;; procedures.

;; Handler procedures can either return
;; - a bare WorldState
;; - a package

;; package - a structure consisting of
;; - a World State
;; - a serializable symbolic expression to send to the Server

;; ** Our Canvas and Our Color

(define CANVAS-WIDTH 400) ; pixels
(define CANVAS-HEIGHT 300) ; pixels
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; Choose colors which will, for any number of clients, be maximally
;; distinguishable. Uses may have color blindness so be use additional methods
;; (labels, textures, etc.) where visual differentiation is required.
 (define color-names
   #( ; a vector, indexable by client number
     red yellow green blue purple orange brown fuchsia
     skyblue yellowgreen navy aquamarine azure goldenrod coral chartreuse
     lime cornflowerblue indigo crimson forestgreen cyan hotpink lavender
     olive salmon turquoise silver indianred royalblue magenta pink
     teal violet
     ) )

(define *our-color* #f)
(define (set-our-color c)
  (if *our-color*
      (eprintf "~a already set to ~a" '*our-color* *our-color*)
      (set! *our-color*
            (cond [(image-color? c) c]
                  [(natural? c)
                   (set-our-color (vector-ref color-names (remainder c (vector-length color-names))))
                   (eprintf "~a: ~a\n" '*our-color* *our-color*)]
                  [else (error "bad color or index ~a" c)] ) ) ) )

;; *our-world-number* should be set via a U2W-WELCOME
;; message before any sprites are created.
(define *our-world-number* #f)
(define (set-our-world-number n)
  (if *our-world-number*
      (eprintf "~a already set to ~a" '*our-world-number* *our-world-number*)
      (set! *our-world-number* n) ) )

;; EXERCISE: Select colors that are maximally distinct
;; using a colorspace model from package color.
;; (require color)

;; ** Sprites

;; Each distinct sprite will have a universally unique id (uuid)
(struct/contract
  sprite ( [uuid strict-uuid-string?]
           [image (or/c image? #f)]
           [x natural?] [y natural?]
           [dx integer?] [dy integer?]
           [on-tick (or/c procedure? #f)]
           [on-key (or/c procedure? #f)]
           [to-draw (or/c procedure? #f)] )
  #:mutable #:transparent )

;; We need to be able to send sprites across worlds.
;; This requires us to serialize them, i.e. convert them to a byte stream.
;; Alas, Racket doesn't provide for serialization of
;; - regular structures, images or procedures

;; A serializable-struct can be serialized if all of their components
;; can be serialized.  So let's create a serializable sprite-proxy!

;; Images will be represented either by
;; (1) a filesystem path to a stored image
;; (2) a symbol representing a function which
;;     takes a color and returns an image.
;; Procedures will be represented by their names (symbols).
;; A sprite-proxy will have the same uuid as the sprite it is a proxy for.
;; Only the uuid field is required.  The other fields can default to #f if
;; the corresponding sprite field is irrelevant, i.e. not requiring an update.
(serializable-struct
 sprite-proxy (uuid image x y dx dy on-tick on-key to-draw)
 #:guard (struct-guard/c
          strict-uuid-string? (or/c #f string? symbol?)
          (or/c #f natural?) (or/c #f natural?)
          (or/c #f integer?) (or/c #f integer?)
          (or/c #f procedure?) (or/c #f procedure?) (or/c #f procedure?) )
 #:transparent )

;; ** Transmitting Sprites

;; To transmit a sprite between a world and a server
;; - a sprite-proxy is created of the sprite
;; - the sprite-proxy is sent as part of a Mail Message
;; - when the Mail Message is received
;; - any sprite-proxy structures are turned back into sprite structures

;; a straight copy would look like this
#;(define (sprite->proxy s)
  (sprite-proxy
   (sprite-uuid s) ; ok
   (sprite-image s) ; needs a string path or function name instead
   (sprite-x s) ; ok
   (sprite-y s) ; ok
   (sprite-dx s) ; ok
   (sprite-dy s) ; ok
   (sprite-on-tick s) ; needs a procedure name instead
   (sprite-on-key s)  ; needs a procedure name instead
   (sprite-to-draw s) ; needs a procedure name instead
   ) )

;; We need a place to associate
;; - the keys (path strings or procedure names)
;; - with the original sprite values
;; for each of the sprite fields that won't serialize.

;; We can use association lists.  Since they're associated
;; with specific sprite structure fields, we'll use a global
;; structure as a central registry:
(struct/contract
  registry (
            ;; To support dynamic loading of images, we could allow the
            ;; image to be #f until it's needed.  Then it can be created
            ;; from its procedure or loaded from its filesystem path.
            [image (listof (cons/c (or/c string? symbol?)
                                   (or/c image? procedure? string? #f) ))]
            [on-tick (listof (cons/c symbol? procedure?))]
            [on-key (listof (cons/c symbol? procedure?))]
            [to-draw (listof (cons/c symbol? procedure?))] )
  #:mutable #:transparent )
(define proxy-registry (registry '() '() '() '()))
;; The keys in each association list should be unique.

;; Return the element of the list in the proxy-registry field
;; (1) accessed with the given getter (structure selector function)
;; (2) satisfying the given predicate
;; - or #f if none found!!
(define (registry-find predicate getter)
  (findf predicate (getter proxy-registry)) )

;; Ensure that a cons pair (key . val) is in the association list
;; of the proxy-registry field with the given getter.
;; If it isn't, add it with the provided setter.
(define (register-key-val key val getter setter)
  (let* ( [alist (getter proxy-registry)]
          [found (findf (λ (pair) (equal? key (first pair))) alist)] )
    (unless found (setter proxy-registry (cons (cons key val) alist))) ) )

;; Given the key, return the val part of the (key . val) association
;; in the proxy-registry field accessed with the given getter function.
;; Returns #f if key is not present!!
(define (key->val key getter)
  (let ( [found (registry-find (λ (pair) (equal? key (first pair))) getter)] )
    (and found (second found)) ) )

;; Given the val, return the key part of the (key . val) association
;; in the proxy-registry field accessed with the given getter function.
;; Returns #f if key is not present!!
(define (val->key val getter)
  (let ( [found (registry-find (λ (pair) (equal? val (second pair))) getter)] )
    (and found (first found)) ) )

;; Load a bitmap from a file, cache (save) it in the registry
;; and return it. What happens if there's no image at that path?
(define (bitmap/file/cache path)
  (let ( [image (bitmap/file path)] )
    (register-key-val path image registry-image set-registry-image!)
    image ) )

;; Given a path or symbol as a key, return the associated image
;; and ensure it's registered.
(define (get-image key)
  (let ( [found (key->val key registry-image)] )
    (cond [(image? found) found]
          [(procedure? found) (found *our-color*)] ; cache it??
          [(string? found) (bitmap/file/cache found)]
          [(string? key) (bitmap/file/cache key)]
          [else (eprintf "Can't get image from ~a\n" key) #f] ) ) )

;; Given a sprite, convert it to a proxy which can be serialized
;; for transmission across a byte stream.  #f will be substituted
;; for any unknown values!!
(define (sprite->proxy s)
  (sprite-proxy
   (sprite-uuid s)
   (val->key (sprite-image s) registry-image)
   (sprite-x s) (sprite-y s) (sprite-dx s) (sprite-dy s)
   (val->key (sprite-on-tick s) registry-on-tick)
   (val->key (sprite-on-key s) registry-on-key)
   (val->key (sprite-to-draw s) registry-to-draw) ) )

;; Given a sprite-proxy, convert it to a
;; This returns a new sprite, even if there's already a sprite
;; with this uuid.  You'll have to update any such sprite with
;; any non-#f values of the new sprite!!
(define (proxy->sprite sp)
  (sprite (sprite-proxy-uuid sp)
          (get-image (sprite-proxy-image sp))
          (sprite-proxy-x sp)
          (sprite-proxy-y sp)
          (sprite-proxy-dx sp)
          (sprite-proxy-dy sp)
          (key->val (sprite-proxy-on-tick sp) registry-on-tick)
          (key->val (sprite-proxy-on-key sp) registry-on-key)
          (key->val (sprite-proxy-to-draw sp) registry-to-draw) ) )

;; WARNING: If we don't register all of the proxy values we need we'll
;; wind up with imcomplete sprite-proxy structures where some of the
;; fields will be #f which will then be converted into incomplete
;; sprite structures on receipt!!
;; Should this happen, depending on message type:
;; MUTATE-SPRITE -- only update valid fields, log an issue
;; NEW-SPRITE -- ignore this sprite, log an issue
;; DROP-SPRITE -- only the uuid matters, so no worries!
;; -- Learn about logging vs. volatile error reports!!

;; Make a sprite-proxy which represents desired updates to a sprite
(define (make-proxy s
                    #:image [image #f]
                    #:x [x #f] #:y [y #f] #:dx [dx #f] #:dy [dy #f]
                    #:tick [tick #f] #:key [key #f] #:draw [draw #f] )
  ;; will changing value v to vv make a difference?
  (define (delta v vv) (and vv (not (equal? v vv)) vv))
  (sprite-proxy (sprite-uuid s)
                (delta (sprite-image s) image)
                (delta (sprite-x s) x) (delta (sprite-y s) y)
                (delta (sprite-dx s) dx) (delta (sprite-dy s) dy)
                (delta (sprite-on-tick s) tick)
                (delta (sprite-on-key s) key)
                (delta (sprite-to-draw s) draw) ) )

(define (mutate-sprite-from-proxy! s sp)
  (unless (equal? (sprite-uuid s) (sprite-proxy-uuid sp))
    (error "improper mutation of ~a by ~a" s sp) )
  (when (sprite-proxy-image sp)
    (set-sprite-image! s (or (key->val (sprite-proxy-image sp) registry-image)
                             (begin
                               (error "no image in ~a" sp)
                               (sprite-image s) ) )) )
  (when (sprite-proxy-x sp) (set-sprite-x! s (sprite-proxy-x sp)))
  (when (sprite-proxy-y sp) (set-sprite-y! s (sprite-proxy-y sp)))
  (when (sprite-proxy-dx sp) (set-sprite-dx! s (sprite-proxy-dx sp)))
  (when (sprite-proxy-dy sp) (set-sprite-dy! s (sprite-proxy-dy sp)))
  (when (sprite-proxy-on-tick sp)
    (set-sprite-on-tick! s (or (key->val (sprite-proxy-on-tick sp) registry-on-tick)
                               (begin
                                 (error "no on-tick in ~a" sp)
                                 (sprite-on-tick s) ) )) )
  (when (sprite-proxy-on-key sp) 
    (set-sprite-on-key! s (or (key->val (sprite-proxy-on-key sp) registry-on-key)
                              (begin
                                (error "no on-key in ~a" sp)
                                (sprite-on-key s) ) )) )
  (when (sprite-proxy-to-draw sp)
    (set-sprite-to-draw! s (or (key->val (sprite-proxy-to-draw sp) registry-to-draw)
                               (begin
                                 (error "no to-draw in ~a" sp)
                                 (sprite-to-draw s) ) )) ) )

;; If you're interested in how macros and structures work, here are
;; some exercises which might interest you:
;; EXERCISE: Write a macro which extracts the field names
;;           from a structure id.  Then you can write
#;(test-true (set-eq? (struct-fieldnames sprite) (struct-fieldnames sprite-proxy)))
#;(test-true (subset? (struct-fieldnames registry) (struct-fieldnames sprite)))
;; ADVANCED EXERCISE:
;; Extend the definition of struct so that with suitable keywords it can
;; do everything which the other structure creating macros can do, i.e.
;; the features of struct/contract, serializable-struct, etc.  See
;; https://github.com/GregDavidson/computing-magic/blob/main/Racket/racket-structs.org
;; and post an issue if you'd like a mentor for this project!

;; ** Messages (World <--> Universe Server Mail)

;; The Racket big-bang and universe procedures will automatically
;; send messages as "Mail" as long as our messages are serializable.
;; For details see
;; https://docs.racket-lang.org/teachpack/2htdpuniverse.html
;; and especially section 2.4.5 "The World is not Enough"
;; https://docs.racket-lang.org/teachpack/2htdpuniverse.html#%28part._universe._world2%29

;; For this game, our Messages (Mail) will either be
;; - a symbol or
;; - a list of a symbols and sprite-proxy structures
;;   starting with a symbol

;; Forward Messages sent by any world to the server
;; will be forwarded to all the other worlds.
;; These symbols may appear in Forward Messages:
(define NEW-SPRITE 'new)
(define MUTATE-SPRITE 'mutate)
(define DROP-SPRITE 'drop)

;; The other message types are defined in
;; the file sprites-worlds-game.rkt

;; ** Our World State

;; It's actually the State of the Universe
;; as viewed from our world!

;; Our WorldState consists of two lists of sprites:
(struct/contract
 sprites ( [ours (listof sprite?)]
           [theirs (listof sprite?)] )
 #:transparent )

;; Our tick and key events are relayed to our-sprites.
;; Our receive events are relayed to their-sprites.
;; Our draw events are relayed to all sprites!

;; Our WorldState evolves
;; - when a sprite moves according to its velocity
;; - when a sprite is lost by going outside of the canvas

;; Changes to our-sprites need to be "Mailed" to the Server
;; so it can forward those changes to the Other Worlds!

(define INITIAL-STATE (sprites '() '()))

;; A WorldResult is one of:
;; – WorldState
;; – (make-package WorldState Mail)

;; MailActions are (or/c NEW-SPRITE MUTATE-SPRITE DROP-SPRITE)
;; Mail is either (list U2W-WELCOME natural?)
;; or it is (cons/c MailActions (listof (or/c MailActions sprite?))
;; that is, a list of action symbols followed by sprites.
;; The action will be applied to the sprites which follow the action.
;; Normally actions will be followed by at least one sprite;
;; nothing happens if an action is followed by zero sprites!

;; Given
;; - a Mail action list from the server
;;   a list of sprites
;; Returns an updated list of sprites
;; - according to the actions
(define (update-sprites actions sprites)
  ;; process the actions with mutually recursive functions
  ;; letrec creates a common scope for all of its bindings
  (letrec
      (
       ;; which function should process the proxies?
       [switch (λ (action proxies sprites)
                 (cond [(eq? action NEW-SPRITE) (call new proxies sprites)]
                       [(eq? action MUTATE-SPRITE) (call mutate proxies sprites)]
                       [(eq? action DROP-SPRITE) (call drop proxies sprites)]
                       [else (error "bad action ~a in message ~a" action proxies)] ) )]
       ;; we have a handler, do we have any proxies to apply?
       [call (λ (handler actions sprites)
               (if (null? actions)
                   sprites
                   (handler actions sprites) ) )]
       ;; add any new proxies as sprites
       [new (λ (actions sprites)
              (let ( [first (car actions)] [rest (cdr actions)] )
                (if (sprite-proxy? first)
                    (new rest (cons (proxy->sprite first) sprites))
                    (switch first rest sprites) ) ) )]
       ;; drop any sprites with these uuids
       [drop (λ (actions sprites)
               (let ( [first (car actions)] [rest (cdr actions)] )
                 (if (sprite-proxy? first)
                     (drop rest (remove (λ (s) (eq? (sprite-proxy-uuid first) (sprite-uuid s)))
                                        sprites ))
                     (switch first rest sprites) ) ) )]
       ;; mutate any sprites with these uuids and new field values
       [mutate (λ (actions sprites)
                 (let ( [first (car actions)] [rest (cdr actions)] )
                   (if (sprite-proxy? first)
                       (let* ( [uuid (sprite-proxy-uuid first)]
                               [s (findf (λ (s) (eq? uuid (sprite-uuid s))) sprites)] )
                         (mutate-sprite-from-proxy! s first)
                         (mutate rest sprites) )
                       (switch first rest sprites) ) ) )] )
    (if (null? actions) ; no actions?
        sprites           ; just return the sprites unchanged
        (let ( [action (car actions)] [values (cdr actions)] )
          (switch action values sprites) ) ) ) )

;; Given
;; - our current state
;; - mail from the server
;; Return an updated WorldState
;; - possibly including Return Mail
(define (receive state mail)
  (eprintf "1. mail: ~a\n" mail)
  (cond [(not (list? mail)) (error "bad mail ~a" mail)]
        [(null? mail) state] ; no actions, return state unchanged
        [(welcome? mail)
         (eprintf "2. mail: ~a\n" mail)
         (let ( [number (welcome-world-number mail)] )
           (set-our-world-number number)
           (set-our-color number)
           (sprites (if (null? (sprites-ours state))
                        (list (make-ball *our-color*))
                        (sprites-ours state) )
                    (sprites-theirs state) ) ) ]
        [else
         (eprintf "3. mail: ~a\n" mail)
         (sprites (sprites-ours state)
                  (update-sprites mail (sprites-theirs state)) )] ) )

;; ** Action Procedures and Functions

(define DX-BOOST 5)
(define DY-BOOST 5)
(define DY-FALLING 2)

;; decay moves a value closer to its target value,
;; i.e. it makes boosts decay.
(define (decay value target [step 1])
  (cond [(> value target) (- value step)]
        [(< value target) (+ value step)]
        [else value] ) )

;; Why add text to the ball? (How common is color blindness?)
;; We could use the provided name instead (or in addition to) the
;; number.  How can you better center the text on the ball??
(define (make-ball color)
  (eprintf "~a: ~a\n" '*our-color* *our-color*)
  (overlay (text (number->string *our-world-number*) 10 'black)
           (circle 20 "solid" color) ) )
(register-key-val 'make-ball make-ball registry-image set-registry-image!)

(define (on-canvas? image x y)
  (and (< 0 x (- CANVAS-WIDTH (image-width image)))
       (< 0 y (- CANVAS-HEIGHT (image-height image))) ) )

;; WorldState -> ActionList
;; update the sprite's coordinates;
;; drop it if it's no longer on-canvas;
;; decay any velocity boosts
(define (move-sprite s)
  (let ( [image (sprite-image s)]
         [x (sprite-x s)] [y (sprite-y s)]
         [dx (sprite-dx s)] [dy (sprite-dy s)] )
    (let ( [xx (+ x dx)] [yy (+ y dy)] )
      (if (not (on-canvas? image xx yy))
          (list DROP-SPRITE (make-proxy s))
          (let ( [dxx (decay dx 0)] [dyy (decay dy DY-FALLING)] )
            (list MUTATE-SPRITE
                  (make-proxy s #:x xx #:y yy #:dx dxx #:dy dyy) ) ) ) ) ) )
(register-key-val 'move-sprite move-sprite registry-on-tick set-registry-on-tick!)

;; WorldState Key -> ActionList
;; apply any boosts to its velocity
(define (boost-sprite-on-key s k)
  (cond
    [(key=? k "up") (list MUTATE-SPRITE
                          (make-proxy s #:dy (+ (sprite-dy s) DY-BOOST)) )]
    [(key=? k "down") (list MUTATE-SPRITE
                            (make-proxy s #:dy (- (sprite-dy s) DY-BOOST)) )]
    [(key=? k "right") (list MUTATE-SPRITE
                             (make-proxy s #:dx (+ (sprite-dx s) DX-BOOST)) )]
    [(key=? k "down") (list MUTATE-SPRITE
                            (make-proxy s #:dx (- (sprite-dx s) DX-BOOST)) )]
    [else '()] )
 )
(register-key-val 'boost-sprite-on-key boost-sprite-on-key registry-on-key set-registry-on-key!)

;; ** Rendering

(define (half n) (quotient n 2))

;; Like place-image, but relative to the left-bottom corner
;; of the sprite and the canvas.
(define (draw-image image x y canvas)
  (let ( [center-x (+ x (half (image-width image)))]
         [center-y (+ y (half (image-height image)))] )
    (place-image image center-x (- CANVAS-HEIGHT center-y) canvas) ) )

(define (draw-sprite sprite canvas)
  (draw-image (sprite-image sprite)
              (sprite-x sprite) (sprite-y sprite)
              canvas ) )

;; ** big-bang callback procedures

;; Return updated sprites composed of all sprites returned by updates.
;; How might grouping actions by ACTION symbol impact efficiency??
(define (gather-actions-on-tick sprites)
  (foldr append ; append together all the returned action lists
         '()    ; starting with none
         (map   ; collicting small action lists from
          ;; calling sprite's own on-tick method on itself
          (λ (sprite) ( (sprite-on-tick sprite) sprite ))
          ;; for every sprite in sprites
          sprites ) ) )

;; return Package from applying on-tick methods to our sprites
(define (update-world-on-tick world)
  (let* ( [ours (sprites-ours world)]
          [actions (gather-actions-on-tick ours)]
          [ours-updated (update-sprites actions ours)] )
    (eprintf "our-sprites: ~a\n" ours-updated)
    (make-package (sprites ours-updated (sprites-theirs world)) actions) ) )

;; deliver key events to all sprites in the given list
;; returning a merged action list in Mail format
(define (gather-actions-on-key sprites key)
  (foldr append ; append together all the returned action lists
         '()    ; starting with none
         (map   ; collicting small action lists from
          ;; calling sprite's own on-key method on itself
          (λ (sprite) ( (sprite-on-key sprite) sprite key ))
          ;; for every sprite in sprites
          sprites ) ) )

;; return Package from applying on-key methods to our sprites
(define (update-world-on-key world key)
  (let* ( [actions (gather-actions-on-key (sprites-ours world) key)]
          [our-sprites (update-sprites actions sprites)] )
    (make-package (sprites our-sprites (sprites-theirs world)) actions) ) )

;; Return a new canvas with our sprites drawn on top of
;; their sprites drawn on an empty canvas.
(define (draw-world world)
  (foldr draw-sprite
         (foldr draw-sprite EMPTY-CANVAS (sprites-theirs world))
         (sprites-ours world) ) )

; String -> WorldState
; create a world, hook it up to a server and start it running
(define (create-world a-name [server LOCALHOST])
  (when (symbol? a-name) (set! a-name (symbol->string a-name)))
  (big-bang INITIAL-STATE
    [on-receive receive]
    [to-draw draw-world]
    [on-key update-world-on-key]
    [on-tick update-world-on-tick 1/30]
    [stop-when (λ (_) #f)] ; for now, never stop!
    [name a-name]
    [register server] ) )

(define (go [server LOCALHOST]) (launch-many-worlds
                                 (create-world "Eti" server)
                                 (create-world "Brandt" server)
                                 (create-world "Touch" server) ))
