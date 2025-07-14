;; (setq-local racket-repl-buffer-name "*sprites-worlds-client-repl*")
#lang racket/base
;; * Multiple Worlds Multiple Sprites Client

;; See sprites-worlds-game.org for information about the game.

;; The file sprites-words-games.rkt provides
;; - inter-client (inter-world) protocol information
;; - including a sprite-proxy structure
;; - You'll want to look it over carefully!
(require 2htdp/image
         2htdp/universe
         racket/cmdline
         racket/contract/base
         racket/contract/region
         racket/list
         racket/math
         "sprites-worlds-game.rkt")

(tracing #t) ; trace everywhere!
(*testing* #t) ; customize for easy testing

;; ** Client-Side 2http Framework Types

;; The WorldState is defined by the Client
;; and passed to and received from all handler
;; procedures.

;; Handler procedures can either return
;; - a bare WorldState
;; - a package structure consisting of
;;   - a World State
;;   - a serializable symbolic expression to send to the Server
;;     - 2http serialization requirements are quite limited

;; ** Our Canvas

(define CANVAS-WIDTH 400)               ; pixels
(define CANVAS-HEIGHT 300)              ; pixels
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; ** Managing Colors

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

(define (choose-color c)
  (define this 'choose-color)
  (cond [(natural? c)
         (vector-ref color-names (remainder c (vector-length color-names))) ]
        [(image-color? c) c]
        [else (error 'this "invalid color ~a" c)] ) )

;; EXERCISE: Select colors that are maximally distinct
;; using a colorspace model from package color.
;; (require color)

;; ** Grouping World Parameters in an Immutable Structure

;; Parameters can be bound in the global environment
;; or they can be made part of the world state.

;; Global bindings are good if they are constant
;; after program initialization.

;; If we need to supply specific parameters to
;; procedures running in other worlds
;; (or other threads) then it's better to group
;; them in structures and pass them explicitly
;; to those procedures.

;; Such grouped parameters can also become part of
;; our World State.

;; A structure type for World Parameters needed by
;; functions passed by name in our proxy structures.
;; It's Universe serializable because
;; - it's a #:prefab structure
;; - it's fields are Universe serializable
(struct params ( number color falling )
  #:constructor-name make-params
  #:prefab )

;; ** Sprites

;; Each distinct sprite will have a unique key
(struct/contract
  sprite ( [key key-value?]
           [image (or/c image? #f)]
           [x natural?] [y natural?]
           [dx integer?] [dy integer?]
           [on-tick (or/c procedure? #f)]
           [on-key (or/c procedure? #f)]
           [to-draw (or/c procedure? #f)] )
  #:mutable #:transparent )

;; ** Serializing Sprites for Transmission

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
;; A sprite-proxy will have the same key as the sprite it is a proxy for.
;; Only the key field is required.  The other fields can default to #f if
;; the corresponding sprite field is irrelevant, i.e. not requiring an update.

;; ** Translating Non-Serializable Values

;; To transmit a sprite between a world and a server
;; - a sprite-proxy is created of the sprite
;; - the sprite-proxy is sent as part of a Mail Message
;; - when the Mail Message is received
;; - sprite-proxy structures are used to create and update sprite structures

;; We need a place to associate
;; - the keys (path strings or procedure names)
;; - with the original sprite values
;; for each of the sprite fields that won't serialize.

;; We can use association lists.  Since they're associated
;; with specific sprite structure fields, we'll use a global
;; structure as a registry, with its field names corresponding
;; to the field names to be translated.

;; here's the type
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

;; here's the registry
(define proxy-registry (registry '() '() '() '()))

;; The keys in each association list should be unique.
;; What will happen if the values are not unique??

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
;; and return it. What happens if there's no image at that path??
(define (bitmap/file/cache path)
  (let ( [image (bitmap/file path)] )
    (register-key-val path image registry-image set-registry-image!)
    image ) )

;; Given a path or symbol as a key, return the associated image
;; and ensure it's registered.
(define (get-image params key)
  (define this 'get-image-key)
  (let ( [found (key->val key registry-image)] )
    (cond [(image? found) found]
          [(procedure? found) (found params)] ; cache it??
          [(string? found) (bitmap/file/cache found)]
          [(string? key) (bitmap/file/cache key)]
          [else           ; should this be an error??
           (eprintf "~a warning: no image at ~a\n" this key)
           #f ] ) ) )

;; Given a sprite, convert it to a proxy which can be serialized
;; for transmission across a byte stream.  #f will be substituted
;; for any unknown values!!
;; See make-proxy for what we're really using!!
#;(define (sprite->proxy s)
  (make-sprite-proxy
   (sprite-key s)
   (val->key (sprite-image s) registry-image)
   (sprite-x s) (sprite-y s) (sprite-dx s) (sprite-dy s)
   (val->key (sprite-on-tick s) registry-on-tick)
   (val->key (sprite-on-key s) registry-on-key)
   (val->key (sprite-to-draw s) registry-to-draw) ) )

;; Given a sprite-proxy, convert it to a new sprite.
;; This is used with NEW-SPRITE actions.
(define (proxy->sprite params sp)
  (define this 'proxy->sprite)
  (let ( [s (sprite (sprite-proxy-key sp)
          (get-image params (sprite-proxy-image sp))
          (sprite-proxy-x sp)
          (sprite-proxy-y sp)
          (sprite-proxy-dx sp)
          (sprite-proxy-dy sp)
          (key->val (sprite-proxy-on-tick sp) registry-on-tick)
          (key->val (sprite-proxy-on-key sp) registry-on-key)
          (key->val (sprite-proxy-to-draw sp) registry-to-draw) )] )
    (when (tracing this) (eprintf "~a sprite-proxy-key: ~a -> ~a" this sp s ))
    s ) )

;; NOTE: If we don't register all of the proxy values we need we'll
;; wind up with sprite-proxy structures where some of the fields
;; will be #f with various results, depending on message actions:
;; MUTATE-SPRITE -- only update non-#f fields
;; NEW-SPRITE -- ignore this sprite, report an issue!!
;; DROP-SPRITE -- only the key matters, so no worries!
;; -- Learn about logging vs. volatile error reports!!

;; Do we have a new value different from the old value?
;; Return new value if it will, #f if it won't.
(define (delta old new) (and new (not (equal? old new)) new))

;; Make a sprite-proxy which represents desired updates to a sprite
;; This is used with MUTATE-SPRITE actions.
(define (make-proxy s
                    #:image [image #f]
                    #:x [x #f] #:y [y #f] #:dx [dx #f] #:dy [dy #f]
                    #:tick [tick #f] #:key [key #f] #:draw [draw #f] )
  (make-sprite-proxy (sprite-key s)
                     (and (delta (sprite-image s) image) (val->key image registry-image))
                     (delta (sprite-x s) x) (delta (sprite-y s) y)
                     (delta (sprite-dx s) dx) (delta (sprite-dy s) dy)
                     (and (delta (sprite-on-tick s) tick) (val->key tick registry-on-tick))
                     (and (delta (sprite-on-key s) key) (val->key key registry-on-key))
                     (and (delta (sprite-to-draw s) draw) (val->key draw registry-to-draw) ) ) )

(define (mutate-sprite-from-proxy! params s sp)
  (define this 'mutate-sprite-from-proxy!)
  (define original (struct-copy sprite s)) ; for debugging
  (unless (eq? (sprite-key s) (sprite-proxy-key sp))
    (error this "improper mutation of ~a by ~a" s sp) )
  (when (sprite-proxy-image sp)
    (set-sprite-image! s (get-image params (sprite-proxy-image sp)) ) )
  (when (sprite-proxy-x sp) (set-sprite-x! s (sprite-proxy-x sp)))
  (when (sprite-proxy-y sp) (set-sprite-y! s (sprite-proxy-y sp)))
  (when (sprite-proxy-dx sp) (set-sprite-dx! s (sprite-proxy-dx sp)))
  (when (sprite-proxy-dy sp) (set-sprite-dy! s (sprite-proxy-dy sp)))
  (when (sprite-proxy-on-tick sp)
    (set-sprite-on-tick! s (or (key->val (sprite-proxy-on-tick sp) registry-on-tick)
                               (begin
                                 (error this "no on-tick in ~a" sp)
                                 (sprite-on-tick s) ) )) )
  (when (sprite-proxy-on-key sp) 
    (set-sprite-on-key! s (or (key->val (sprite-proxy-on-key sp) registry-on-key)
                              (begin
                                (error this "no on-key in ~a" sp)
                                (sprite-on-key s) ) )) )
  (when (sprite-proxy-to-draw sp)
    (set-sprite-to-draw! s (or (key->val (sprite-proxy-to-draw sp) registry-to-draw)
                               (begin
                                 (error this "no to-draw in ~a" sp)
                                 (sprite-to-draw s) ) )) )
  (when (equal? s original) (error this "~a didn't change ~a" sp s))
  (when (tracing this) (eprintf "~a ~a mutated ~a to ~a\n" this sp original s)) )

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

;; The other message types are defined in
;; the file sprites-worlds-game.rkt

;; ** Our World State

;; It's actually the State of the Universe
;; as viewed from our world!

;; Our WorldState consists of our parameters
;; and two lists of sprites:
(struct state ( params ours theirs )
  #:constructor-name make-state
  #:guard
  (struct-guard/c params? (listof sprite?) (listof sprite?))
  #:transparent
  )

;; Our tick and key events are relayed to our-sprites.
;; Our receive events are relayed to their-sprites.
;; Our draw events are relayed to all sprites!

;; Our WorldState evolves
;; - when a sprite moves according to its velocity
;; - when a sprite is lost by going outside of the canvas

;; Changes to our-sprites need to be "Mailed" to the Server
;; so it can forward those changes to the Other Worlds!

;; A WorldResult is one of:
;; – WorldState
;; – (make-package WorldState Mail)

;; Forward Messages sent by any world to the server
;; will be forwarded to all the other worlds.
;; These symbols may appear in Forward Messages:
(define NEW-SPRITE 'new)
(define MUTATE-SPRITE 'mutate)
(define DROP-SPRITE 'drop)

#; (flat-named-contract
    ActionMail
    (listof (or/c (cons/c DROP-SPRITE (listof key-value?))
                  (cons/c MUTATE-SPRITE (cons/c params? (listof sprite-proxy?)))
                  (cons/c NEW-SPRITE (cons/c params? (listof sprite-proxy?))) )) )

;; All three update-sprites actions are dealing with two sets sharing a key.
;; We're using algorithms of time complexity O(n*m) for n actions on m sprites.
;; Strategies if this is a bottleneck include
;; - using an indexed data structure for either or both sets
;; - partitioning sprites and sprite-proxies by the world which owns the sprite
;;   - and possibly ordering both by creation time using ULID or BUID keys

;; Given
;; - a ActionMail from the server
;; - a list of sprites
;; Returns an updated list of sprites
;; - according to the actions
(define (update-sprites action-lists sprites)
  (define this 'update-sprites)
  ;; return the subset of the action-lists of the given action-type
  (define (subset action-type) (filter (λ (action) (eq? action-type (car action))) action-lists))
  ;; return the sprites left after removing those matching the keys in drop-lists
  (define (after-drops drop-lists sprites)
    (let ( [merged-drops (foldr append '() drop-lists)] )
      (remove (λ (s) (memq s merged-drops)) sprites) ) )
  ;; update (mutate) the sprites from the proxies in the update-lists with matching keys
  (define (after-updates update-lists sprites)
    (for-each (λ (update-list) ; for each update-list
                (let ( [params (car update-list)] )
                  (for-each (λ (sp) ; for each update proxy
                              (for-each (λ (s) ; for each sprite
                                          (when (eq? (sprite-proxy-key sp) (sprite-key s))
                                            (mutate-sprite-from-proxy! params s sp) ) )
                                        sprites ) ) ; the sprites
                            (cdr update-list) ) ) ) ; the proxies
              update-lists ) ; the update lists
    sprites ) ; return the list of sprites we were given
  ;; add new sprites specified by the proxies in the creation lists
  (define (after-creations creation-lists sprites)
    (foldr append '() (append (map (λ (creation-list)
                                     (let ( [params (car creation-list)] [proxies (cdr creation-list)] )
                                       (map (λ (sp) (proxy->sprite params sp)) proxies) ) )
                                   creation-lists )
                              (list sprites) )) )
  ;; compose: after-drops -> after-updates -> after-creations
  (when (tracing this) (eprintf "~a action-lists ~a\n" this action-lists))
  (when (tracing this) (eprintf "~a sprites ~a\n" this sprites))
  (after-creations (subset NEW-SPRITE)
                   (after-updates (subset MUTATE-SPRITE)
                                  (after-drops (subset DROP-SPRITE) sprites) ) ) )

;; Given
;; - the state of the world
;; - action mail from the server
;; Return an updated WorldState
;; - possibly including Return Mail
(define (receive world mail)
  (let ( [this 'receive]
         [our-params (state-params world)]
         [our-sprites (state-ours world)]
         [their-sprites (state-theirs world)] )
    (when (tracing this) (eprintf "~a mail: ~a\n" this mail))
    (cond [(not (list? mail)) (error this "bad mail ~a" mail)]
          [(null? mail) world] ; no actions, return world unchanged
          [(welcome? mail)
           ;; check (null? our-sprites)
           (let* ( [number (welcome-world-number mail)]
                   [color (choose-color number)]
                   [updated-params (struct-copy params our-params
                           [number number]
                           [color color] ) ] )
             (when (tracing this) (eprintf "~a number ~a color ~a\n" this number color))
             (let* ( [new-sprite (make-sprite  updated-params (make-ball updated-params))]
                     [ours-updated (cons new-sprite our-sprites)] )
               (make-package 
                (make-state updated-params ours-updated their-sprites)
                (list NEW-SPRITE (make-proxy new-sprite)) ) ) ) ]
          [else (make-state our-params
                       our-sprites
                       (update-sprites mail their-sprites) )] ) ) )

;; ** Action Procedures and Functions

(define DX-BOOST 6)
(define DY-BOOST 6)
(define DY-FALLING -2)

;; decay moves a value closer to its target value,
;; i.e. it makes boosts decay.
(define (decay value target [step 1])
  (cond [(> value target) (- value step)]
        [(< value target) (+ value step)]
        [else value] ) )

;; Why add text to the ball? 10% of humans are color blind!
;; EXERCISE: Use the provided name instead (or in addition to)
;; the number.
(define (make-ball params)
  (define this 'make-ball)
  (let ( [color (params-color params)]
         [number (params-number params)] )
    (when (tracing this) (eprintf "~a number ~a color ~a\n" this number color))
    (overlay (text (number->string number) 10 'black)
             (circle 20 "solid" color) ) ) )
(register-key-val 'make-ball make-ball registry-image set-registry-image!)

(define (make-sprite params image #:key [key #f]
                     #:x [x #f] #:y [y #f] #:dx [dx #f] #:dy [dy #f]
                     #:on-tick [on-tick #f] #:on-key [on-key #f] #:on-draw [on-draw #f] )
    (sprite
     (or key (key-value))
     image
     (or x (half CANVAS-WIDTH))
     (or y (- CANVAS-HEIGHT (image-height image) 10))
     (or dx 0) (or dy (params-falling params))
     (or on-tick move-sprite)
     (or on-key boost-sprite-on-key)
     (or on-draw draw-sprite) ) )

(define (on-canvas? image x y)
  (and (< 0 x (- CANVAS-WIDTH (image-width image)))
       (< 0 y (- CANVAS-HEIGHT (image-height image))) ) )

;; ** Sprite Action Methods

;; params sprite -> ActionList
;; update the sprite's coordinates;
;; drop it if it's no longer on-canvas;
;; decay any velocity boosts
(define (move-sprite params s)
  (let ( [image (sprite-image s)]
         [x (sprite-x s)] [y (sprite-y s)]
         [dx (sprite-dx s)] [dy (sprite-dy s)] )
    (let ( [xx (+ x dx)] [yy (+ y dy)] )
      (if (not (on-canvas? image xx yy))
          (list DROP-SPRITE (make-proxy s))
          (let ( [dxx (decay dx 0)] [dyy (decay dy (params-falling params))] )
            (if (and (= x xx) (= y yy) (= dx dxx) (= dy dyy))
                '() ; nothing changed
                (list MUTATE-SPRITE
                      (make-proxy s #:x xx #:y yy #:dx dxx #:dy dyy) ) ) ) ) ) ) )
(register-key-val 'move-sprite move-sprite registry-on-tick set-registry-on-tick!)

;; Called by boost-sprite-on-key to do the work
(define (boost-sprite s key ddx ddy)
  (define this 'boost-sprite)
  (when (tracing this) (eprintf "~a boosting sprite ~a ~a\n" this (sprite-key s) key))
  (list MUTATE-SPRITE
        ;; make-proxy will ignore unchanging values
        (make-proxy s
                    #:dx (+ (sprite-dx s) ddx)
                    #:dy (+ (sprite-dy s) ddy) ) ) )

;; Sprite Key -> ActionList
;; apply any boosts to its velocity
(define (boost-sprite-on-key params s k)
  (cond
    [(key=? k "up") (boost-sprite s k 0 DY-BOOST)]
    [(key=? k "down") (boost-sprite s k 0 (- DY-BOOST))]
    [(key=? k "left") (boost-sprite s k (- DX-BOOST) 0)]
    [(key=? k "right") (boost-sprite s k DX-BOOST 0)]
    [else '()] ) )
(register-key-val 'boost-sprite-on-key boost-sprite-on-key registry-on-key set-registry-on-key!)

;; ** Rendering Method

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
(register-key-val 'draw-sprite draw-sprite registry-to-draw set-registry-to-draw!)

;; ** big-bang callback procedures

;; Notice the similarity of
;; - gather-actions-on-tick and gather-actions-on-key
;; - update-world-on-tick and update-world-on-key
;; EXERCISE: Write 2 more general procedures replacing these 4!

;; Return updated sprites composed of all sprites returned by updates.
;; How might grouping actions by ACTION symbol impact efficiency??
(define (gather-actions-on-tick params sprites)
  (define this 'gather-actions-on-key)
  (foldr append ; append together all the returned action lists
         '()    ; starting with none
         (map   ; collect action lists by
          ;; calling sprite's on-tick method on itself
          (λ (sprite) (let* ( [method (sprite-on-tick sprite)]
                              [result (method params sprite)] )
                        (cond [(null? result) result] ; return empty list
                              [(and (pair? result) (pair? (car result))) result] ; return list of action lists
                              [(and (pair? result) (symbol? (car result))) (list result)] ; return list of one action list
                              [else (error this "invalid result ~a" result)] ) ))
          ;; for every sprite in sprites
          sprites ) ) )

;; return Package from applying on-tick methods to our sprites
(define (update-world-on-tick world)
  (define this 'update-world-on-tick)
  (let* ( [our-params (state-params world)]
          [our-sprites (state-ours world)]
          [actions (gather-actions-on-tick our-params our-sprites)] )
    (if (null? actions)
        world
        (begin
          (when (tracing this) (eprintf "~a actions: ~a\n" this actions))
          (let ( [ours-updated (update-sprites actions our-sprites)] )
            (when (tracing this) (eprintf "~a ours-updated ~a\n" this ours-updated))
            (make-package (make-state our-params ours-updated (state-theirs world)) actions) ) ) ) ) )

;; deliver key events to all sprites in the given list
;; returning a merged action list in Mail format
(define (gather-actions-on-key params sprites key)
  (define this 'gather-actions-on-key)
  (foldr append ; append together all the returned action lists
         '()    ; starting with none
         (map   ; collect action lists by
          ;; calling sprite's on-key method on itself
          (λ (sprite) (let* ( [method (sprite-on-key sprite)]
                              [result (method params sprite key)] )
                        (cond [(null? result) result] ; return empty list
                              [(and (pair? result) (pair? (car result))) result] ; return list of action lists
                              [(and (pair? result) (symbol? (car result))) (list result)] ; return list of one action list
                              [else (error this "invalid result ~a" result)] ) ))
          ;; for every sprite in sprites
          sprites ) ) )

;; return Package from applying on-key methods to our sprites
(define (update-world-on-key world key)
  (define this 'update-world-on-key)
  (let* ( [our-params (state-params world)]
          [our-sprites (state-ours world)]
          [actions (gather-actions-on-key our-params our-sprites key)] )
    (when (tracing this) (eprintf "~a actions: ~a\n" this actions))
    (if (null? actions)
        world
        (let ( [ours-updated (update-sprites actions our-sprites)] )
          (when (tracing this) (eprintf "~a ours-updated ~a\n" this ours-updated))
          (make-package (make-state our-params ours-updated (state-theirs world)) actions) ) ) ) )

;; Return a new canvas with our sprites drawn on top of
;; their sprites drawn on an empty canvas.
(define (draw-world world)
  (foldr draw-sprite
         (foldr draw-sprite EMPTY-CANVAS (state-theirs world))
         (state-ours world) ) )

; String -> WorldState
; create a world, hook it up to a server and start it running
(define (create-world a-name [server LOCALHOST])
  (define this 'create-world)
  (when (symbol? a-name) (set! a-name (symbol->string a-name)))
  (when (tracing this) (eprintf "~a ~a" this a-name))
  (big-bang 
   ;; initial state
   (make-state
    ;; parameters
    (make-params 0              ; will get a new value from the welcome message
                 'black         ; will get a new value after the welcome message
                 (if *testing* 0 DY-FALLING) )
    ;; our sprites and their sprites
    '() '() )
    [on-receive receive]
    [to-draw draw-world]
    [on-key update-world-on-key]
    [on-tick update-world-on-tick 1/10] ; 1/30 good, lower for testing
    [stop-when (λ (_) #f)] ; for now, never stop!
    [name a-name]
    [register server] ) )

;; ** Process Command Line or Enter REPL

(define *user* (make-parameter #f))
(define *host* (make-parameter LOCALHOST))
(define *cli* (make-parameter (positive? (vector-length (current-command-line-arguments)))))
(define *repl* (make-parameter (not (*cli*))))

(define (local) (*host* LOCALHOST))
(define (ngender) (*host* "ngender.net"))
(define (testing) (*testing* #t) (tracing #t))
(define (go [user #f]) (create-world (or user (*user*)) (*host*)))

(define args
  (if (not (*cli*))
      '()
      (command-line
       #:once-each
       [("-t" "--tracing") "trace everywhere" (tracing #t)]
       [("-T" "--testing") "make easier to test" (testing)]
       [("-H" "--host") host "server host" (*host* host)]
       [("-N" "--ngender") "host ngender.net" (*host* "ngender.net")]
       [("-i" "--repl") "enter repl, do not start client" (*repl* #t)]
       #:args (user . functions-to-trace)
       (cons user functions-to-trace)
       ) ) )

(when (*cli*)
  (let ( [this (find-system-path 'run-file)] )
    (when (null? args) (error this "user name required"))
    (*user* (car args))
    (let ( [names (map string->symbol (cdr args))] )
      ;; eval will throw an error if name is not globally bound!
      (for-each (λ (name) (unless (procedure? (eval name))
                            (error this "No procedure ~a to trace" name) ))
                names )
      (when (not (null? names)) (apply tracing (cons #t names))) ) ) )

;; this is only useful if we're already in a repl
(unless (*repl*) (go))
