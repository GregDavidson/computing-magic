;; (setq-local racket-repl-buffer-name "*sprites-worlds-client-repl*")
#lang racket/base
;; * Multiple Worlds Multiple Sprites Client

;; See sprites-worlds-game.org for information about the game.

;; ** Packages, Game Type Predicates, struct sprite, more Game Requires

;; The file sprites-words-games.rkt provides
;; - inter-client (inter-world) protocol information
;; - including a sprite-proxy structure
;; - You'll want to look it over carefully!

;; ** Requires and Early Definitions

;; Racket Package Requires

(require 2htdp/image
         2htdp/universe
         racket/cmdline
         racket/contract/base
         racket/contract/region
         racket/sequence
         racket/list
         racket/math )

;; Application Package Type Predicates

(require
 (contract-in "sprites-worlds-game.rkt"
              [universe? (-> any/c boolean?)]
              [message? (-> any/c boolean?)]
              [welcome-message? (-> any/c boolean?)]
              [goodbye-message? (-> any/c boolean?)]
              [world-sprites? (-> any/c boolean?)]
              [sprite-proxy? (-> any/c boolean?)]
              [sprite-id? (-> any/c boolean?)]
              [world-id? (-> any/c boolean?)]
              [params? (-> any/c boolean?)]
              [update? (-> any/c boolean?)]
              [actions? (-> any/c boolean?)] ) )

;; struct sprite is here for its type predicate

;; Each distinct sprite will have a unique key
(struct/contract
  sprite ( [image (or/c image? #f)]
           [x natural?] [y natural?]
           [dx integer?] [dy integer?]
           [on-tick (or/c procedure? #f)]
           [on-key (or/c procedure? #f)]
           [to-draw (or/c procedure? #f)] )
  #:mutable #:transparent )

;; Application Package Non-Type Requires

(require
 (contract-in "sprites-worlds-game.rkt"
              [make-universe (->* () (natural?) universe?)]
              [universe-world (-> universe? world-id? (or/c #f world-sprites?))]
              [universe-world-ids (-> universe? sequence?)]
              [universe-set! (-> universe? world-id? (or/c #f world-sprites?) void?)]
              [universe-drop! (-> universe? natural? void?)]

              [make-world-sprites (->* () (natural?) world-sprites?)]
              [world-sprite (-> world-sprites? sprite-id? (or/c #f sprite?))]
              [world-sprite-ids (-> world-sprites? sequence?)]
              [world-sprites (-> world-sprites? sequence?)]
              [world-sprite-set! (-> world-sprites? sprite-id? (or/c #f sprite?) void?)]
              [world-sprite-drop!  (-> world-sprites? natural? void?)]

              [sprite-proxy-sprite (-> sprite-proxy? sprite-id?)]
              [sprite-proxy-image (-> sprite-proxy? (or/c #f string? symbol?))]
              [sprite-proxy-x (-> sprite-proxy? (or/c #f natural?))]
              [sprite-proxy-y (-> sprite-proxy? (or/c #f natural?))]
              [sprite-proxy-dx (-> sprite-proxy? (or/c #f integer?))]
              [sprite-proxy-dy (-> sprite-proxy? (or/c #f integer?))]
              [sprite-proxy-on-tick (-> sprite-proxy? (or/c #f symbol?))]
              [sprite-proxy-on-key (-> sprite-proxy? (or/c #f symbol?))]
              [sprite-proxy-to-draw (-> sprite-proxy? (or/c #f symbol?))]
              [make-sprite-proxy
               (-> sprite-id?
                   (or/c #f string? symbol?)
                   (or/c #f natural?) (or/c #f natural?)
                   (or/c #f integer?) (or/c #f integer?)
                   (or/c #f symbol?) (or/c #f symbol?) (or/c #f symbol?)
                   sprite-proxy?)]

              [world-id->string (-> world-id? string?)]
              [message-world (-> message? world-id?)]
              [message-params (-> message? (or/c params? world-id?))]
              [make-params (-> world-id? image-color? integer? params?)]
              [params-world (-> params? world-id?)]
              [params-color (-> params? image-color?)]
              [params-falling (-> params? integer?)]
              [make-actions (-> params? (listof update?) actions?)]
              [actions-updates (-> actions? (listof update?))]

              [my-parameter (->* (any/c procedure? symbol?) ((or/c #f symbol? string?)) parameter?)]
              [*tracing* parameter?]
              [tracing (->* () (symbol?) boolean?)]
              [*testing* parameter?] ) )

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
        [else (error this "invalid color ~a" c)] ) )

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

;; NOTE: struct params fulfills this role and has been
;; moved to the sprites-worlds-games.rkt module.

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
          [found (findf (λ (pair) (equal? key (car pair))) alist)] )
    (unless found (setter proxy-registry (cons (cons key val) alist))) ) )

;; Given the key, return the val part of the (key . val) association
;; in the proxy-registry field accessed with the given getter function.
;; Returns #f if key is not present!!
(define (key->val key getter)
  (let ( [found (registry-find (λ (pair) (equal? key (car pair))) getter)] )
    (and found (cdr found)) ) )

;; Given the val, return the key part of the (key . val) association
;; in the proxy-registry field accessed with the given getter function.
;; Returns #f if key is not present!!
(define (val->key val getter)
  (let ( [found (registry-find (λ (pair) (equal? val (cdr pair))) getter)] )
    (and found (car found)) ) )

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
           #;(eprintf "~a warning: no image at ~a\n" this key)
           (eprintf "~a warning: substituting a ball for missing image ~a\n" this key)
           ;; PATCH: !!!
           (make-ball params) #;#f ] ) ) )

;; Given a sprite, convert it to a proxy which can be serialized
;; for transmission across a byte stream.
;; This is only used for brand new sprites!
;; See make-proxy for how we update sprites!
(define (sprite->proxy sprite-id s)
  (make-sprite-proxy
   sprite-id
   (val->key (sprite-image s) registry-image)
   (sprite-x s) (sprite-y s) (sprite-dx s) (sprite-dy s)
   (val->key (sprite-on-tick s) registry-on-tick)
   (val->key (sprite-on-key s) registry-on-key)
   (val->key (sprite-to-draw s) registry-to-draw) ) )

;; Given a sprite-proxy, convert it to a new sprite.
;; This is used with brand new sprites.
(define (proxy->sprite params sp)
  (define this 'proxy->sprite)
  (let ( [s (sprite
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
(define (make-proxy sprite-id s
                    #:image [image #f]
                    #:x [x #f] #:y [y #f] #:dx [dx #f] #:dy [dy #f]
                    #:tick [tick #f] #:key [key #f] #:draw [draw #f] )
  (make-sprite-proxy sprite-id
                     (and (delta (sprite-image s) image) (val->key image registry-image))
                     (delta (sprite-x s) x) (delta (sprite-y s) y)
                     (delta (sprite-dx s) dx) (delta (sprite-dy s) dy)
                     (and (delta (sprite-on-tick s) tick) (val->key tick registry-on-tick))
                     (and (delta (sprite-on-key s) key) (val->key key registry-on-key))
                     (and (delta (sprite-to-draw s) draw) (val->key draw registry-to-draw) ) ) )

(define (mutate-sprite-from-proxy! params s sp)
  (define this 'mutate-sprite-from-proxy!)
  (define original (struct-copy sprite s)) ; for debugging
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

;; Our WorldState consists of
;; - our parameters
;; - a universe? vector indexed by world ids
;;   - the elements of which are
;;     - world-sprites? vectors indexed by sprite ids
;;       - the elements of which are
;;         - sprites owned by that world
;; YES: worlds-states is a copy of
;;      all the sprites in the universe,
;;      not just our sprites!
(struct state ( params worlds-sprites )
  #:constructor-name make-state
  #:guard
  (struct-guard/c (or/c #f params?) (or/c #f universe?))
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

;; Given
;; - ActionMail - an action containing a list of sprite updates from the same world
;; - universe - a vector of worlds which are vectors of sprites
;; Side Effects: Carries out the sprite updates
(define (update-sprites! actions universe)
  (define this 'update-sprites!)
  ;; return the sprites left after removing those matching the keys in drop-lists
  (let* ( [world (message-world actions)]
          [params (message-params actions)]
          [updates (actions-updates actions)]
          [sprites (universe-world universe world)] )
    (map (λ (update)
           (cond
             [(sprite-id? update)
              (when sprites ; in case the world has been dropped
                (world-sprite-drop! sprites update) )]
             [(sprite-proxy? update)
              (let ( [sprite-id (sprite-proxy-sprite update)] )
                (when (not sprites) ; in case the world has been dropped
                  (universe-set! universe world (make-world-sprites sprite-id))
                  (set! sprites (universe-world universe world)) )
                (let ( [sprite (world-sprite sprites sprite-id)] )
                  (if sprite
                      (mutate-sprite-from-proxy! params sprite update)
                      (world-sprite-set! sprites sprite-id (proxy->sprite params update)) ) ) ) ]
             [else (error this "unknown update ~a" update)] ) )
           updates ) ) )

;; Given
;; - the state of the world
;; - action mail from the server
;; Return an updated WorldState
;; - possibly including Return Mail
;; ??? do we get a single mail message or a list of them ???
(define (receive world-state mail)
  (let ( [this 'receive]
         [existing-params (state-params world-state)]
         [existing-universe (state-worlds-sprites world-state)] )
    (when (tracing this) (eprintf "~a mail: ~a\n" this mail))
    (cond [(welcome-message? mail)
           (if (or existing-params existing-universe)
               (begin (eprintf "~a world ~a warning: attempting re-welcome!\n"
                               this (params-world existing-params) )
                      world-state )
               (let* ( [new-world (message-world mail)]
                       [new-color (choose-color new-world)]
                       [new-params (make-params new-world new-color
                                                (if (*testing*) 0 DY-FALLING) )] )
                 (when (tracing this)
                   (eprintf "~a world ~a color ~a\n" this new-world new-color) )
                 (let* ( [new-sprite (make-sprite new-params (make-ball new-params))]
                         [new-universe (make-universe (+ 1 new-world))]
                         [new-world-sprites (make-world-sprites)] )
                   (universe-set! new-universe new-world new-world-sprites)
                   (world-sprite-set! new-world-sprites 0 new-sprite)
                   (make-package (make-state new-params new-universe)
                                 (make-actions
                                  new-params
                                  (list (sprite->proxy 0 new-sprite)) ) ) ) ) ) ]
          [(goodbye-message? mail) (universe-drop! existing-universe (message-world mail))]
          [(actions? mail) (update-sprites! mail existing-universe) world-state]
          [else (error this "bad mail ~a" mail)] ) ) )

;; ** Action Procedures and Functions

(define DX-BOOST 6)
(define DY-BOOST 6)
(define DY-FALLING -4)

;; decay moves a value closer to its target value,
;; i.e. it makes boosts decay.
(define (decay value target [step 1])
  (cond [(> value target) (- value step)]
        [(< value target) (+ value step)]
        [else value] ) )

;; Why add text to the ball? 10% of humans are color blind!
;; EXERCISE: Use the provided name instead (or in addition to)
;; the id.
(define (make-ball params)
  (define this 'make-ball)
  (let ( [color (params-color params)]
         [id (params-world params)] )
    (when (tracing this) (eprintf "~a id ~a color ~a\n" this id color))
    (let ( [image (overlay (text (world-id->string id) 10 'black)
                           (circle 20 "solid" color) )] )
      ;; THIS ISN'T WORKING!!!
      (register-key-val 'make-ball image  registry-image set-registry-image!)
      image ) ) )
;; PATCH - Works as a default for now !!!
(register-key-val 'make-ball #f registry-image set-registry-image!)
(register-key-val 'make-ball make-ball registry-image set-registry-image!)

(define (make-sprite params image #:key [key #f]
                     #:x [x #f] #:y [y #f] #:dx [dx #f] #:dy [dy #f]
                     #:on-tick [on-tick #f] #:on-key [on-key #f] #:on-draw [on-draw #f] )
    (sprite
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
(define (move-sprite params sprite-id s)
  (let ( [image (sprite-image s)]
         [x (sprite-x s)] [y (sprite-y s)]
         [dx (sprite-dx s)] [dy (sprite-dy s)] )
    (let ( [xx (+ x dx)] [yy (+ y dy)] )
      (if (not (on-canvas? image xx yy))
          sprite-id                     ; drop
          (let ( [dxx (decay dx 0)] [dyy (decay dy (params-falling params))] )
            (if (and (= x xx) (= y yy) (= dx dxx) (= dy dyy))
                '() ; nothing changed
                (make-proxy sprite-id s #:x xx #:y yy #:dx dxx #:dy dyy) ) ) ) ) ) )
(register-key-val 'move-sprite move-sprite registry-on-tick set-registry-on-tick!)

;; Called by boost-sprite-on-key to do the work
(define (boost-sprite sprite-id s key ddx ddy)
  (define this 'boost-sprite)
  (when (tracing this) (eprintf "~a boosting sprite ~a ~a\n" this sprite-id key))
  (if (and (zero? ddx) (zero? ddy))
      '()
      (make-proxy sprite-id s
                  #:dx (+ (sprite-dx s) ddx)
                  #:dy (+ (sprite-dy s) ddy) ) ) )

;; Sprite Key -> ActionList
;; apply any boosts to its velocity
(define (boost-sprite-on-key params sprite-id s k)
  (cond
    [(key=? k "up") (boost-sprite sprite-id s k 0 DY-BOOST)]
    [(key=? k "down") (boost-sprite sprite-id s k 0 (- DY-BOOST))]
    [(key=? k "left") (boost-sprite sprite-id s k (- DX-BOOST) 0)]
    [(key=? k "right") (boost-sprite sprite-id s k DX-BOOST 0)]
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

;; Return list of all actions returned by sprite on-tick methods
;; on-tick methods can return
;; - an empty list, meaning no actions
;; - a list of actions
;; - a single action
(define (gather-actions-on-tick params sprites)
  (define this 'gather-actions-on-tick)
  ;; We're modeling the sprites container as a primitive vector
  ;; - gvector currently, although that could change
  ;; we're going to do this iteratively for a change - enjoy!
  (let ( [updates
          (foldr append ; append the lists of updates into one
                 '()    ; starting with none
                 (for/list ( [i (world-sprite-ids sprites)] )
                   (let ( [sprite (world-sprite sprites i)] )
                         ;; call sprite's on-tick method on itself
                         (let* ( [method (sprite-on-tick sprite)]
                                 [result (method params i sprite)] )
                           (if (list? result) result (list result)) ) ) ) ) ] )
    ;; package the updates, if any, into an actions structure
    (if (null? updates) #f (make-actions params updates)) ) )

;; return Package from applying on-tick methods to our sprites
(define (update-world-on-tick world-state)
  (define this 'update-world-on-tick)
  (let ( [universe (state-worlds-sprites world-state)]
          [our-params (state-params world-state)] )
    (if (not (and universe our-params))
        world-state ; we've not be welcomed yet
        (let* ( [our-world (params-world our-params)]
                [our-sprites (universe-world universe our-world)]
                [actions (gather-actions-on-tick our-params our-sprites)] )
          (if (not actions)
              world-state ; no updates this time
              (begin
                (when (tracing this) (eprintf "~a actions: ~a\n" this actions))
                (let ( [ours-updated (update-sprites! actions universe)] )
                  (when (tracing this) (eprintf "~a ours-updated ~a\n" this ours-updated))
                  (make-package world-state actions) ) ) ) ) ) ) )

;; deliver key events to all sprites in the given list
;; returning a merged action list in Mail format
(define (gather-actions-on-key params sprites key)
  (define this 'gather-actions-on-key)
  ;; We're modeling the sprites container as a primitive vector
  ;; - gvector currently, although that could change
  ;; we're going to do this iteratively for a change - enjoy!
  (let ( [updates
          (foldr append ; append the lists of lists of updates into one list
                 '()    ; starting with none
                 (for/list ( [i (world-sprite-ids sprites)] )
                   ;; generate a list of updates for each sprite in the world
                   (let ( [sprite (world-sprite sprites i)] )
                              ;; call sprite's on-key method on itself
                              (let* ( [method (sprite-on-key sprite)]
                                      [result (method params i sprite key)] )
                                (when tracing (eprintf "~a result: ~a\n" this result))
                                (if (list? result) result (list result)) ) ) ) ) ] )
    ;; package the updates, if any, into an actions structure
    (if (null? updates) #f (make-actions params updates)) ) )

;; return Package from applying on-key methods to our sprites
(define (update-world-on-key world-state key)
  (define this 'update-world-on-key)
  (let ( [universe (state-worlds-sprites world-state)]
         [our-params (state-params world-state)] )
    (if (not (and universe our-params))
        world-state ; we've not be welcomed yet
        (let* ( [our-world (params-world our-params)]
                [our-sprites (universe-world universe our-world)]
                [actions (gather-actions-on-key our-params our-sprites key)] )
          (when (tracing this) (eprintf "~a actions: ~a\n" this actions))
          (if (not actions)
              world-state ; no updates from this key
              (let ( [ours-updated (update-sprites! actions universe)] )
                (when (tracing this) (eprintf "~a ours-updated ~a\n" this ours-updated))
                (make-package world-state actions) ) ) ) ) ) )

;; Let all the sprites in the universe draw on an empty
;; canvas and return that canvas.
;; EXERCISE: How could we ensure that our sprites are always drawn on top??
(define (draw-world world-state)
  (define this 'draw-world)
  (let ( [universe (state-worlds-sprites world-state)]
         [canvas EMPTY-CANVAS] )
    (when universe ; initialization has happened
      (for ( [world-id (universe-world-ids universe)] )
        (let ( [sprites (universe-world universe world-id)] )
          (when (not (world-sprites? sprites)) (error this "Not world-sprites ~a" sprites))
            (for ( [sprite-id (world-sprite-ids sprites)] )
              (let ( [sprite (world-sprite sprites sprite-id)] )
                  (let ( [drawing-method (sprite-to-draw sprite)] )
                    (set! canvas (drawing-method sprite canvas)) ) ) ) ) ) )
    canvas ) )

;; Should we send the universe server
;; a message before we just detach??
(define (no-sprites-left world-state)
  (define this 'draw-world)
  (let ( [universe (state-worlds-sprites world-state)]
         [our-params (state-params world-state)] )
    (if (not our-params)
        #f ; no sprites yet!
        (let* ( [our-world (params-world our-params)]
                [our-sprites (universe-world universe our-world)] )
          (not (sequence-ormap sprite? (world-sprites our-sprites))) ) ) ) )

; String -> WorldState
; create a world, hook it up to a server and start it running
(define (create-world a-name [server LOCALHOST])
  (define this 'create-world)
  (when (symbol? a-name) (set! a-name (symbol->string a-name)))
  (when (tracing this) (eprintf "~a ~a" this a-name))
  (big-bang
   (make-state #f #f) ; to be initialized from welcome message
   [on-receive receive]
   [to-draw draw-world]
   [on-key update-world-on-key]
   [on-tick update-world-on-tick 1/10] ; 1/30 good, lower for testing
   [stop-when no-sprites-left]
   [name a-name]
   [register server] ) )

;; ** Process Command Line or Enter REPL

(define *user* (my-parameter #f (or/c symbol? string?) 'user))
(define *host* (my-parameter LOCALHOST string? 'host))
(define *args* (my-parameter (let ( [cl (current-command-line-arguments)] )
                                 (if (positive? (vector-length cl)) cl #f) )
                               (or/c #f vector?) 'command-arguments ))

(define (local) (*host* LOCALHOST))
(define (ngender) (*host* "ngender.net"))

(define args
  (if (not (*args*))
      '()
      (command-line
       #:once-each
       [("-t" "--tracing") "trace everywhere" (*tracing* #t)]
       [("-T" "--testing") "ease testing and trace everywhere" (begin (*testing* #t) (*tracing* #t))]
       [("-H" "--host") host "server host" (*host* host)]
       [("-N" "--ngender") "host ngender.net" (*host* "ngender.net")]
       #:args (user . functions-to-trace)
       (cons user functions-to-trace)
       ) ) )

;; Prompt and then read an input line as a string
(define (get-string-line prompt)
  (eprintf "~a: " prompt)
  (read-line) )

(when (*args*)
  (let ( [this (find-system-path 'run-file)] )
    (when (null? args) (error this "user name required"))
    (*user* (car args))
    (let ( [names (map string->symbol (cdr args))] )
      ;; warn us if name is not bound to a procedure
      (for-each (λ (name) (unless (procedure? (eval name))
                            (eprintf "~a: No procedure ~a to trace\n" this name) ))
                names )
      (when (not (null? names)) (apply tracing (cons #t names))) ) ) )

(define (go #:user [user #f] #:host [host #f] #:trace [trace #f] #:test [test #f])
  (define this 'go)
  (parameterize ( [*user* (or user (*user*) (get-string-line "User name"))]
                  [*host* (or host (*host*))]
                  [*tracing* (or trace (*tracing*))]
                  [*testing* (or test (*testing*))] )
    (eprintf "~a user ~a host ~a tracing ~a testing ~a\n"
             this (*user*) (*host*) (*tracing*) (*testing*) )
    (create-world (*user*) (*host*)) ) )

(if (*args*)
    (go)
    (let ( [yes (regexp "[yY].*")]
           [reply (get-string-line "run world client? [y/n]")] )
      (when (regexp-match yes reply)
        (parameterize ( [*tracing* #t] )
          (go) ) ) ) )
