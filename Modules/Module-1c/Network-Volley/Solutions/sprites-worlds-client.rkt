;; (setq-local racket-repl-buffer-name "*sprites-worlds-client-repl*")
#lang racket/base
;; * Multiple Worlds Multiple Sprites Client

;; See sprites-worlds-game.org for information about the game.
;; - change this name!!

;; The file sprites-words-client.rkt provides
;; - a Framework for writing 2D Game Clients
;;   based on Worlds of Sprites

;; ** What We Require

;; (struct-out) forms don't always do the job so we
;; sometimes comment them out and follow them with
;; the individual exports.

;; having a little trouble with importing struct params-base
;; so let's do it up front and the simplest way
(require (only-in "sprites-worlds-game.rkt"
                 params-base params-base? make-params-base params-base-world ))

;; import structure ids
(require (only-in "sprites-worlds-game.rkt"
                  params-base
                  message
                  welcome-message
                  goodbye-message
                  sprite-proxy
                  actions ))

;; Racket Package Requires

(require 2htdp/image
         2htdp/universe
         racket/cmdline
         racket/contract/base
         racket/contract/region
         racket/sequence
         racket/list
         racket/math
         racket/function )

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
              ;;[params-base? (-> any/c boolean?)]
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
              [universe-worlds (-> universe? sequence?)]
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
              [make-message (-> (or/c  params-base? world-id?) message?)]
              [message-world (-> message? world-id?)]
              [message-params (-> message? (or/c params-base? world-id?))]
              [welcome-message-alist (-> welcome-message? (listof (cons/c symbol? any/c)))]
              ;; [make-params-base (-> world-id? params-base?)]
              ;; [params-base-world (-> params-base? world-id?)]
              ;; see comment by definition of struct params !!
              ;; [make-params (-> world-id? string? integer? params?)]
              ;; [params-world (-> params? world-id?)]
              ;; [params-color (-> params? image-color?)]
              ;; [params-falling (-> params? integer?)]
              [make-actions (-> params-base? (listof update?) actions?)]
              [actions-updates (-> actions? (listof update?))]

              [my-parameter (->* (any/c procedure? symbol?) ((or/c #f symbol? string?)) parameter?)]
              [*tracing* parameter?]
              [tracing (->* () (symbol?) boolean?)]
              [*testing* parameter?] ) )

;; provide contracts for these
(require (only-in "sprites-worlds-game.rkt"
                  make-welcome
                  make-goodbye ))

;; Generic alist management procedures
(require (only-in "sprites-worlds-game.rkt" obj-alist-procs))

;; ** What We Provide

;; (struct-out) forms don't always do the job so we
;; sometimes comment them out and follow them with
;; the individual exports along with related bindings.


;; *** What We Re-Export from Game

;; having a little trouble with exporting
;; struct params-base and struct params
;; so let's do it up front and the simplest way
(require (only-in "sprites-worlds-game.rkt"
                 params-base params-base? make-params-base params-base-world ))

(provide ;; the easy way isn't working:
 #;(struct-out sprite-proxy)
 ;; sprites-worlds-game.rkt:381:2: struct-out: no binding for structure-type identifier in: struct:sprite-proxy
 ;; so we do it the explicit way
 sprite-proxy sprite-proxy? make-sprite-proxy
 sprite-proxy-x sprite-proxy-y sprite-proxy-dx sprite-proxy-dy
 sprite-proxy-on-tick sprite-proxy-on-key
 sprite-proxy-to-draw
 ;; easy way
 #;(struct-out message)
 ;; explicit way
 message message? make-message message-params
 message-world
 ;; easy way
 #;(struct-out welcome-message)
 welcome-message welcome-message? welcome-message-alist
 make-welcome
 ;; easy way
 #; (struct-out goodbye--message)
 ;; explicit way
 goodbye-message goodbye-message? make-goodbye
 ;; easy way
 #; (struct-out actions)
 ;; explicit way
  actions actions? make-actions actions-updates
 )

;; 2htdp Re-Exports
#;(provide empty-scene
         image-width
         image-height
         make-package
         key=?
         LOCALHOST
         big-bang )

;; separate into Re-Exports vs. exports of local defines
(provide world-sprites
         command-line
         my-parameter
         proxy->sprite gather-actions-on-tick gather-actions-on-key
         make-proxy
         universe-world
         choose-color )

(provide 
 params-base params-base? make-params-base params-base-world
 params params? make-params params-world params-color params-falling )

(provide *tracing* tracing *testing*)

;; *** What We Provide Of Our Own

(provide universe?
         world-sprites?
         sprite-id?
         world-id?
         )

(provide CANVAS-WIDTH
         CANVAS-HEIGHT
         (struct-out sprite)
         mutate-sprite-from-proxy!
         no-state-yet
         no-state-yet?
         (struct-out client-state)
         update-sprites!
         receive )

;; Can't use struct-out
#; (struct-out sprite-proxy)  ; doesn't work!
;; sprites-worlds-game.rkt:331:2: struct-out: no binding for structure-type identifier in: struct:sprite-proxy
#; (struct-out message)
#; (struct-out actions)

(provide sprite-proxy
         sprite-proxy?
         make-sprite-proxy
         sprite-proxy-sprite 
         sprite-proxy-image
         sprite-proxy-x 
         sprite-proxy-y 
         sprite-proxy-dx 
         sprite-proxy-dy 
         sprite-proxy-on-tick
         sprite-proxy-on-key 
         sprite-proxy-to-draw )

(provide make-message message? message-params)

(provide welcome-message? welcome-message-alist)

(provide make-actions actions? actions-updates)

(provide on-tick-register! on-key-register! image-register!)

(provide receive no-sprites-left? draw-world update-world-on-key update-world-on-tick)

(provide get-string-line)

;; What else to provide???


;; * What We Define

;; ** Our World Parameters

;; Our World Parameters give context to
;; any procedures which are creating
;; foreign entities, i.e. sprites which
;; belong to other worlds

(struct params params-base (color falling)
  #:constructor-name make-params
  #:prefab )

(define params-world params-base-world)


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

;; ** Sizes, Constants and Colors

;; *** Our Canvas

(define CANVAS-WIDTH 400)               ; pixels
(define CANVAS-HEIGHT 300)              ; pixels
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; *** Ball Sizes

(define (half n) (quotient n 2))

(define BALL-RADIUS 20)
(define BALL-MARGIN 1)
(define BALL-SIZE (* 2 (+ BALL-RADIUS BALL-MARGIN)))
(define BALL-TEXT-SIZE (half BALL-RADIUS))

;; *** Ball Accelerations

(define DX-BOOST 6)
(define DY-BOOST 6)
(define DY-FALLING -4) ; disabled when *testing*

;; *** a proxy template for our first sprite

;; This will be copied when we receive our welcome message
;; after we connect to the universe server.  Some of the
;; field values may be changed in the copy.  This structure
;; will also be used in testing.

(define PROXY-0  (make-sprite-proxy
                  0 'make-ball ; sprite-id image-function
                  (half CANVAS-WIDTH) (- CANVAS-HEIGHT BALL-SIZE)
                  0 0 ; dx dy
                  'move-sprite 'boost-sprite-on-key 'draw-sprite ))

;; *** Managing Colors

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

;; here's the structure type
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

;; instantiate the registry structure object
(define proxy-registry (registry '() '() '() '()))

;; define the procedures to manage the association lists

(define-values (image-register! image-key->val image-val->key)
  (obj-alist-procs proxy-registry registry-image set-registry-image!) )

(define-values (on-tick-register! on-tick-proc on-tick-name)
  (obj-alist-procs proxy-registry registry-on-tick set-registry-on-tick!) )

(define-values (on-key-register! on-key-proc on-key-name)
  (obj-alist-procs proxy-registry registry-on-key set-registry-on-key!) )

(define-values (to-draw-register! to-draw-proc to-draw-name)
  (obj-alist-procs proxy-registry registry-to-draw set-registry-to-draw!) )

;; Given a path or symbol as a key, return the associated image
;; and ensure it's registered.
(define (get-image params key)
  (define this 'get-image-key)
  (let* ( [fallback ; better than throwing an error??
           (λ ()
             (eprintf "~a key ~a failed, substituting ball\n" this key)
             (make-ball params) ) ]
          [found (image-key->val key)]
          [bitmap/file/cache
           (λ (path)
             (with-handlers ( [exn:fail? (λ (exn) (fallback))] )
               (let ( [image (bitmap/file path)] )
                 (image-register! path image)
                 image ) ) ) ] )
    (cond [(image? found) found]
          [(string? found) (bitmap/file/cache found)]
          [(string? key) (bitmap/file/cache key)]
          [(procedure? found) (found params)]
          [else (fallback)] ) ) )

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
             (on-tick-proc (sprite-proxy-on-tick sp))
             (on-key-proc (sprite-proxy-on-key sp))
             (to-draw-proc (sprite-proxy-to-draw sp)) )] )
    (when (tracing this) (eprintf "~a sprite-proxy-key: ~a -> ~a" this sp s ))
    s ) )

;; Given a possible new value, a structure, a getter and a possible value mapper
;; Prefer the new value if it exists, otherwise use the getter to get the old value.
;; Use the mapper to substitute a serializable value.
(define (delta new s getter [mapper identity])
  (mapper (or new (getter s))) )

;; EXERCISE:
;; Only provide values for fields which need to be changed.
;; Here's what delta might look like:
#;(define (delta new s getter [mapper identity])
    (and new (not (equal? (getter s) new)) (mapper new)) )
;; But: This won't work for any world which missed the creation proxy.
;; So: How could we solve this problem???

;; Make a sprite-proxy which represents desired updates to a sprite
;; This is used with MUTATE-SPRITE actions.
(define (make-proxy sprite-id s
                    #:image [image #f]
                    #:x [x #f] #:y [y #f] #:dx [dx #f] #:dy [dy #f]
                    #:tick [tick #f] #:key [key #f] #:draw [draw #f] )
  (make-sprite-proxy sprite-id
                     (delta image s sprite-image image-val->key)
                     (delta x s sprite-x) (delta y s sprite-y)
                     (delta dx s sprite-dx) (delta dy s sprite-dy)
                     (delta tick s sprite-on-tick on-tick-name)
                     (delta key s sprite-on-key on-key-name)
                     (delta draw s sprite-to-draw to-draw-name) ) )

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
    (set-sprite-on-tick! s (or (on-tick-proc (sprite-proxy-on-tick sp))
                               (begin
                                 (error this "no on-tick in ~a" sp)
                                 (sprite-on-tick s) ) )) )
  (when (sprite-proxy-on-key sp)
    (set-sprite-on-key! s (or (on-key-proc (sprite-proxy-on-key sp))
                              (begin
                                (error this "no on-key in ~a" sp)
                                (sprite-on-key s) ) )) )
  (when (sprite-proxy-to-draw sp)
    (set-sprite-to-draw! s (or (to-draw-proc (sprite-proxy-to-draw sp))
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
(define no-state-yet #f)
(define (no-state-yet? x) (eq? no-state-yet x))
(struct client-state ( params worlds-sprites )
  #:constructor-name make-state
  ;; #:guard (struct-guard/c params? universe?)
  #:transparent )

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
  (let* ( [world-id (message-world actions)]
          [params (message-params actions)]
          [updates (actions-updates actions)]
          [world-sprites (universe-world universe world-id)] )
    (when (tracing this) (eprintf "~a params ~a updates ~a sprites ~a\n" this params updates world-sprites))
    (map (λ (update)
           (cond
             [(sprite-id? update) ; drop this sprite
              (when world-sprites ; in case the world has been dropped
                (when (tracing this) (eprintf "~a dropping sprite ~a from world ~a\n"
                                              this update world-id ))
                (world-sprite-drop! world-sprites update) ) ]
             [(sprite-proxy? update) ; create or update this sprite
              (let ( [sprite-id (sprite-proxy-sprite update)] )
                (when (not world-sprites) ; in case the world has been dropped
                  (when (tracing this) (eprintf "~a creating new world ~a\n"
                                                this world-id ))
                  (universe-set! universe world-id (make-world-sprites sprite-id)) )
                (let* ( [sprites (or world-sprites (universe-world universe world-id))]
                        [sprite (world-sprite sprites sprite-id)] )
                  (if sprite ; it already exists, so mutate it in place
                      (mutate-sprite-from-proxy! params sprite update)
                      ;; it doesn't exist, so create it
                      ;; should we check if the sprite-proxy is complete???
                      (let ( [new-sprite (proxy->sprite params update)] )
                        (when (tracing this) (eprintf "~a adding new sprite ~a to world ~a at index ~a\n"
                                                      this new-sprite  world-id sprite-id ))
                        (world-sprite-set! sprites sprite-id new-sprite) ) ) ) ) ]
             [else (error this "unknown update ~a" update)] ) )
         updates ) ) )

;; Given
;; - the state of the world
;; - action mail from the server
;; Return an updated WorldState
;; - possibly including Return Mail
(define (do-receive world-state mail)
  (define this 'receive)
  (if world-state
      (let ( [params (client-state-params world-state)]
             [universe (client-state-worlds-sprites world-state)] )
        (cond
          [(goodbye-message? mail)
           (universe-drop! universe (message-world mail)) ]
          [(actions? mail) (update-sprites! mail universe)]
          [else (error this "bad mail ~a for state ~a" mail world-state)] )
        ;; return world-state after mutation by update-sprites!
        world-state )
      (begin
        (unless (welcome-message? mail)
          (error this "expected welcome instead of ~a" mail) )
        ;; build our state, our world and our first sprite
        (let* ( [new-world-id (message-world mail)]
                [new-color (choose-color new-world-id)]
                [new-params (make-params new-world-id
                                         new-color
                                         (if (*testing*) 0 DY-FALLING) )] )
          #;(when (tracing this)
              (eprintf "~a world ~a color ~a\n" this new-world-id new-color) )
          (let* (
                 ;; create a proxy to represent our first sprite
                 [new-sprite-proxy
                  (struct-copy sprite-proxy PROXY-0 [dx (params-falling new-params)]) ]
                 ;; put it in an actions message
                 [new-actions (make-actions new-params (list new-sprite-proxy))]
                 ;; to perform on our new universe
                 [new-universe (make-universe (+ 1 new-world-id))] )
            ;; update locally
            #;(when (tracing this) (eprintf "~a actions ~a\n" this new-actions))
            (update-sprites! new-actions new-universe)
            #;(when (tracing this)
              (eprintf "~a new params ~a new actions ~a new universe ~a\n" this new-params new-actions new-universe) )
            ;; return new state, with mail
            (make-package (make-state new-params new-universe) new-actions) ) ) ) ) )

(define (receive world-state mail)
  (define this 'receive)
  (when (tracing this) (eprintf "~a mail ~a for world-state ~a\n" this mail world-state))
  (let ( [result (do-receive world-state mail)] )
    (when (tracing this) (eprintf "~a returning ~a\n" this result))
    result ) )

;; ** Action Procedures and Functions

;; decay moves a value closer to its target value,
;; i.e. it makes boosts decay.
(define (decay value target [step 1])
  (cond [(> value target) (- value step)]
        [(< value target) (+ value step)]
        [else value] ) )

;; Why add text to the ball? 10% of humans are color blind!
;; PRACTICE: Add high-contrast patterns to supplement color
;; everywhere color is being used to distinguish visual elements. 
;; EXERCISE: Use the provided name instead (or in addition to)
;; the id.

(define (make-ball params)
  (define this 'make-ball)
  (let ( [color (params-color params)]
         [id (params-world params)] )
    (when (tracing this) (eprintf "~a id ~a color ~a\n" this id color))
    (let ( [image (overlay (text (world-id->string id) BALL-TEXT-SIZE 'black)
                           (circle BALL-RADIUS "solid" color) )] )
      image ) ) )
(image-register! 'make-ball make-ball)

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
(on-tick-register! 'move-sprite move-sprite)

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
(on-key-register! 'boost-sprite-on-key boost-sprite-on-key)

;; ** Rendering Method

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
(to-draw-register! 'draw-sprite draw-sprite)

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
  ;; we're going to do this iteratively for a change.
  ;; EXERCISE: How might this be done functionally, i.e. with
  ;; mapping and/or folding operations??
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
(define (do-update-world-on-tick world-state)
  (define this 'update-world-on-tick)
  (if (no-state-yet? world-state)
      world-state  ; we've not been welcomed yet
      (let ( [universe (client-state-worlds-sprites world-state)]
             [our-params (client-state-params world-state)] )
        (let* ( [our-world-id (params-world our-params)]
                [our-sprites (universe-world universe our-world-id)]
                [actions (gather-actions-on-tick our-params our-sprites)] )
          (if (not actions)
              world-state ; no updates this time
              (begin
                (when (tracing this) (eprintf "~a actions: ~a\n" this actions))
                (update-sprites! actions universe) ; may invalidate our-sprites binding
                (when (tracing this) (eprintf "~a ours-updated ~a\n" this (universe-world universe our-world-id)))
                (make-package world-state actions) ) ) ) ) ) )

;; create a clojure around do-update-world-on-tick which
;; will show us any changing inputs or outputs
;; when we're tracing and testing
(define update-world-on-tick
  (let ( [this 'update-world-on-tick]
         [first-time #t]
         [last-world-state #f]
         [last-result #f] )
    (λ (world-state)
      (when (and (tracing 'this) (*testing*)
                 (or first-time (not (equal? world-state last-world-state))) )
        (eprintf "~a input world-state ~a\n" this world-state) )
      (let ( [result (do-update-world-on-tick world-state)] )
        (when (and (tracing 'this) (*testing*)
                   (or first-time (not (equal? result result))) )
          (eprintf "~a returning ~a\n" this result) )
        (set! first-time #f)
        (set! last-world-state world-state)
        (set! last-result result)
        result ) ) ) )

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
(define (do-update-world-on-key world-state key)
  (define this 'update-world-on-key)
  (if (no-state-yet? world-state)
      world-state
      (let ( [universe (client-state-worlds-sprites world-state)]
             [our-params (client-state-params world-state)] )
        (if (not (and universe our-params))
            world-state ; we've not be welcomed yet
            (let* ( [our-world-id (params-world our-params)]
                    [our-sprites (universe-world universe our-world-id)]
                    [actions (gather-actions-on-key our-params our-sprites key)] )
              (when (tracing this) (eprintf "~a actions: ~a\n" this actions))
              (if (not actions)
                  world-state ; no updates from this key
                  (begin
                    (update-sprites! actions universe) ; may invalidate our-sprites binding
                    (when (tracing this) (eprintf "~a ours-updated ~a\n" this (universe-world universe our-world-id)))
                    (make-package world-state actions) ) ) ) ) ) ) )

(define (update-world-on-key world-state key)
  (define this 'update-world-on-key)
  (when (tracing this) (eprintf "~a world-state ~a key ~a\n" this world-state key))
  (let ( [result (do-update-world-on-key world-state key)] )
    (when (tracing this) (eprintf "~a returning ~a\n" this result))
    result ) )

;; Use the drawing methods of all sprites in the universe
;; to compose their images onto a single canvas and return
;; the result.
;; EXERCISE: How could we ensure that our sprites are always drawn on top,
;; i.e. when our sprites overlap those from other worlds??
(define (draw-world world-state)
  (define this 'draw-world)
  (if (no-state-yet? world-state)
      EMPTY-CANVAS
      ;; compose the worlds of our world state
      (sequence-fold
       (λ (c w)
         ;; compose the sprites of world w
         (sequence-fold (λ (c s) (let ( [drawing-method (sprite-to-draw s)] )
                                   (drawing-method s c) ))
                        c ; start with this canvas
                        ;; generate a sequence of all sprites in our world
                        (world-sprites w) ) )
       EMPTY-CANVAS ; start with this canvas
       ;; generate a sequence of all worlds in our universe
       (universe-worlds (client-state-worlds-sprites world-state)) ) ) )

;; Should we send the universe server
;; a message before we just detach??
(define (no-sprites-left? world-state)
  (define this 'no-sprites-left?)
  (and (not (no-state-yet? world-state))
       (let* ( [universe (client-state-worlds-sprites world-state)]
               [params (client-state-params world-state)]
               [world-id (params-world params)]
               [sprites (universe-world universe world-id)] )
         (or (not sprites) ; our world is missing entirely!
             (not (sequence-ormap sprite? (world-sprites sprites))) ) ) ) )
