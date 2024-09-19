;; -*- mode: racket; racket-repl-buffer-name: "*sprites-client-framework*"; -*-
#lang racket/base
;; * Multiple Sprites in Multiple Worlds Client Framework

;; See sprites-universes.org for more information

;; ** What We Require

;; Racket Package Requires

(require (only-in 2htdp/image
                  image? image-color? place-image image-height image-width
                  bitmap/file )
         (only-in 2htdp/universe make-package)
         racket/cmdline
         racket/contract/base
         racket/contract/region
         racket/sequence
         racket/stream
         racket/list
         racket/math
         racket/function )

;; Require Structure IDs

(require (only-in "sprites-framework.rkt"
                  sprite-proxy params-base message ))

;; Require Type Predicates

(require
 (contract-in "sprites-framework.rkt"
              [universe? (-> any/c boolean?)]
              [message? (-> any/c boolean?)]
              [welcome-message? (-> any/c boolean?)]
              [goodbye-message? (-> any/c boolean?)]
              [world? (-> any/c boolean?)]
              [sprite-proxy? (-> any/c boolean?)]
              [sprite-id? (-> any/c boolean?)]
              [world-id? (-> any/c boolean?)]
              [params-base? (-> any/c boolean?)]
              [update? (-> any/c boolean?)]
              [actions? (-> any/c boolean?)] ) )

;; ** struct sprite

;; struct sprite needs to be here before the contract-in expressions needing its
;; type predicate

;; Each distinct sprite will have a unique key
(struct/contract
  sprite ( [image (or/c image? #f)]
           [x natural?] [y natural?]
           [dx integer?] [dy integer?]
           [on-tick (or/c procedure? #f)]
           [on-key (or/c procedure? #f)]
           [to-draw (or/c procedure? #f)] )
  #:mutable #:transparent )

;; Non-Type Requires

(require
 (contract-in "sprites-framework.rkt"
              [make-universe (->* () (natural?) universe?)]
              [universe-world (-> universe? world-id? (or/c #f world?))]
              [universe-worlds (-> universe? sequence?)]
              [universe-set! (-> universe? world-id? (or/c #f world?) void?)]
              [universe-drop! (-> universe? natural? void?)]

              [make-world (->* () (natural?) world?)]
              [world-sprite (-> world? sprite-id? (or/c #f sprite?))]
              [world-sprite-ids (-> world? sequence?)]
              [world-sprites (-> world? sequence?)]
              [world-set! (-> world? sprite-id? (or/c #f sprite?) void?)]
              [world-drop!  (-> world? natural? void?)] ) )

(require
 (contract-in "sprites-framework.rkt"
              [sprite-proxy-sprite (-> sprite-proxy? sprite-id?)]
              [sprite-proxy-image (-> sprite-proxy? (or/c string? symbol?))]
              [sprite-proxy-x (-> sprite-proxy? natural?)]
              [sprite-proxy-y (-> sprite-proxy? natural?)]
              [sprite-proxy-dx (-> sprite-proxy? integer?)]
              [sprite-proxy-dy (-> sprite-proxy? integer?)]
              [sprite-proxy-on-tick (-> sprite-proxy? symbol?)]
              [sprite-proxy-on-key (-> sprite-proxy? symbol?)]
              [sprite-proxy-to-draw (-> sprite-proxy? symbol?)]
              [make-sprite-proxy
               (-> sprite-id?
                   (or/c string? symbol?)
                   natural? natural?
                   integer? integer?
                   symbol? symbol? symbol?
                   sprite-proxy?)] ) )

(require
 (contract-in "sprites-framework.rkt"
              [world-id->string (-> world-id? string?)]
              [make-message (-> (or/c  params-base? world-id?) message?)]
              [message-world (-> message? world-id?)]
              [message-params (-> message? (or/c params-base? world-id?))]
              [make-welcome (-> world-id? (listof (cons/c symbol? any/c)) welcome-message?)]
              [welcome-message-alist (-> welcome-message? (listof (cons/c symbol? any/c)))]
              [make-goodbye (-> (or/c  params-base? world-id?) goodbye-message?)]
              [make-params-base (-> world-id? params-base?)]
              [params-base-world (-> params-base? world-id?)]
              [make-actions (-> params-base? (listof update?) actions?)]
              [actions-updates (-> actions? (listof update?))] ) )

(require
 (contract-in "sprites-framework.rkt"
              [my-parameter (->* (any/c procedure? symbol?) ((or/c #f symbol? string?)) parameter?)]
              [*tracing* parameter?]
              [tracing (->* () (symbol?) boolean?)]
              [*testing* parameter?]
              [trace-procs (-> (listof string?) void)]
              [get-string-line (-> string? string?)]
              [program-is-standalone? (-> boolean?)] ) )

;; Returns 3 alist management procedures
;; TODO Write a contract for this!!
(require (only-in "sprites-framework.rkt" obj-alist-procs))

;; ** What We Provide

;; *** What We Re-Export from Game

(provide
 #;(struct-out sprite-proxy)
 sprite-proxy sprite-proxy? make-sprite-proxy
 sprite-proxy-x sprite-proxy-y sprite-proxy-dx sprite-proxy-dy
 sprite-proxy-on-tick sprite-proxy-on-key
 sprite-proxy-to-draw
 #;(struct-out message)
 message message? make-message message-params
 message-world
 #;(struct-out welcome-message)
 welcome-message? welcome-message-alist
 make-welcome
 #; (struct-out goodbye--message)
 goodbye-message? make-goodbye
 )

(provide universe?
         make-universe
         world?
         sprite-id?
         world-drop!
         world-id?
         world-id->string
         )

(provide world-sprites
         command-line
         my-parameter
         proxy->sprite gather-actions-on-tick gather-actions-on-key
         make-proxy
         universe-world )

(provide #;(struct-out sprite-proxy)
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

(provide
 params-base params-base? make-params-base params-base-world )
;; params params? make-params params-world params-color params-falling )

(provide make-message message? message-params)

(provide welcome-message? welcome-message-alist)

(provide make-actions actions? actions-updates)

(provide *tracing* tracing *testing* trace-procs)

(provide get-string-line program-is-standalone?)

;; *** What We Provide Of Our Own

(provide (struct-out sprite)
         mutate-sprite-from-proxy!
         no-state-yet
         no-state-yet?
         (struct-out client-state)
         update-sprites!
         choose-color
         )

(provide imager-register! on-tick-register! on-key-register! to-draw-register!)

(provide draw-world update-world-on-key update-world-on-tick)

(provide half)

;; *** What We Provide for Debugging!!

;; These provides should be commented out when no longer needed!!

(provide proxy-registry)

;; * What We Define

;; ** Miscellanea

;; Intended for pixel sizes.  Application should
;; try to make them even numbers!
(define (half n) (quotient n 2))

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

;; here's the structure type
(struct/contract
  registry ( [imager (listof (cons/c symbol? procedure?))]
             [image (listof (cons/c (or/c symbol? string?) image?))]
             [on-tick (listof (cons/c symbol? procedure?))]
             [on-key (listof (cons/c symbol? procedure?))]
             [to-draw (listof (cons/c symbol? procedure?))] )
  #:mutable #:transparent )

;; instantiate the registry structure object
(define proxy-registry (registry '() '() '() '() '()))

;; define the procedures to manage the association lists

(define-values (imager-register! imager-key->val imager-val->key)
  (obj-alist-procs proxy-registry registry-imager set-registry-imager!) )

(define-values (image-register! image-key->val image-val->key)
  (obj-alist-procs proxy-registry registry-image set-registry-image!) )

(define-values (on-tick-register! on-tick-proc on-tick-name)
  (obj-alist-procs proxy-registry registry-on-tick set-registry-on-tick!) )

(define-values (on-key-register! on-key-proc on-key-name)
  (obj-alist-procs proxy-registry registry-on-key set-registry-on-key!) )

(define-values (to-draw-register! to-draw-proc to-draw-name)
  (obj-alist-procs proxy-registry registry-to-draw set-registry-to-draw!) )

;; registry-image is holding two different things
;; - (string? key) = filesystem-path --> image
;; - (symbol? key) = drawing-function-name --> image
;; in both cases we recover proxy keys with image-val->key
;; when (string? key) it serves as a cache
;; QUESTION: What negative consequences follow this kludge?

;; EXERCISE: How might we cache procedural images?
;; HINTS:
;; - Let the procedures figure it out
;; - sprite->image could be a richer structure
;; QUESTION: Could a solution here clean up the kludge?

;; Given a path or symbol as a key find or
;; ( (load or create) and register) ) the appropriate image
;; or return #f if none of this is possible.
(define (get-image params key)
  (define this 'get-image)
  (let ( [image
          (cond ;; might it be a path to a bitmap file?
            [(string? key) (let ( [found (image-key->val key)] )
                             (if (image? found)
                                 found
                                 (let ( [new-image (bitmap/file key)] )
                                   (image-register! key new-image)
                                   new-image ) ) )]
            ;; might it be an image producing function?
            [(symbol? key) (let ( [imager (imager-key->val key)] )
                             (if (not (procedure? imager))
                                 (begin0 #f
                                         (eprintf "~a warning: unknown imager ~a\n"
                                                  this key ) )
                                 (let ( [new-image (imager params)] )
                                   (image-register! key new-image)
                                   new-image ) ) )]
            [else #f] ) ])
    (if (image? image) (begin0 image
                               (when (tracing this) (eprintf "~a ~a --> ~a\n"
                                                             this key image )) )
        (error this "no image for key ~a") ) ) )

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
;;     - world? vectors indexed by sprite ids
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
;; Returns the list of updated sprites --> who cares about this???
(define (update-sprites! actions universe)
  (define this 'update-sprites!)
  ;; return the sprites left after removing those matching the keys in drop-lists
  (let* ( [world-id (message-world actions)]
          [params (message-params actions)]
          [updates (actions-updates actions)]
          [world-sprites (universe-world universe world-id)] )
    (when (tracing this) (eprintf "~a params ~a updates ~a sprites ~a\n" this params updates world-sprites))
    (for-each (λ (update)
                (cond
                  [(sprite-id? update) ; drop this sprite
                   (when world-sprites ; in case the world has been dropped
                     (when (tracing this) (eprintf "~a dropping sprite ~a from world ~a\n"
                                                   this update world-id ))
                     (world-drop! world-sprites update) ) ]
                  [(sprite-proxy? update) ; create or update this sprite
                   (let ( [sprite-id (sprite-proxy-sprite update)] )
                     (when (not world-sprites) ; in case the world has been dropped
                       (when (tracing this) (eprintf "~a creating world ~a\n"
                                                     this world-id ))
                       ;; imperative mutation, because ... reasons!
                       (set! world-sprites (make-world sprite-id))
                       (universe-set! universe world-id world-sprites) )
                     (let ( [sprite (world-sprite world-sprites sprite-id)] )
                       (if sprite ; it already exists, so mutate it in place
                           (mutate-sprite-from-proxy! params sprite update)
                           ;; it doesn't exist, so create it
                           (let ( [new-sprite (proxy->sprite params update)] )
                             (when (tracing this) (eprintf "~a adding sprite ~a to world ~a at index ~a\n"
                                                           this new-sprite  world-id sprite-id ))
                             (world-set! world-sprites sprite-id new-sprite) ) ) ) ) ]
                  [else (error this "unknown update ~a" update)] ) )
              updates )
    (when (tracing this) (show-world world-id world-sprites)) ) )

;; ** Delegate big-bang Callback Procedures to Sprite Methods

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
        (let* ( [our-world-id (params-base-world our-params)]
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
            (let* ( [our-world-id (params-base-world our-params)]
                    [our-sprites (universe-world universe our-world-id)]
                    [actions (gather-actions-on-key our-params our-sprites key)] )
              (when (tracing this) (eprintf "~a actions: ~a\n" this actions))
              (if (not actions)
                  world-state ; no updates from this key
                  (begin
                    (update-sprites! actions universe) ; may invalidate our-sprites binding
                    (when (tracing this)
                      (eprintf "~a ours-updated ~a\n" this (universe-world universe our-world-id)))
                    (make-package world-state actions) ) ) ) ) ) ) )

(define (update-world-on-key world-state key)
  (define this 'update-world-on-key)
  (when (tracing this) (eprintf "~a world-state ~a key ~a\n" this world-state key))
  (let ( [result (do-update-world-on-key world-state key)] )
    (when (tracing this) (eprintf "~a returning ~a\n" this result))
    result ) )

;; *** Rendering (Drawing)

;; Like place-image, but relative to the left-bottom corner
;; of the sprite and the canvas.
(define (draw-image image x y canvas)
  (let ( [center-x (+ x (half (image-width image)))]
         [center-y (+ y (half (image-height image)))] )
    (place-image image center-x (- (image-height canvas) center-y) canvas) ) )

(define (draw-sprite sprite canvas)
  (draw-image (sprite-image sprite)
              (sprite-x sprite) (sprite-y sprite)
              canvas ) )
(to-draw-register! 'draw-sprite draw-sprite)


;; Use the drawing methods of all sprites in the universe
;; to compose their images onto a single canvas and return
;; the result.
;; EXERCISE: How could we ensure that our sprites are always drawn on top,
;; i.e. when our sprites overlap those from other worlds??
(define (draw-world world-state empty-canvas)
  (define this 'draw-world)
  (if (no-state-yet? world-state)
      empty-canvas
      ;; compose the worlds of our world state
      (sequence-fold
       (λ (c w)
         ;; compose the sprites of world w
         (sequence-fold (λ (c s) (let ( [drawing-method (sprite-to-draw s)] )
                                   (drawing-method s c) ))
                        c ; start with this canvas
                        ;; generate a sequence of all sprites in our world
                        (world-sprites w) ) )
       empty-canvas ; start with this canvas
       ;; generate a sequence of all worlds in our universe
       (universe-worlds (client-state-worlds-sprites world-state)) ) ) )


;; ** Testing

(provide show-world show-universe)

(require (only-in "sprites-framework.rkt"
                  world? world-sprite world-sprite-ids
                  universe? universe-world universe-world-ids ))

(define (show-world world-id world)
  (if (not (world? world))
      (eprintf "show-world: world = ~a\n" world)
      (stream-for-each
       (λ (i) (eprintf "world ~a sprite ~a = ~a\n" world-id i (world-sprite world i)))
       (world-sprite-ids world) ) ) )

(define (show-universe universe)
  (if (not (universe? universe))
      (eprintf "show-universe: universe = ~a\n" universe)
      (stream-for-each
       (λ (i) (show-world i (universe-world universe i)))
       (universe-world-ids universe) ) ) )
