;; -*- mode: racket; racket-repl-buffer-name: "*network-volley-game-repl*"; -*-
#lang racket/base
;; * Network Volley Multiple Worlds Multiple Sprites Game Client

;; See sprites-worlds-game.org for information about the game.

;; ** Requires and Provides

(require 2htdp/image
         2htdp/universe
         racket/sequence
         racket/contract/base
         racket/contract/region
         "sprites-worlds-client.rkt" )

;; for testing we provide
(provide PROXY-0
         move-sprite
         boost-sprite-on-key
         DY-FALLING
         decay )

;; ** Our World Parameters

;; Our World Parameters give context to
;; any procedures which are creating
;; foreign entities, i.e. sprites which
;; belong to other worlds

(struct params params-base (color falling)
  #:constructor-name make-params
  #:prefab )

(define params-world params-base-world)

;; ** Canvas Parameters and Rendering (Drawing)

(define CANVAS-WIDTH 400)               ; pixels
(define CANVAS-HEIGHT 300)              ; pixels
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; ** Balls

(define BALL-RADIUS 20)
(define BALL-MARGIN 1)
(define BALL-SIZE (* 2 (+ BALL-RADIUS BALL-MARGIN)))
(define BALL-TEXT-SIZE (half BALL-RADIUS))

(define DX-BOOST 6)
(define DY-BOOST 6)
(define DY-FALLING -4) ; disabled when *testing*

;; 10% of humans are color blind so don't just use color!
;; We're adding text.
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

;; ** Action Procedures and Functions

;; decay moves a value closer to its target value,
;; i.e. it makes boosts decay.
(define (decay value target [step 1])
  (cond [(> value target) (- value step)]
        [(< value target) (+ value step)]
        [else value] ) )

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
           (world-sprite-drop! universe (message-world mail)) ]
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

;; ** Run as Command or within REPL

; String -> WorldState
; create a world, hook it up to a server and start it running
(define (create-world a-name
                      [server LOCALHOST]
                      #:receive [receive receive]
                      #:tick [tick 1/30]
                      #:stop [stop? no-sprites-left?] )
  (define this 'create-world)
  (when (symbol? a-name) (set! a-name (symbol->string a-name)))
  (when (tracing this) (eprintf "~a ~a" this a-name))
  (big-bang
   no-state-yet
   [to-draw (λ (world) (draw-world world EMPTY-CANVAS))]
   [on-key update-world-on-key]
   [on-receive receive]
   [on-tick update-world-on-tick (if (*testing*) 1/10 tick)]
   [stop-when stop?]
   [name a-name]
   [register server] ) )

(define *user* (my-parameter #f (or/c symbol? string?) 'user))
(define *host* (my-parameter LOCALHOST string? 'host))

(define (local) (*host* LOCALHOST))
(define (ngender) (*host* "ngender.net"))

(define (go #:user [user #f] #:host [host #f] #:trace [trace #f] #:test [test #f])
  (define this 'go)
  (parameterize ( [*user* (or user (*user*) (get-string-line "User name"))]
                  [*host* (or host (*host*))]
                  [*tracing* (or trace (*tracing*))]
                  [*testing* (or test (*testing*))] )
    (eprintf "~a user ~a host ~a tracing ~a testing ~a\n"
             this (*user*) (*host*) (*tracing*) (*testing*) )
    (create-world (*user*) (*host*)) ) )

(if (program-is-standalone?)
    (begin
      (command-line
       #:once-each
       [("-t" "--tracing") "trace everywhere" (*tracing* #t)]
       [("-T" "--testing") "ease testing and trace everywhere"
                           (begin (*testing* #t) (*tracing* #t)) ]
       [("-H" "--host") host "server host" (*host* host)]
       [("-N" "--ngender") "host ngender.net" (ngender)]
       #:args (user . proc-names) (begin (*user* user) (trace-procs proc-names)) )
      (go) )
    ;; we should be at a REPL
    (let ( [yes-pattern (regexp "^ *[yY]")]
           [reply (get-string-line "run world client? [y/n]")] )
      (when (regexp-match yes-pattern reply)
        (parameterize ( [*tracing* #t] )
          (go) ) ) ) )
