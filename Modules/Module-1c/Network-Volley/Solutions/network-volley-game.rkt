;; (setq-local racket-repl-buffer-name "*network-volley-game-repl*")
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
   [to-draw draw-world]
   [on-key update-world-on-key]
   [on-receive receive]
   [on-tick update-world-on-tick (if (*testing*) 1/10 tick)]
   [stop-when stop?]
   [name a-name]
   [register server] ) )

;; ** Process Command Line or Enter REPL

(define *user* (my-parameter #f (or/c symbol? string?) 'user))
(define *host* (my-parameter LOCALHOST string? 'host))

(define (local) (*host* LOCALHOST))
(define (ngender) (*host* "ngender.net"))

(command-line
 #:once-each
 [("-t" "--tracing") "trace everywhere" (*tracing* #t)]
 [("-T" "--testing") "ease testing and trace everywhere"
                     (begin (*testing* #t) (*tracing* #t)) ]
 [("-H" "--host") host "server host" (*host* host)]
 [("-N" "--ngender") "host ngender.net" (*host* "ngender.net")]
 #:args (user . proc-names) (begin (*user user) (trace-procs proc-names)) )

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
    (go)
    (let ( [yes (regexp "[yY].*")]
           [reply (get-string-line "run world client? [y/n]")] )
      (when (regexp-match yes reply)
        (parameterize ( [*tracing* #t] )
          (go) ) ) ) )
