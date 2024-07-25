;; (setq-local racket-repl-buffer-name "*sprites-worlds-game-repl*")
#lang racket/base
;; * Multiple Worlds Sprites Game Protocol and Overview

;; See sprites-worlds-game.org for information about the game.

;; This file is required by the Universe Server
;; and each World Client.  It provides
;; - inter-client (inter-world) protocol information
;; - universe-world protocol information
;; - including a sprite-proxy structure

;; The (provide ...) forms follow the Require Forms section.
;; Non-trivial implementations have their own sections following

;; ** Require Forms


(require racket/math
         racket/list
         racket/set
         #; racket/bool
         racket/function
         racket/sequence
         data/gvector )

;; ** Provide Forms and Trivial Implementations

;; *** IDs and ID Maps

;; Details below at ** IDs and ID Maps

;; Unique Keys associate Sprite Proxies with Sprites
;; Because Sprites are associated with a specific World
;; - the keys can be a combination of a world-id and a sprite-id

(define world-id? natural?)
(define world-id->string number->string)

(define sprite-id? natural?)

(provide world-id? sprite-id? world-id->string)

;; world-sprites map sprite-ids to sprites

(provide (rename-out
          (gvec? world-sprites?)
          (make-gvec make-world-sprites)
          (gvec-count world-sprites-count)
          (gvec-ref world-sprite)
          (gvec->sequence world-sprites)
          (gvec->list world-sprite-list)
          (gvec-find-index world-sprite-index)
          (gvec-set! world-sprite-set!)
          (gvec-drop! world-sprite-drop!) ))

;; universes map universe-ids to world-sprites

(provide (rename-out
          (gvec? universe?)
          (make-gvec make-universe)
          (gvec-count universe-count)
          (gvec-ref universe-world)
          (gvec->sequence universe-worlds)
          (gvec->list universe-world-list)
          (gvec-find-index universe-world-index)
          (gvec-set! universe-set!)
          (gvec-add! universe-add!)
          (gvec-drop! universe-drop!) ))

;; *** Sprite Proxies

;; Sprite Proxies represent new sprites or
;; changes to existing sprites.

;; Implementation below: ** Sprite Proxies

(provide make-sprite-proxy
         (struct-out sprite-proxy) )

;; *** Tracing and Testing

;; Tracing controls the display of runtime information
;; to aid in understanding and debugging the programs.

;; Testing Mode has the program behave in a manner
;; which is simpler to study and debug.  E.g. the
;; sprites won't move without explicit boosting.

;; Implementation below: ** Tracing and Testing

(provide tracing *testing*)

;; *** Client-Server Message Types

;; a param structure provides context needed to
;; run functions to create or update foreign sprites.

;; messages carry information between a
;; world and a universe server or between
;; worlds when relayed by a universe server

;; an update can describe a sprite to drop
;; a sprite to create or a sprite to change

;; an action structure associates a context,
;; i.e. a world-id or a params structure
;; with a list of updates.

(provide
 (struct-out params)
 (struct-out message)
 message-world
 (struct-out welcome-message)
 make-welcome
 (struct-out goodbye-message)
 update?
 (struct-out actions) )

;; ** Tracing and Testing

;; Tracing can be set to
;; #f = don't trace anywhere
;; #t = trace everywhere
;; set of function names = trace only those functions

;; (tracing) = return #t if tracing is #t, false otherwise
;; (tracing name) = return #t if name is in set of functions
;; (tracing #t) = enable tracing everywhere
;; (tracing #f) = disable tracing
;; (tracing #t name ...) = enable tracing for specified names
;; (tracing #f name ...) = disable tracing for specified names

;; TODO: This is too complex!
;; Either redesign it and/or
;; (1) provide a thorough contract
;; (2) provide thorough testing
(define tracing
  (let ( [this 'tracing] [*setting* (make-parameter #f)] )
    (λ args
      (let ( [setting (*setting*)] )
        (if (null? args)
            (and (boolean? setting) setting) ; return #t iff setting is #t
            (let ( [mode (car args)] [names (cdr args)] )
              (if (null? names)
                  (cond [(boolean? mode) (*setting* mode)] ; set setting to #t or #f
                        [(symbol? mode) (if (boolean? setting) setting (set-member? setting mode))]
                        [else (error this "unknown mode ~a" mode)] )
                  (when (and (boolean? mode) (ormap symbol? names))
                      ;; ensure setting is a mutable set using comparison function eq?
                      (when (not (set-mutable? setting)) (set! setting (mutable-seteq)))
                      (let ( [update! (if mode set-add! set-remove!)] )
                        (for-each (λ (name) (update! setting name))
                                  names ) )
                      (*setting* setting) ) ) ) ) ) ) ) )

;; In testing mode, disable any required user interactions
;; in particular, disable falling!
(define *testing* (make-parameter #f))

;; ** Game Parameters

;; A structure type for World Parameters needed by
;; functions passed by name in our proxy structures.
;; It's Universe serializable because
;; - it's a #:prefab structure
;; - it's fields are Universe serializable
(struct params ( world color falling )
  #:constructor-name make-params
  #:prefab )

;; Consider adding a hash list to make
;; params more open ended.

;; ** Client-Server message Types

(struct message (params) #:prefab)

(define (message-world message)
  (define this 'message-world)
  (let ( [p (message-params message)] )
    (cond [(params? p) (params-world p)]
          [(world-id? p) p]
          [else (error this "invalid message params p")] ) ) )

;; server to new world
(struct welcome-message message (alist) #:prefab)

;; world to world, relayed by server
(struct actions message (updates) #:prefab)

;; world to server: goodbye, drop me please!
(struct goodbye-message message ())

;; and maybe some additional things as an association list

;; Is alist an association list whose keys are all symbols?
(define (symbol-key-alist? alist)
  (and (list? alist) (andmap (compose symbol? car) alist)) )

;; Return a welcome message with a world number and
;; possibly additional values in an association list.
(define (make-welcome n . alist)
  (define this 'make-welcome)
  (unless (world-id? n) (error this "invalid world-id ~a" n))
  (unless (symbol-key-alist? alist) (error this "invalid alist ~a" alist))
  (welcome-message n alist) )

;; ** IDs and ID Maps

;; Given IDs which are small contiguous numbers
;; we can use gvectors to manage collections of
;; - IDs mapped to Values, e.g.
;; - worldsprites: sprites indexed sprite-ids
;; - universes: world-sprites indexed by world-ids

;; gvec-nextid will keep the indexes small by
;; - starting with 0
;; - reusing ids when items are dropped

(define make-gvec
  (case-lambda
   [() (make-gvector)]
   [(index) (make-gvector #:capacity (+ 1 index))] ) )

(define gvec? gvector?)

(define gvec-count gvector-count)

(define (gvec-ref gv i) (gvector-ref gv i #f))

;; return the non-false values in a gvec as a stream
(define gvec->sequence (compose (curry sequence-filter identity) in-gvector))

;; return the non-false values in a gvec as a list
(define gvec->list (compose gvec->sequence sequence->list))

;; Return the index of the first non-#f slot of
;; the gvec v which is #f, or if none, gvector-count
(define (gvec-first-free-index gv)
  (let (  [end (gvector-count gv)] )
    (let loop ( [i 0] )
      (if (or (= i end) (not (gvector-ref gv i)))
          i
          (loop (+ 1 i)) ) ) ) )

;; EXERCISE: Replace this O(n) algorithm with an O(1) algorithm

;; Clue: An O(1) algorithm is straightforward if
;; 1. The vector contents aren't numbers
;; 2. An extra slot is always available at the end
;; 3. The extra slots "point at each other" from lowest to highest, circularly
;; Examples:
;; an empty gvec: (gvector 0)
;; a gvec with 2 slots available for reuse:
;;   (gvector 'item0 3 'item2 5 'item4 1)
;; the end -> position 1 -> position 3 --> the end
;; pronouncing -> as "points to"

;; Return first index of item in gvec or #f if none.
(define (gvec-find-index gv item)
    (let ( [end (gvector-count gv)] )
      (let loop ( [i 0] )
        (cond [(= i end) #f]
              [(equal? item (gvector-ref gv i)) i]
              [else (loop (+ 1 i))] ) ) ) )

;; Ensure that i is valid for a set!
(define (gvec-ensure-size gv i [val #f])
  (let ( [size-now (gvector-count gv)] )
       (when (>= i size-now)
         (let ( [more-needed (- i size-now -1)] )
         (apply (curry gvector-add! gv) (build-list more-needed (λ (_) #f))) ) ) ) )

(define (gvec-set! gv index item)
  (define this 'gvec-set!)
  (gvec-ensure-size gv index)
  (let ( [old-item (gvector-ref gv index)] )
    (unless (equal? old-item item) ; make set! idempotent
      (when old-item
        (error this "gvec[~a] is ~a, rejecting ~a" index old-item item))
      (gvector-set! gv index item) ) ) )

(define (gvec-add! gv item)
  (let ( [index (gvec-first-free-index gv)] )
    (gvec-set! gv index item)
    index ) )
  
(define (gvec-drop! gv i)
  (gvector-set! gv i #f) )

;; ** Sprite Proxies

;; We need to be able to send sprites across worlds.
;; This requires us to serialize them, i.e. convert them to a byte stream.
;; Alas, Racket doesn't provide for serialization of
;; - regular structures, images or procedures

;; A serializable-struct can be serialized if all of their components
;; can be serialized.  Alas, the 2htdp/universe package won't accept
;; serializable structures!  It will accept lists, vectors and
;; prefab structures.

;; We can represent images either by
;; (1) a filesystem path to a stored image
;; (2) a symbol representing a function which
;;     takes a params structure and returns an image.
;; Procedures will be represented by their names (symbols).
;; A sprite-proxy will have the same key as the sprite it is a proxy for.
;; Only the key field is required.  The other fields can default to #f if
;; the corresponding sprite field is irrelevant, i.e. not requiring an update.
(struct
  sprite-proxy (sprite image x y dx dy on-tick on-key to-draw)
  #:constructor-name raw-sprite-proxy
  #:prefab )

;; prefab structures don't support guards or contracts!
;; - we can associate a contract with make-sprite-proxy
;;   and then give it a contract when it is imported.
(define make-sprite-proxy raw-sprite-proxy)

;; an update is either
;; a sprite-id indicating a sprite to be dropped
;; a sprite-proxy used to create or mutate a sprite
(define (update? u) (or (sprite-id? u) (sprite-proxy? u)))