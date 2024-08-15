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
         racket/set
         racket/function
         racket/sequence
         racket/contract/base
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
          [gvec? world-sprites?]
          [make-gvec make-world-sprites]
          [gvec-ref world-sprite]
          [gvec-ids world-sprite-ids]
          (gvec->sequence world-sprites)
          [gvec-set! world-sprite-set!]
          [gvec-drop! world-sprite-drop!] ))

;; universes map universe-ids to worlds
;; either iworld or world-sprites

(provide (rename-out
          [gvec? universe?]
          [make-gvec make-universe]
          [gvec-ref universe-world]
          [gvec-ids universe-world-ids]
          [gvec->sequence universe-worlds]
          [gvec-find-index universe-world-index]
          [gvec-set! universe-set!]
          [gvec-add! universe-add!]
          [gvec-drop! universe-drop!] ))

;; *** Sprite Proxies

;; Sprite Proxies represent new sprites or
;; changes to existing sprites.

;; Implementation below: ** Sprite Proxies

(provide (struct-out sprite-proxy))

;; *** Parameters, Tracing and Testing

;; Tracing controls the display of runtime information
;; to aid in understanding and debugging the programs.

;; Testing Mode has the program behave in a manner
;; which is simpler to study and debug.  E.g. the
;; sprites won't move without explicit boosting.

;; Implementation below: ** Tracing and Testing

(provide my-parameter tracing *tracing* *testing*)

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

;; ** Parameters, Tracing and Testing

;; fix the problems with the make-parameter guard
;; - ensure it is applied to the initial value
;; - ensure it returns the value it has checked
;; - allow value to initially be #f
(define (my-parameter value guard name [check-name #f])
  (define (string x) (format "~a" x))
  (define (check v)
    (unless (guard v)
      (raise-argument-error name
                            (string (or check-name (object-name guard) 'check))
                            v ) )
    v )
  (make-parameter (and value (check value)) check name) )

;; the parameter *tracing* can be set to
;; #f = don't trace anywhere
;; #t = trace everywhere
;; set of function names (symbols) = trace only those functions

;; ==> Use *tracing* with the function tracing!

;; (tracing) = return #t if *tracing* is #t, false otherwise
;; (tracing name) = return #t if *tracing* is #t
;;   or *tracing* is a set of function names including name

(define *tracing* (my-parameter #f (or/c boolean? generic-set?) 'tracing))

(define (tracing [name #f])
  (cond [(boolean? (*tracing*)) (*tracing*)]
        [(symbol? name) (set-member? name (*tracing*))]
        [else #f] ) )

;; In testing mode, disable any required user interactions
;; in particular, disable falling!
(define *testing* (my-parameter #f boolean? 'testing))

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

;; params could be a world-id? or a struct params
(struct message (params)
  #:constructor-name make-message
  #:prefab)

(define (message-world message)
  (define this 'message-world)
  (let ( [p (message-params message)] )
    (cond [(params? p) (params-world p)]
          [(world-id? p) p]
          [else (error this "invalid message params p")] ) ) )

;; server to new world
;; create with make-welcome function
(struct welcome-message message (alist) #:prefab)

;; world to world, relayed by server
(struct actions message (updates)
  #:constructor-name make-actions
  #:prefab )

;; world to server: goodbye, drop me please!
(struct goodbye-message message ()
  #:constructor-name make-goodbye
  #:prefab )

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
  (when (tracing this) (eprintf "~a world ~a alist ~a\n" this n alist))
  (welcome-message n alist) )

;; ** gvec maps ids to values just right

;; Given IDs which are small contiguous numbers
;; we can use gvector to manage collections of
;; - IDs mapped to Values, e.g.
;; - worldsprites: sprites indexed sprite-ids
;; - universes: world-sprites indexed by world-ids

;; We'll customize gvector to have exactly the
;; semantics we want and call the result gvec!

;; gvec ids will be automatically reused, smallest
;; first, so the underlying gvector won't grow
;; until it must.

;; Make a new gvec with a default initial capacity
;; or with just enough room to hold a given index.
(define make-gvec
  (case-lambda
   [() (make-gvector)]
   [(index) (make-gvector #:capacity (+ 1 index))] ) )

;; A gvec is just a gvector we manage a bit differently.
(define gvec? gvector?)

;; We can reference an element of a gvec by and index
;; to return the element there, or #f if no such
;; element or index exist.
(define (gvec-ref gv i) (gvector-ref gv i #f))

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

;; return the non-false ids in a gvec as a sequence or stream
(define (gvec-ids gv)
  (sequence-filter (λ (id) (gvector-ref gv id))
                   (in-range 0 (gvector-count gv)) ) )

;; return the non-false values in a gvec as a sequence or stream
(define (gvec->sequence gv)
  (sequence-map (λ (id) (gvector-ref gv id))
                (gvec-ids gv) ) )

;; Ensure that gv is big enough to have a slot with index i
(define (gvec-ensure-size gv i [val #f])
  (let ( [size-now (gvector-count gv)] )
       (when (>= i size-now)
         (let ( [more-needed (- i size-now -1)] )
         (apply (curry gvector-add! gv) (build-list more-needed (λ (_) #f))) ) ) ) )

;; Add item to gv at index, growing gv if necessary.
(define (gvec-set! gv index item)
  (define this 'gvec-set!)
  (when (not gv) (error this "set ~a ~a ~a" gv index item))
  (gvec-ensure-size gv index)
  (let ( [old-item (gvector-ref gv index)] )
    (unless (equal? old-item item) ; make set! idempotent
      (when old-item
        (error this "gvec[~a] is ~a, rejecting ~a" index old-item item))
      (gvector-set! gv index item) ) ) )

;; Add item to the first free slot in gv
;; returning the id of that slot.
(define (gvec-add! gv item)
  (let ( [index (gvec-first-free-index gv)] )
    (gvec-set! gv index item)
    index ) )

;; when gv exists, drop its element at index i
(define (gvec-drop! gv i)
  (when gv (gvector-set! gv i #f)) )

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
  #:constructor-name make-sprite-proxy
  #:prefab )

;; an update is either
;; a sprite-id indicating a sprite to be dropped
;; a sprite-proxy used to create or mutate a sprite
(define (update? u) (or (sprite-id? u) (sprite-proxy? u)))
