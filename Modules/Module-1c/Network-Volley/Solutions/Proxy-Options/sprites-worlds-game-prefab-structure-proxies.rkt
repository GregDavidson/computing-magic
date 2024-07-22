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
         racket/stream
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

;; indexes can be mapped to values with

(provide (rename-out
          (gvec? universe?)
          (make-gvec make-universe)
          (gvec-count universe-count)
          (gvec-ref universe-world)
          (gvec->stream universe-worlds)
          (gvec->list universe-world-list)
          (gvec-first-free-index universe-next-index)
          (gvec-find-index universe-world-index)
          (gvec-set! universe-set!)
          (gvec-drop! universe-drop!) ))

(provide (rename-out
          (gvec? world-sprites?)
          (make-gvec make-world-sprites)
          (gvec-count world-sprites-count)
          (gvec-ref world-sprite)
          (gvec->stream world-sprites)
          (gvec->list world-sprite-list)
          (gvec-first-free-index world-next-index)
          (gvec-find-index world-sprite-index)
          (gvec-set! world-sprite-set!)
          (gvec-drop! world-sprite-drop!) ))

;; *** Sprite Proxies

;; Implementation below: ** Sprite Proxies

(provide make-sprite-proxy
         (struct-out sprite-proxy) )

#;(provide
 (contract-out
  [make-sprite-proxy
   (-> world-id? sprite-id?
       (or/c #f string? symbol?) ; key image
       (or/c #f natural?)   (or/c #f natural?) ; x y
       (or/c #f integer?)   (or/c #f integer?) ; dx dy
       (or/c #f procedure?) (or/c #f procedure?) (or/c #f procedure?) ; methods
       sprite-proxy? ) ] )
 (struct-out sprite-proxy) )

;; *** Tracing and Testing

;; Implementation below: ** Tracing and Testing

(provide tracing *testing*)

;; *** Client-Server Message Types

(provide (struct-out message)
         (struct-out welcome-message)
         make-welcome
         (struct-out goodbye-message)
         (struct-out actions)
         update? )

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

(define tracing
  (let ( [this 'tracing] [setting (make-parameter #f)] )
    (λ args
      (if (null? args)
          (and (boolean? setting) setting) ; return #t iff setting is #t
          (let ( [mode (car args)] [names (cdr args)] )
            (if (null? names)
                (cond [(boolean? mode) (set! setting mode)] ; set setting to #t or #f
                      [(symbol? mode) (if (boolean? setting) setting (set-member? setting mode))]
                      [else (error this "unknown mode ~a" mode)] )
                (if (and (boolean? mode) (ormap symbol? names))
                    ;; ensure setting is a mutable set using comparison function eq?
                    (when (not (set-mutable? setting)) (set! setting (mutable-seteq)))
                    (let ( [update! (if mode set-add! set-remove!)] )
                      (for-each (λ (name) (update! setting name))
                                names ) ) ) ) ) ) ) ) )

;; In testing mode, disable any required user interactions
;; in particular, disable falling!
(define *testing* (make-parameter #f))

;; ** Client-Server message Types

(struct message (world) #:prefab)

;; server to new world
(struct welcome-message message (alist) #:prefab)

;; world to world, relayed by server
(struct actions message (params updates) #:prefab)

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
;; - worlds indexed by world-ids
;; - sprites indexed sprite-ids

;; gvec-nextid will keep the indexes small by
;; - starting with 0
;; - reusing ids when items are dropped

(define make-gvec make-gvector)

(define gvec? gvector?)

(define gvec-count gvector-count)

(define (gvec-ref gv i) (gvector-ref gv i #f))

;; return the non-false values in a gvec as a stream
(define gvec->stream (compose (curry stream-filter identity) in-gvector))

;; return the non-false values in a gvec as a list
(define gvec->list (compose gvec->stream stream->list))

;; Return the index of the first non-#f slot of
;; the gvec v which is #f, or if none, gvector-count
(define (gvec-first-free-index v)
  (let loop ( [i 0] [end (gvector-count v)] )
    (if (or (= i end) (not (gvector-ref v i)))
        i
        (loop (+ 1 i)) ) ) )
;; A faster algorithm is possible if
;; 1. The vector contents aren't numbers
;; 2. An extra slot is always available at the end
;; 3. The extra slots "point at each other" from lowest to highest, circularly

;; Return first index of item in gvec or #f if none.
(define (gvec-find-index gv item)
  (let loop ( [i 0] [end (gvector-count gv)] )
    (cond [(= i end) #f]
          [(not (equal? item (gvector-ref gv i))) i]
          [else (loop (+ 1 i))] ) ) )

;; Ensure that i is valid for a set!
(define (gvec-ensure-size gv i [val #f])
  (let ( [size-now (gvector-count gv)] )
       (when (>= i size-now)
         (let ( [more-needed (- size-now i -1)] )
         (apply gvector-add! (build-list more-needed (λ () #f))) ) ) ) )

(define (gvec-set! gv index item)
  (define this 'gvec-set!)
  (gvec-ensure-size)
  (let ( [old-item (gvector-ref gv index)] )
    (unless (equal? old-item item) ; make set! idempotent
      (when old-item
        (error this "gvec[~a] is ~a, rejecting ~a" index old-item item))
      (gvector-set! gv index item) ) ) )

(define (gvec-drop! gv i)
  (gvector-set! gv i #f) )

;; ** Sprite Proxies

;; We need to be able to send sprites across worlds.
;; This requires us to serialize them, i.e. convert them to a byte stream.
;; Alas, Racket doesn't provide for serialization of
;; - regular structures, images or procedures

;; A serializable-struct can be serialized if all of their components
;; can be serialized.  Alas, the 2htdp/universe package won't accept
;; serializable structures!  So let's create a prefab sprite-proxy!

;; Images will be represented either by
;; (1) a filesystem path to a stored image
;; (2) a symbol representing a function which
;;     takes a color and returns an image.
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
(define make-sprite-proxy raw-sprite-proxy)

(define (update? u) (or sprite-id? sprite-proxy?))
