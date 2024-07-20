#lang racket/base
;; * Multiple Worlds Sprites Game Protocol and Overview

;; See sprites-worlds-game.org for information about the game.

;; This file is required by the Universe Server
;; and each World Client.  It provides
;; - inter-client (inter-world) protocol information
;; - universe-world protocol information
;; - including a sprite-proxy structure

;; All of the (provide ...) forms are in the next section
;; Non-trivial implementations have their own sections following

(require racket/math
         racket/list
         racket/set
         racket/bool
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
(define world-id-tbd 666) ; to be determined, not the final value
(define world-id->string number->string)

(define sprite-id? natural?)

(provide world-id? sprite-id? world-id-tbd world-id->string)

;; indexes can be mapped to values with

(provide (rename-out
          (make-gvec make-universe)
          (gvec? universe?)
          (gvec-ref universe-world)
          (gvec->stream universe-worlds)
          (gvec->list universe-world-list)
          (gvec-next universe-next-index)
          (gvec-index universe-world-index)
          (gvec-set! universe-set!)
          (gvec-drop! universe-drop!) ))

(provide (rename-out
          (make-gvec make-world-sprites)
          (gvec? world-sprites?)
          (gvec-ref world-sprite)
          (gvec->stream world-sprites)
          (gvec->list world-sprite-list)
          (gvec-next world-next-index)
          (gvec-index world-sprite-index)
          (gvec-set! world-set!)
          (gvec-drop! world-drop!) ))

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

;; *** Universe Server <-> World Client

;; welcome message details below at: ** Universe Server <-> World Client

(provide W2U-EMPTY W2U-DONE)
(provide U2W-WELCOME)

;; Messages from a World Client to the Universe Server

;; Does the server need to know this?  Maybe notify
;; other clients instead??
(define W2U-EMPTY 'empty)  ;; we've lost our sprites

(define W2U-DONE 'done)  ;; detach us!

(provide message-head welcome? make-welcome welcome-alist welcome-world-id)

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

;; ** Universe Server <-> World Client

;; Provide a world-id? for a new world
(define U2W-WELCOME 'welcome)
(define WORLD-ID-KEY 'world-id)

;; and maybe some additional things as an association list

;; Is alist an association list whose keys are all symbols?
(define (symbol-key-alist? alist)
  (or (null? alist)
      (and (pair? alist)
           (pair? (car alist)) (symbol? (caar alist))
           (symbol-key-alist? (cdr alist)) ) ) )

;; What Action Symbol, if any, begins this message?
(define (message-head m)
  (if (pair? m) (car m) #f) )

;; Is this a welcome message?
(define (welcome? message)
  (and (pair? message)
       (eq? U2W-WELCOME (car message))
       (let ( [wn (assoc WORLD-ID-KEY (cdr message))] )
         (and wn (world-id? (cadr wn))) )
       (symbol-key-alist? (cdr message))) )

;; Return a welcome message with a world number and
;; possibly additional values in an association list.
(define (make-welcome n . alist)
  (define this 'make-welcome)
  (unless (world-id? n) (error this "invalid world-id ~a" n))
  (unless (symbol-key-alist? alist) (error this "invalid alist ~a" alist))
  (cons U2W-WELCOME (cons (list WORLD-ID-KEY n) alist)) )

;; Return the association list from a welcome message.
(define (welcome-alist welcome)
  (define this 'welcome-alist)
  (unless (welcome? welcome) (error this "invalid welcome ~a" welcome))
  (cdr welcome) )

;; Return the World Id from a welcome message.
(define (welcome-world-id welcome)
  (define this 'welcome-world-id)
  (let ( [found (assoc WORLD-ID-KEY (welcome-alist welcome))] )
    (unless (and (pair? found) (= (length found) 2))
      (error this "missing world id in ~a" welcome) )
    (let ( [id (second found)] )
      (unless (world-id? id) (error  this "invalid world id ~a in ~a" id welcome))
      id ) ) )

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

(define gvec-ref gvector-ref)

;; return the non-false values in a gvec as a stream
(define gvec->stream (compose (curry stream-filter identity) in-gvector))

;; return the non-false values in a gvec as a list
(define gvec->list (compose gvec->stream stream->list))

;; Return the index of the first non-#f slot of
;; the gvec v which is #f, or if none, gvector-count
(define (gvec-next v)
  (let loop ( [i 0] [end (gvector-count v)] )
    (if (or (= i end) (not (gvector-ref v i)))
        i
        (loop (+ 1 i)) ) ) )
;; A faster algorithm is possible if
;; 1. The vector contents aren't numbers
;; 2. An extra slot is always available at the end
;; 3. The extra slots "point at each other" from lowest to highest, circularly

;; Return first index of item in gvec or #f if none.
(define (gvec-index gv item)
  (let loop ( [i 0] [end (gvector-count gv)] )
    (cond [(= i end) #f]
          [(not (equal? item (gvector-ref gv i))) i]
          [else (loop (+ 1 i))] ) ) )

(define (gvec-set! gv index item)
  (define this 'gvec-set!)
  ;; ensure gv has enough slots
  (when (> index (gvector-count gv))
      (let loop ( [i (gvector-count gv)] )
        (unless (= i index)
          (gvector-add! gv #f)
          (loop (+ 1 i)) ) ) )
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
  sprite-proxy (world sprite image x y dx dy on-tick on-key to-draw)
  #:constructor-name raw-sprite-proxy
  #:prefab )

;; prefab structures don't support guards or contracts!
;; - we can associate a contract with make-sprite-proxy
(define make-sprite-proxy raw-sprite-proxy)
