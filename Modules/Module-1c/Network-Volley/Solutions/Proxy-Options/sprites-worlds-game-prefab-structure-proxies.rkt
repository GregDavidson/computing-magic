#lang racket
;; * Multiple Worlds Sprites Game Protocol and Overview

;; See sprites-worlds-game.org for information about the game.

;; This file is required by the Universe Server
;; and each World Client.  It provides
;; - inter-client (inter-world) protocol information
;; - universe-world protocol information
;; - including a sprite-proxy structure

(require uuid) ; univerally unique identifiers

;; The definition of struct sprite-proxy is at the end of the file
(provide
 (contract-out [make-sprite-proxy
                (-> uuid-symbol? (or/c #f string? symbol?) ; uuid image
                    (or/c #f natural?)   (or/c #f natural?) ; x y
                    (or/c #f integer?)   (or/c #f integer?) ; dx dy
                    (or/c #f procedure?) (or/c #f procedure?) (or/c #f procedure?) ; methods
                    sprite-proxy? ) ])
 (struct-out sprite-proxy) )

(provide tracing *testing*)
(provide W2U-EMPTY W2U-DONE)
(provide U2W-WELCOME)
(provide message-head welcome? make-welcome welcome-alist welcome-world-number)

;; Messages from a World Client to the Universe Server

;; Does the server need to know this?  Maybe notify
;; other clients instead??
(define W2U-EMPTY 'empty)  ;; we've lost our sprites

(define W2U-DONE 'done)  ;; detach us!

;; Messages from the Universe Server to a World Client

;; Provide a natural? number to uniquely identify a world.
(define U2W-WELCOME 'welcome)
(define WORLD-NUMBER-KEY 'world-number)

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
       (let ( [wn (assoc WORLD-NUMBER-KEY (cdr message))] )
         (and wn (natural? (cadr wn))) )
       (symbol-key-alist? (cdr message))) )

;; Return a welcome message with a world number and
;; possibly additional values in an association list.
(define (make-welcome n . alist)
  (define this 'make-welcome)
  (unless (natural? n) (error this "invalid world-number ~a" n))
  (unless (symbol-key-alist? alist) (error this "invalid alist ~a" alist))
  (cons U2W-WELCOME (cons (list WORLD-NUMBER-KEY n) alist)) )

;; Return the association list from a welcome message.
(define (welcome-alist welcome)
  (define this 'welcome-alist)
  (unless (welcome? welcome) (error this "invalid welcome ~a" welcome))
  (cdr welcome) )

;; Return the World Number from a welcome message.
(define (welcome-world-number welcome)
  (define this 'welcome-world-number)
  (let ( [found (assoc WORLD-NUMBER-KEY (welcome-alist welcome))] )
    (unless (and (pair? found) (= (length found) 2))
      (error this "missing world number in ~a" welcome) )
    (let ( [number (second found)] )
      (unless (natural? number) (error  this "invalid world number ~a in ~a" number welcome))
      number ) ) )

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

;; ** struct sprite-proxy

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
;; A sprite-proxy will have the same uuid as the sprite it is a proxy for.
;; Only the uuid field is required.  The other fields can default to #f if
;; the corresponding sprite field is irrelevant, i.e. not requiring an update.
;; EXERCISE: prefab structures don't support guards or contracts:
;; --> How can we add contracts a different way??
(struct
 sprite-proxy (uuid image x y dx dy on-tick on-key to-draw)
  #:constructor-name raw-sprite-proxy
  #:prefab
  #;#:guard
  #;(struct-guard/c uuid-symbol?  (or/c #f string? symbol?)
                  (or/c #f natural?)   (or/c #f natural?)
                  (or/c #f integer?)   (or/c #f integer?)
                  (or/c #f procedure?) (or/c #f procedure?) (or/c #f procedure?) ) )

(define make-sprite-proxy raw-sprite-proxy)

;; ** Notes
