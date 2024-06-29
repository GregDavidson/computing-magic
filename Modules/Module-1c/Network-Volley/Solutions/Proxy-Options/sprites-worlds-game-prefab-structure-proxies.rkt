#lang racket
;; * Multiple Worlds Sprites Game Protocol and Overview

;; see sprites-worlds-game.org for information about the game

;; This file provides both the Universe Server and the World Clients with the
;; protocol which connects them.

(require uuid) ; univerally unique identifiers

;; The definition of struct sprite-proxy is at the end of the file
;; after a lot of comments!
(provide (struct-out sprite-proxy))
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
  (unless (natural? n) (error "invalid world-number ~a" n))
  (unless (symbol-key-alist? alist) (error "invalid alist ~a" alist))
  (cons U2W-WELCOME (cons (list WORLD-NUMBER-KEY n) alist)) )

;; Return the association list from a welcome message.
(define (welcome-alist welcome)
  (unless (welcome? welcome) (error "invalid welcome ~a" welcome))
  (cdr welcome) )

;; Return the World Number from a welcome message.
(define (welcome-world-number welcome)
  (let ( [found (assoc WORLD-NUMBER-KEY (welcome-alist welcome))] )
    (unless (and (pair? found) (= (length found) 2)) (error "missing world number in welcome ~a" welcome))
    (let ( [number (second found)] )
      (unless (natural? number) (error "invalid world number ~a in ~a" number welcome))
      number ) ) )

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
  #:prefab
  #;#:guard
  #;(struct-guard/c uuid-symbol?  (or/c #f string? symbol?)
                  (or/c #f natural?)   (or/c #f natural?)
                  (or/c #f integer?)   (or/c #f integer?)
                  (or/c #f procedure?) (or/c #f procedure?) (or/c #f procedure?) ) )

;; ** Notes
