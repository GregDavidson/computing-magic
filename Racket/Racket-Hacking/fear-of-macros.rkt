#lang racket/base

;; https://www.greghendershott.com/fear-of-macros/Transform_.html

;; One source says this is the default.
;; Another says that it's only the default in #lang/racket
(require (for-syntax racket/base))

(define-syntax (show-yourself stx)
  (printf "show-yourself syntax: ~a\n" stx)
  (printf "show-yourself syntax->datum: ~a\n" (syntax->datum stx))
  (datum->syntax stx (list 'quote (list 'Got (syntax->datum stx)))) )

(show-yourself to the world)

(define-syntax-rule (show exp)
  (printf "~a = ~a\n" 'exp exp) )

(newline)
(expand-once #'(show (+ 2 2)))
(show #'(+ 2 2))
(show (+ 2 2))

(define-syntax foo1
  (λ (stx) (syntax "I am foo1!")) )

(define-syntax (foo2 stx)
  (syntax "I am foo1!") )

(newline)
(show #'(foo))

(define-syntax (show-me stx)
       (println  stx)
     #'(void))

(show (show-me '(+ 1 2)))

(define stx #'(if x (list "true") #f))
(show stx)
(show  (syntax-source stx))
(show (syntax-line stx))
(show (syntax-column stx))
(show (syntax->datum stx))
(show (syntax-e #'x))
(show (syntax-e stx))
(show (syntax->list stx))

(define-syntax (reverse-me stx)
    (datum->syntax stx (reverse (cdr (syntax->datum stx)))))
(show (reverse-me "backwards" "am" "i" list))
(reverse-me "backwards" "am" "i" values)

(require (for-syntax racket/match))
(define-syntax (our-if stx)
    (match (syntax->list stx)
      [(list _ condition true-expr false-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]
                                 [else ,false-expr]))]))
(show (syntax->datum (expand-once #'(our-if #t "true" "false"))))
(show (our-if #t "true" "false"))

;; see
;; begin-for-syntax
;; define-for-syntax