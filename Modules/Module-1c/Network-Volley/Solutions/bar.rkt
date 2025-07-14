#lang racket/base
(require (only-in "foo.rkt" foo foo? make-foo foo-a))

(provide
 foo foo? make-foo foo-a
 bar bar? make-bar bar-a bar-b )

(struct bar foo (b)
  #:constructor-name make-bar
  #:prefab )

(define bar-a foo-a)