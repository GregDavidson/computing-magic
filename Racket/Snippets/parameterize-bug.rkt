#lang racket/base

(define *host* (make-parameter "ngender.net" string? 'host))

(parameterize ( [*host* "hello"] )
  (define h (*host*))
  (eprintf "h ~a\n" h)
  (eprintf "host ~a\n" (*host*)) )

;; prints: h #t
;; prints: host #t
;; racket version: 8.13 [cs]
