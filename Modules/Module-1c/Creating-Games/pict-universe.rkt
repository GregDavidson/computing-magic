#lang racket

;; Is it possible to use create Racket 2D games using
;; pict instead of 2htdp/image ?
;; - How can we get bigbang from (require 2htdp/universe)
;;   to play nicely with pict?
;; - It could also be possible to use BOTH pict and 2htdp image packages.

;; * Package Requires

(require pict)
(require 2htdp/universe)
(require rackunit) ; for checks
