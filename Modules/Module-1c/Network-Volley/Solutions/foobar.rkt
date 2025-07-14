#lang racket/base
(require (only-in "bar.rkt"
                   foo foo? make-foo foo-a ; re-exported from "foo.rkt"
                   bar bar? make-bar bar-b ; extending struct foo
                   bar-a
                   ))
