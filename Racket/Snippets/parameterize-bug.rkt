#lang racket/base

(require 2htdp/image
         2htdp/universe
         racket/cmdline
         racket/contract/base
         racket/contract/region
         racket/sequence
         racket/list
         racket/set
         racket/math )

(define *user* (make-parameter #f (or/c symbol? string?) 'user))
(define *host* (make-parameter LOCALHOST string? 'host))
(define *tracing* (make-parameter #f (or/c boolean? generic-set?) 'tracing))
(define *testing* (make-parameter #f boolean? 'testing))

(define (local) (*host* LOCALHOST))
(define (ngender) (*host* "ngender.net"))
(define (testing) (*testing* #t) (*tracing* #t))

;; Prompt and then read an input line as a string
(define (get-string-line prompt)
  (eprintf "~a: " prompt)
  (read-line) )

(define (go1 #:user [user #f] #:host [host #f] #:trace [trace #f] #:test [test #f])
  (define this 'go1)
  (parameterize ( [*user* (or user (*user*) (get-string-line "User name"))]
                  [*host* (or host (*host*))]
                  [*tracing* (or trace (*tracing*))]
                  [*testing* (or test (*testing*))] )
    (eprintf "~a user ~a host ~a tracing ~a testing ~a\n"
             this (*user*) (*host*) (*tracing*) (*testing*) ) ) )

(define (go2 #:user [user #f] #:host [host #f] #:trace [trace #f] #:test [test #f])
  (define this 'go2)
  (let ( [u (or user (*user*) (get-string-line "User name"))]
         [h (or host (*host*))]
         [tra (or trace (*tracing*))]
         [tes (or test (*testing*))] )
    (eprintf "~a inside let user ~a host ~a tracing ~a testing ~a\n"
             this u h tra tes )
    (parameterize ( [*user* u]
                    [*host* h]
                    [*tracing* tra]
                    [*testing* tes] )
      (eprintf "~a inside parameterize user ~a host ~a tracing ~a testing ~a\n"
               this (*user*) (*host*) (*tracing*) (*testing*) ) ) ) )
