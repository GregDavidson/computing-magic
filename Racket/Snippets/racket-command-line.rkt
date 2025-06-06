#lang racket

(define program-name (find-system-path 'run-file))

(define *tracing* (make-parameter #f boolean? 'tracing))

(define args
  (command-line
   ;; #:program program-name ; the default
   ;; #:once-each ; set these at most once
   ;; #:multi ; set these 0 or more times
   #:final ; last possible option(s)
   [("-t" "--tracing") "trace things"
                       (*tracing* #t)]
   #:args args args )) ; return additional arguments

(eprintf "~a tracing ~a\n" program-name (*tracing*))
(eprintf "~a arguments ~a\n" program-name args)
