;; * aif macro

;; https://lambdaland.org/posts/2024-11-21_powerful_or_safe_languages/

#lang racket
(require racket/stxparam syntax/parse/define)

(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error (syntax-e stx)
                        "can only be used inside aif")))

(define-syntax (aif stx)
  (syntax-parse stx
    [(_ test tcase fcase)
     #'(let ([tmp test])
         (if tmp
             (syntax-parameterize
                 ([it (make-rename-transformer #'tmp)])
               tcase)
             fcase))]))

(aif 41 (+ it 1) 'whatever) ;=> 42
#;it                          ;error: it: can only be used inside aif

(aif (+ 2 2) (printf "it is ~a\n" it) (printf "not it!\n") )
