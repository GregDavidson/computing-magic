#lang racket/base

;; https://www.gavinmcg.com/2016/02/03/racket-macros.html

(require (for-syntax racket/base  ; this is supposedly the default
                     ))
(require racket/pretty)

(define-syntax-rule (show exp)
  (begin
    (printf "~a ⇒\n" 'exp)
    (pretty-display exp) ) )

(define-syntax-rule (sho exp)
  (let ( [code (expand-once #'exp)] )
    (begin
      (printf "~a ⇒\n" #'exp)`
      (pretty-display code)
      code ) ) )

;; https://web.archive.org/web/20191108081750/http://www.willdonnelly.net/blog/scheme-syntax-rules/

;; equivalent simpler version follows
#;(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
     (let loop ()
       (if condition
           (begin
             body ...
             (loop))
           #f ) ) ) ) )

(define-syntax-rule (while condition body ...)
  (let loop ()
    (if condition
        (begin
          body ...
          (loop))
        #f ) ) )


(newline)
(displayln "show sho:")
(show (syntax->datum (expand-once #'(sho (syntax->datum #'(while (< x 5)
                                                                 (set! x (+ x 1))
                                                                 (printf "~a " x) ))))))

(newline)
(displayln "sho:")
(sho (syntax->datum #'(while (< x 5)
                             (set! x (+ x 1))
                             (printf "~a " x) )))

(newline)
(displayln "show:")
(show (syntax->datum (expand-once #'(while (< x 5)
                                           (set! x (+ x 1))
                                           (printf "~a " x) ))))

(newline)
(displayln "Count with while:")
(define x 0)
(while (< x 5)
  (set! x (+ x 1))
  (printf "~a " x) )

(define-syntax for
  (syntax-rules (in as)
    ((for element in list body ...)
     (map (lambda (element)
            body ...)
          list ) )
    ((for list as element body ...)
     (for element in list body ...)) ) )

(newline)
(show (syntax->datum (expand-once #'(for '(0 1 2 3 4) as i
                                      (printf "~a " i)) )))
(newline)
(show (syntax->datum (expand-once (expand-once #'(for '(0 1 2 3 4) as i
                                                   (printf "~a " i)) ))))
(newline)
(show (syntax->datum (expand #'(for '(0 1 2 3 4) as i
                                 (printf "~a " i)) )))

(newline)
(displayln "Count with for:")
(for '(0 1 2 3 4) as i
     (printf "~a " i))