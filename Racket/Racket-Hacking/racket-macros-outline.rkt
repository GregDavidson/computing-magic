#lang racket/base

;; https://www.gavinmcg.com/2016/02/03/racket-macros.html

(require (for-syntax racket/base))
(require racket/pretty)

(define-syntax-rule (show exp)
  (begin
    (printf "~a =\n" 'exp)
    (pretty-print exp) ) )

;; https://web.archive.org/web/20191108081750/http://www.willdonnelly.net/blog/scheme-syntax-rules/

(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
     (let loop ()
       (if condition
           (begin
             body ...
             (loop))
           #f)))))

(show (syntax->datum (expand-once #'(while (< x 5)
                                           (set! x (+ x 1))
                                           (printf "~a " x) ))))
(define x 0)
(while (< x 5)
  (set! x (+ x 1))
  (printf "~a " x))

(define-syntax for
  (syntax-rules (in as)
    ((for element in list body ...)
     (map (lambda (element)
            body ...)
          list))
    ((for list as element body ...)
     (for element in list body ...))))

(show (syntax->datum (expand-once #'(for '(0 1 2 3 4) as i
                                      (printf "~a " i)) )))
(show (syntax->datum (expand-once (expand-once #'(for '(0 1 2 3 4) as i
                                                   (printf "~a " i)) ))))
(show (syntax->datum (expand #'(for '(0 1 2 3 4) as i
                                 (printf "~a " i)) )))
(for '(0 1 2 3 4) as i
     (printf "~a " i))