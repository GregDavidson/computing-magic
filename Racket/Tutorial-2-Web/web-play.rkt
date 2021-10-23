#lang racket
(require xml) ; for xexpr functions
(require web-server/servlet)
(require web-server/servlet-env)
(require rackunit) ; for unit testing

(struct post (title body))
; creates four functional procedures:
; (: post (-> string? string? post?)) ; constructor
; (: post? (or/c (-> post? #true) (-> ((not/c post?) #false)))
; (: post-title (-> post? string?)) ; selector: returns the title field
; (: post-body (-> post? string?))  ; selector: returns the body field

(define test-post (post "First Post!" "Hey, this is my first post!"))
(define test-post-as-xml-string '(div ((class "post")) "First post!" (p "This is a first post.")))
(display (xexpr->string test-post-as-xml-string))

(define BLOG
  (list
   test-post
   (post "No bills!" "Just car change!") ) )

(define (render-post apost)
  "What goes here?" )

#; (check-equal? (render-post test-post) test-post-as-xml-string "render-post test-post")

(define (nice-xexpr-string an-xexpr [indent #t])
  (display-xml/content (xexpr->xml an-xexpr) #:indentation (if indent 'classic 'none)) )

(define (display-xexpr an-xexpr [indent #t])
  (display (nice-xexpr-string an-xexpr indent)) )

; (display-xexpr test-post-as-xml-string)