#lang racket
(require xml) ; for xexpr->string
(require web-server/servlet)
(require web-server/servlet-env)
(require rackunit)

(struct post (title body))

(define BLOG
  (list
   (post "First Post!" "Hey, this is my first post!")
   (post "No bills!" "Just car change!") ) )

(define (render-post apost)
  "What goes here?" )

#; (check-equal
    (render-post (post "First post!" "This is a first post."))
    '(div ((class "post")) "First post!" (p "This is a first post."))
    "render-post first post" )

(display (xexpr->string '(div ((class "post")) "First post!" (p "This is a first post."))))