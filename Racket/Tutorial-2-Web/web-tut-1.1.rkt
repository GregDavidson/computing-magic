#lang web-server/insta
; Racket Tutorial 
; https://docs.racket-lang.org/continue/index.html
; Alessandro & Touch
; First explorations with Quasiquote variations

(define (start request)
  (render-blog-page BLOG request))

(define (render-blog-page a-blog request)
  (response/xexpr
   `(html (head (title "My Blog"))
          (body (h1 "My Blog")
                ,(render-posts a-blog)))))

;; Three versions of render-post

; render-post: post -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
#;(define (render-post a-post)
  `(div ((class "post"))
        ,(post-title a-post)
        (p ,(post-body a-post))))

#;(define (render-post a-post)
  (list 'div
        '((class "post"))
        (post-title a-post)
        (list 'p (post-body a-post)) ) )

(define (render-post a-post)
  (cons 'div
        (cons '((class "post"))
              (cons (post-title a-post)
                    (cons (cons 'p (cons (post-body a-post)'()))
                     '() ) ) ) ) )

;; Two versions of render-posts

; render-posts: blog -> xexpr
; Consumes a blog, produces an xexpr fragment
; of all its posts.
#;(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))

(define (render-posts a-blog)
  (cons 'div
        (cons '((class "posts"))
              (map render-post a-blog))))

; A blog is a (listof post)
; and a post is a (post title body)
(struct post (title body))
 
; BLOG: blog
; The static blog.
(define BLOG
  (list (post "Second Post" "This is a slight acheivment")
        (post "First Post" "This is hopefully working")
        (post "Self made post" "Why not")))
