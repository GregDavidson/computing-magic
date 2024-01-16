#lang typed/racket

;; On Jan 14, 2024 at 8:33 PM Bill said

;; I can't get this to work:

#;(foldl filter (list 1 -2 3 4.5) (list positive? integer?))

;; I replied:

;; "It needs help inferring the type of filter?"
;; but I was away from my computer and falling asleep,
;; so I fumbled the question.

;; The next day, with my wits about me, I decided to think about
;; all of type types involved and how Typed Racket needed to
;; unify them in order to get things to work.  And how much of
;; that it could do without help.

;; Annoyingly, printing the types is only avalable at the
;; Typed Racket REPL, so I've pasted in the results of my queries:

#; foldl
#; (All (a b c d)
        (case->
         (-> (-> a b b) b (Listof a) b)
         (-> (-> a b c c) c (Listof a) (Listof b) c)
         (-> (-> a b c d d) d (Listof a) (Listof b) (Listof c) d)))

#; filter
#; (All (a b)
        (case->
         (-> (-> a Any : #:+ b) (Listof a) (Listof b))
         (-> (-> a Any) (Listof a) (Listof a))))

#; (list 1 -2 3 4.5)
#; (List One Negative-Fixnum Positive-Byte Positive-Float-No-NaN)

#; (list positive? integer?)
#; (List (-> Real Boolean) (-> Any Boolean))

;; Let's see if Typed Racket will agree with
;; what we'd like it to infer

(let ( [f : (-> (-> Real Boolean) (Listof Real) (Listof Real)) filter]
       [l1 : (Listof Real) (list 1 -2 3 4.5)]
       [l2 : (Listof (-> Real Boolean)) (list positive? integer?)] )
  (foldl f l1 l2) )

;; Yes, so can it infer the pure data parts:

(let ( [f : (-> (-> Real Boolean) (Listof Real) (Listof Real)) filter] )
  (foldl f (list 1 -2 3 4.5) (list positive? integer?)) )

;; Yes, so we can simply write:

(foldl (ann filter (-> (-> Real Boolean) (Listof Real) (Listof Real)))
       (list 1 -2 3 4.5)
       (list positive? integer?) )

;; Until Typed Racket does a better job of inferring types,
;; it's not a good tool for unsupported learners!