#lang racket
(require pict)

(require racket/draw)
(define the-dc (new pdf-dc% [output "a-disk.pdf"]))

(define a-disk
  (disk 40
        #:color "Chartreuse"
        #:border-color "Medium Aquamarine"
        #:border-width 5) )

(send the-dc start-doc "Working on it...")
(send the-dc start-page)
(draw-pict the-picture the-dc 0 0)
(send the-dc end-page)
(send the-dc end-doc)
