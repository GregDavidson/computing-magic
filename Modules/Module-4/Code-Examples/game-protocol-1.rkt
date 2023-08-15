#lang racket


(provide send-item send-number send-string
         recv-msg-hdr recv-tag recv-number recv-string)

;; Special Case: For type 'string (and maybe other types later)
;; - the item will be the string length
;; - the actual string will follow on the next line
;;   as that many characters plus a newline.
(define (send-item out tag [type 'tag] [item #f])
  (fprintf out "~s ~s" type tag)
  (when item (fprintf out " ~s" item))
  (newline out) )

(define (send-number out tag num)
  (send-item out tag 'number num) )

(define (send-string out tag str)
  (send-item out tag 'string (string-length str))
  (write-string str out)
  (newline out) )

;; bytes should either be #f or a utf encoded byte string
;; We don't know how to check if a byte string is utf encoded!!
;; Return either #f or a string converted from the byte string.
(define (maybe-utf-bytes->string bytes)
  (if (not (bytes? bytes)) #f (bytes->string/utf-8 bytes)) )

;; bytes should either be #f or a utf encoded byte string
;; We don't know how to check if a byte string is utf encoded!!
;; Return either #f or a symbol converted from the byte string.
(define (maybe-utf-bytes->symbol bytes)
  (if (not (bytes? bytes)) #f (string->symbol (bytes->string/utf-8 bytes))) )

;; We could put the three components of a message in a list or vector
;; but structure types are made for this kind of thing!
;; See https://docs.racket-lang.org/guide/define-struct.html

(struct msg (type tag item))

;; TODO THE RECEIVING IS STILL WONKY, ESPECIALLY WITH STRINGS
;; GET IT CLEAN!!!
;; It might be OK now, but:
;; TODO THE TEST CODE IS NOW BROKEN!

;; Receive a message or (if the type is 'string) a message header.
;; If the latter, the item field is only the length of the string
;; which is yet to be read!
;; We don't raise errors yet, as they'll be more meaningful when
;; we can report exactly what we expected!
(define (recv-msg-hdr in)
  (let ( [match (regexp-match #rx"^([^ ]+) ([^ ]+)( ([^ ]+))?\n" in)] )
    (if (or (not (pair? match)) (not (= 5 (length match))))
        (msg #f #f #f)
        (msg (maybe-utf-bytes->symbol (list-ref match 1))
             (maybe-utf-bytes->symbol (list-ref match 2))
             (maybe-utf-bytes->string (list-ref match 4)) ) ) ) )

;; used with tag or type symbols
;; got could be #f so we use 'or'
(define (expect expected got)
  (when (not (eq? expected (or got "nothing")))
    (error (format "expected ~a got ~a" expected got)) ) )

(define (recv-msg-type-tag type tag in)
  (let ( [msg (recv-msg-hdr in)] )
    (expect type (msg-type msg))
    (expect tag (msg-tag msg))
    msg ) )

;; expect a number with the expected tag
;; return the number or raise an error
(define (recv-number tag in)
  (let ( [msg (recv-msg-type-tag 'number tag in)] )
    (string->number (msg-item msg)) ) )
 
;; expect a string with the expected tag
;; return the string or raise an error
(define (recv-string tag in)
  (let ( [msg (recv-msg-type-tag 'string tag in)] )
    (let ( [size (string->number (msg-item msg))] )
      (if (not (and (eq? 'string type) (integer? size) (>= size 0)))
          (error "bad string size")
          (read-string size in) ) ) ) )

(define (send-item-to-str tag [type 'tag] [item #f])
  (let ( [out (open-output-string)] )
    (send-item out tag type item)
    (get-output-string out) ) )

(define (test-tag tag [type 'tag] [item #f])
  (let ( [str (send-item-to-str tag type item)] )
    (recv-msg-hdr (open-input-string str)) ) )

(define (send-number-to-str tag num)
  (let ( [out (open-output-string)] )
    (send-number out tag num)
    (get-output-string out) ) )

(define (test-number tag num)
  (let* ( [str (send-number-to-str tag num)] )
    (recv-number (open-input-string str)) ) )

(define (send-string-to-str tag str)
  (let ( [out (open-output-string)] )
    (send-string out tag str)
    (get-output-string out) ) )

(define (test-string tag str)
  (let* ( [str (send-string-to-str tag str)] )
    (recv-string (open-input-string str)) ) )
