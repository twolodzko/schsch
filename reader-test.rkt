#lang racket/base

(require "testing.rkt"
         "reader.rkt")

(begin
  (assert (not (whitespace? #\a)))
  (assert (whitespace? #\newline))
  (assert (whitespace? #\space)))

(begin
  (assert (not (word-end? #\a)))
  (assert (word-end? #\space))
  (assert (word-end? #\tab))
  (assert (word-end? #\())
  (assert (word-end? #\)))
  (assert (word-end? #\')))

(let ([r (new-reader (open-input-string "hello") '())])
  (assert-equal (pop r) #\h)
  (assert-equal (pop r) #\e)
  (assert-equal (pop r) #\l)
  (assert-equal (pop r) #\l)
  (assert-equal (pop r) #\o))

(let ([r (new-reader (open-input-string "123") #\X)])
  (assert-equal (pop r) #\X)
  (assert-equal (pop r) #\1)
  (assert-equal (pop r) #\2))

(let ([r (new-reader (open-input-string "") '())]) (assert-equal (pop r) eof))

(let ([r (new-reader (open-input-string "123") #\X)])
  (assert-equal (peek r) #\X)
  (assert-equal (peek r) #\X)
  (assert-equal (pop r) #\X)
  (assert-equal (peek r) #\1)
  (assert-equal (peek r) #\1)
  (assert-equal (pop r) #\1)
  (assert-equal (pop r) #\2)
  (assert-equal (pop r) #\3)
  (assert-equal (peek r) eof)
  (assert-equal (pop r) eof))

(begin
  (assert-equal (parse-string "abc") '#:abc)
  (assert-equal (parse-string "123") 123)
  (assert-equal (parse-string "3.14") 3.14)
  (assert-equal (parse-string "#t") #t)
  (assert-equal (parse-string "#f") #f)
  (assert-equal (parse-string "#true") '#:#true)
  (assert-equal (parse-string "  \n\n 123") 123)
  (assert-equal (parse-string "()") '())
  (assert-equal (parse-string "(1 2 3)") '(1 2 3))
  (assert-equal (parse-string "( 1 2 3)") '(1 2 3))
  (assert-equal (parse-string "(1 2 3 )") '(1 2 3))
  (assert-equal (parse-string "(1 ((#f) foo 3))") '(1 ((#f) #:foo 3)))
  (assert-equal (parse-string "'(1 2 3)") '(#:quote (1 2 3)))
  (assert-equal (parse-string "\"hello\"") "hello")
  (assert-equal (parse-string "\"William Joseph \\\"Wild Bill\\\" [1] Donovan\"")
                "William Joseph \"Wild Bill\" [1] Donovan")
  (assert-equal
   (parse-string
    ";; [1] a comment\n;[2] another one\n(list;[3] another comment\n;[4] next\n'first\n\n;[5] and next\n'second   ;; [6] comment\n);[7] final comment")
   '(#:list (#:quote #:first) (#:quote #:second))))
