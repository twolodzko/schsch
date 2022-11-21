#lang racket/base
(require "testing.rkt"
         "eval.rkt"
         "envir.rkt"
         "reader.rkt")

(define empty-env (from-records '()))

(begin
  (assert-equal (eval-string "42" empty-env) 42)
  (assert-equal (eval-string "3.14" empty-env) 3.14)
  (assert-equal (eval-string "#t" empty-env) #t)
  (assert-equal (eval-string "#f" empty-env) #f)
  (assert-equal (eval-string "x" (from-records '(#:x 5))) 5))
