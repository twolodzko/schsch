#lang racket/base
(require "testing.rkt"
         "envir.rkt")

(begin
  (assert (not (envir? #t)))
  (assert (envir? (from-records '())))
  (assert (envir? (from-records '(#:x 1 #:y 2)))))

(begin
  (assert (envir? (from-envir (void))))
  (assert (envir? (from-envir (from-records '()))))
  (assert (envir? (from-envir (from-records '(#:x 1 #:y 2))))))

(let ([env (from-records '(#:x 1 #:y 2))])
  (assert-equal (envir-ref env '#:x) 1)
  (assert-equal (envir-ref env '#:y) 2)
  (envir-def env '#:z 3)
  (assert-equal (envir-ref env '#:z) 3))

(let* ([parent (from-records '(#:x 1 #:y 2))] [child (from-envir parent)])
  ;; set value in child
  (envir-def child '#:z 3)
  (assert-equal (envir-ref child '#:z) 3)
  (envir-set! child '#:z 4)
  (assert-equal (envir-ref child '#:z) 4)

  ;; set value in parent
  (assert (not (envir-has? child '#:x)))
  (envir-set! child '#:x 5)
  (assert (not (envir-has? child '#:x)))
  (assert-equal (envir-ref parent '#:x) 5))
