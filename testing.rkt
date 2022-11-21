#lang racket/base
(provide assert
         assert-equal)

(define-syntax assert
  (syntax-rules ()
    [(_ stm)
     (cond
       [stm (void)]
       [else (error (format "assertion failed: ~s~n" (quote stm)))])]))

(define-syntax assert-equal
  (syntax-rules ()
    [(_ stm val)
     (let ([result stm])
       (cond
         [(equal? result val) (void)]
         [else (error (format "for ~s expected: ~s got: ~s~n" (quote stm) val result))]))]))
