#lang racket/base
(require racket/class)
(provide from-records
         from-envir
         envir?
         envir-ref
         envir-def
         envir-set!
         envir-has?)

(define (from-records records)
  (new envir% [data (apply hash records)] [parent (void)]))

(define (from-envir env)
  (new envir% [data #hash()] [parent env]))

(define (envir-ref env key)
  (send env @envir-ref key))

(define (envir-def env key val)
  (send env @envir-def key val))

(define (envir-set! env key val)
  (send env @envir-set! key val))

(define (envir-has? env key)
  (send env has? key))

(define (envir? obj)
  (is-a? obj envir%))

(define envir%
  (class object%
    (init-field data parent)
    (super-new)
    (define/public (has? key) (hash-has-key? data key))
    (define/public (@envir-ref key)
      (cond
        [(has? key) (hash-ref data key)]
        [(envir? parent) (envir-ref parent key)]
        [else (error (format "~s is not defined" key))]))
    (define/public (@envir-def key val) (set! data (hash-set data key val)))
    (define/public (@envir-set! key val)
      (cond
        [(has? key) (set! data (hash-set data key val))]
        [(envir? parent) (envir-set! parent key val)]
        [else (error (format "~s is not defined" key))]))))
