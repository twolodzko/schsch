#lang racket/base
(provide scheme
         last
         symbol->keyword)
(require "eval.rkt"
         "envir.rkt"
         "printer.rkt")

(define (symbol->keyword k)
  (string->keyword (symbol->string k)))

(define (last lst)
  (cond
    [(null? lst) (void)]
    [(null? (cdr lst)) (car lst)]
    [else (last (cdr lst))]))

(define-syntax decorate
  (syntax-rules ()
    [(_ f) (list (symbol->keyword (quote f)) (lambda (args env) (apply f (eval-all args env))))]
    [(_ f1 f2 ...) (append (decorate f1) (decorate f2 ...))]))

(define (_and lst env)
  (cond
    [(null? lst) #t]
    [else (let ([this (car lst)] [rest (cdr lst)]) (if (eval-sexpr this env) (_and rest env) #f))]))

(define (_or lst env)
  (cond
    [(null? lst) #f]
    [else (let ([this (car lst)] [rest (cdr lst)]) (if (eval-sexpr this env) #t (_or rest env)))]))

(define (_if args env)
  (let ([condition (car args)] [ifTrue (cadr args)] [ifFalse (caddr args)])
    (if (eval-sexpr condition env) (eval-sexpr ifTrue env) (eval-sexpr ifFalse env))))

(define (_cond args env)
  (if (null? args)
      (void)
      (let ([this (car args)] [rest (cdr args)])
        (let ([condition (car this)] [body (cdr this)])
          (if (eval-sexpr condition env) (last (eval-all body env)) (_cond rest env))))))

(define (let-init bindings eval-env save-env)
  (cond
    [(null? bindings) (void)]
    [else
     (let ([binding (car bindings)] [rest (cdr bindings)])
       (bind (car binding) (cadr binding) eval-env save-env)
       (let-init rest eval-env save-env))]))

(define (lambda-init keys args eval-env save-env)
  (cond
    [(and (null? keys) (null? args)) (void)]
    [(null? keys) (error "too many arguments")]
    [(null? args) (error "too few arguments")]
    [else
     (let ([key (car keys)] [rest-keys (cdr keys)] [arg (car args)] [rest-args (cdr args)])
       (bind key arg eval-env save-env)
       (lambda-init rest-keys rest-args eval-env save-env))]))

(define (bind key val eval-env save-env)
  (envir-def save-env key (eval-sexpr val eval-env)))

(define scheme
  (append
   (decorate -
             *
             /
             +
             <
             =
             >
             boolean?
             car
             cdr
             cons
             eq?
             equal?
             integer?
             not
             null?
             number?
             pair?
             procedure?
             real?
             string?)
   (list '#:and
         _and
         '#:begin
         (lambda (args env) (last (eval-all args env)))
         '#:cond
         _cond
         '#:define
         (lambda (args env)
           (let ([key (car args)] [val (cadr args)]) (envir-def env key (eval-sexpr val env))))
         '#:set!
         (lambda (args env)
           (let ([key (car args)] [val (cadr args)]) (envir-set! env key (eval-sexpr val env))))
         '#:display
         (lambda (args env) (printf "~a~n" (to-string (eval-sexpr (car args) env))))
         '#:else
         #t
         '#:eval
         (lambda (args env) (eval-sexpr (eval-sexpr (car args) env) env))
         '#:if
         _if
         '#:lambda
         (lambda (args parent-env)
           (let ([keys (car args)] [body (cdr args)])
             (lambda (args call-env)
               (let ([local-env (from-envir parent-env)])
                 (lambda-init keys args call-env local-env)
                 (last (eval-all body local-env))))))
         '#:let
         (lambda (args env)
           (let ([bindings (car args)] [body (cdr args)] [local-env (from-envir env)])
             (let-init bindings env local-env)
             (last (eval-all body local-env))))
         '#:let*
         (lambda (args env)
           (let ([bindings (car args)] [body (cdr args)] [local-env (from-envir env)])
             (let-init bindings local-env local-env)
             (last (eval-all body local-env))))
         '#:list
         eval-all
         '#:load
         (lambda (args env) (eval-file (eval-sexpr (car args) env) env))
         '#:or
         _or
         '#:symbol?
         (lambda (args env) (keyword? (car (eval-all args env))))
         '#:quote
         (lambda (args env) (car args)))))
