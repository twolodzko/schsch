#lang racket/base
(provide eval-sexpr
         eval-all
         eval-string
         eval-file)
(require "envir.rkt"
         "reader.rkt")

(define (eval-sexpr sexpr env)
  (cond
    [(keyword? sexpr) (envir-ref env sexpr)]
    [(pair? sexpr) (eval-list sexpr env)]
    [else sexpr]))

(define (eval-list lst env)
  (let ([fun (eval-sexpr (car lst) env)] [args [cdr lst]]) (fun args env)))

(define (eval-all lst env)
  (map (lambda (sexpr) (eval-sexpr sexpr env)) lst))

(define (eval-string str env)
  (eval-sexpr (parse-string str) env))

(define (eval-file path env)
  (let ([reader (new-reader (open-input-file path) '())])
    (letrec ([impl (lambda (prev)
                     (let ([sexpr (read-sexpr reader)])
                       (cond
                         [(eof-object? sexpr) prev]
                         [else (impl (eval-sexpr sexpr env))])))])
      (impl (void)))))
