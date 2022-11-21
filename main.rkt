#! /usr/bin/env racket
#lang racket/base
(require "reader.rkt"
         "eval.rkt"
         "envir.rkt"
         "scheme.rkt"
         "printer.rkt"
         racket/cmdline)

(define files (make-parameter null))
(define parser (command-line #:args args (files args)))

(define (run-files files env)
  (cond
    [(null? files) (void)]
    [else
     (eval-file (car files) env)
     (run-files (cdr files) env)]))

(define (repl reader env)
  (let ([sexpr (read-sexpr reader)])
    (cond
      [(eof-object? sexpr) (void)]
      [else
       (printf "=> ~a~n" (to-string (eval-sexpr sexpr env)))
       (repl reader env)])))

(let ([env (from-records scheme)] [files (files)])
  (cond
    [(null? files)
     (printf "Press ^D to exit~n~n")
     (repl (new-reader (current-input-port) '()) env)]
    [else (run-files files env)]))
