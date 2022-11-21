#lang racket/base
(require racket/string)
(provide to-string)

(define (to-string obj)
  (cond
    [(keyword? obj) (keyword->string obj)]
    [(list? obj) (format "(~a)" (string-join (map to-string obj) " "))]
    [(void? obj) ""]
    [else (format "~a" obj)]))
