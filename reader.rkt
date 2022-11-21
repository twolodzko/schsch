#lang racket/base
(require racket/class)
(provide whitespace?
         word-end?
         new-reader
         pop
         peek
         read-sexpr
         parse-string)

(define (parse-string str)
  (read-sexpr (new-reader (open-input-string str) '())))

(define (read-sexpr reader)
  (skip-whitespace reader)
  (let ([chr (peek reader)])
    (if (eof-object? chr)
        eof
        (case chr
          [(#\;)
           (skip-till-newline reader)
           (read-sexpr reader)]
          [(#\')
           (pop reader)
           `(#:quote ,(read-sexpr reader))]
          [(#\")
           (pop reader)
           (read-string reader '())]
          [(#\()
           (pop reader)
           (read-list reader '())]
          [(#\)) (error (format "unexpected: ~s" chr))]
          [(#\, #\') (error (format "unimplemented: ~s" chr))]
          [else (parse-word (read-word reader '()))]))))

(define (read-word reader acc)
  (if (word-end? (peek reader))
      (list->string (reverse acc))
      (read-word reader (cons (pop reader) acc))))

(define (parse-word word)
  (cond
    [(equal? word "#t") #t]
    [(equal? word "#f") #f]
    [(string->number word)]
    [else (string->keyword word)]))

(define (read-list reader acc)
  (skip-whitespace reader)
  (case (peek reader)
    [(#\))
     (pop reader)
     (reverse acc)]
    [(#\;)
     (skip-till-newline reader)
     (read-list reader acc)]
    [else (read-list reader (cons (read-sexpr reader) acc))]))

(define (read-string reader acc)
  (let ([chr (pop reader)])
    (case chr
      [(#\") (list->string (reverse acc))]
      [(#\\) (read-string reader (cons (pop reader) acc))]
      [else (read-string reader (cons chr acc))])))

(define (skip-whitespace reader)
  (cond
    [(whitespace? (peek reader))
     (pop reader)
     (skip-whitespace reader)]))

(define (skip-till-newline reader)
  (case (pop reader)
    [(#\newline) (void)]
    [else (skip-till-newline reader)]))

(define (whitespace? chr)
  (case chr
    [(#\newline #\space #\return #\tab #\nul) #t]
    [else #f]))

(define (word-end? chr)
  (cond
    [(eof-object? chr)]
    [(whitespace? chr)]
    [(case chr
       [(#\( #\) #\' #\, #\" #\;) #t]
       [else #f])]
    [else #f]))

(define (list->string lst)
  (apply string lst))

(define reader%
  (class object%
    (init-field in cache)
    (super-new)
    (define/public (pop)
      (if (null? cache)
          (read-char in)
          (let ([out cache])
            (set! cache '())
            out)))
    (define/public (peek)
      (if (null? cache)
          (let ([chr (read-char in)])
            (set! cache chr)
            chr)
          cache))))

(define (new-reader in cache)
  (new reader% [in in] [cache cache]))

(define (pop reader)
  (send reader pop))

(define (peek reader)
  (send reader peek))
