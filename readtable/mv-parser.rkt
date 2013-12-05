#lang racket
(provide parse)

; This parser simply reads expressions e1 ...
; from a string and returns (values e1 ...)
; This is makes it convenient to produce multiple values.

(define (parse src str)
  (define tokens
    (for/list ([token (in-port read (open-input-string str))])
      token))
  #`(values #,@tokens))
