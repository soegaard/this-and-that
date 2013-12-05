#lang readtable 
{} terminating-macro "test-parser.rkt" parse
[] terminating-macro "test-parser.rkt" parse
#lang racket
(define x 1)
(define foo 
  (let ((x 2))
    3[123]))

; TODO: Make {...} syntax work in the repl
