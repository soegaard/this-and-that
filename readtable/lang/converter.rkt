#lang racket
(provide current-modified-readtable
         current-specs)

(define current-specs (make-parameter '()))
(define current-modified-readtable (make-parameter #f))


