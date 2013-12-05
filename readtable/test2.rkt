#lang readtable 
{} terminating-macro "pratt.rkt" parse
#lang racket/base
(provide solve)
(define (solve a b c)
  (define d {b^2-4*a*c})
  (cond [(< d 0) '()]
        [(= d 0) (list {-b/(2*a)})]
        [else    (define r1 {(-b-sqrt[d])/(2*a)})
                 (define r2 {(-b+sqrt[d])/(2*a)})
                 (list (min r1 r2) (max r1 r2))]))

(solve 1 0 -4)
(solve 1 0 0)
(solve 1 0 1)
