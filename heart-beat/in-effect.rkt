#lang racket

(define (in-effect effect)
  (make-do-sequence
   (lambda ()
     (values (λ (pos) (effect))      ; pos->element
             not                     ; next-pos
             #t                      ; initial-pos
             (λ (pos) pos)           ; continue?
             #f
             #f))))

(for/sum ([i  20]
          [x (in-cycle (in-range 4)
                       (in-effect (λ () (display "."))))])
  i)

; ....190

