#lang racket

(define (in-heart-beat n beat)
  (make-do-sequence
   (lambda ()
     (values (λ (pos) (when (= pos n) (beat)) #f) ; pos->element
             (λ (pos) (if (= pos n) 0 (+ pos 1))) ; next-pos
             0                                    ; initial-pos
             (λ (pos) #t)                         ; continue?
             #f
             #f))))

(for/sum ([i  100]
          [x (in-heart-beat 10 (λ () (display ".")))])
  i)

; .........4950