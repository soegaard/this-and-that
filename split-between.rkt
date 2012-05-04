#lang racket

; split-between : pred xs -> (list (list x ...) ...)
;   The list xs is split into sublists.
;   For all neighbors x1 and x2 in xs, (pred x1 x2) determines whether the list is split.
;   Example:  (split-between = (list 1 1 2 3 3))  =>  '((1 1) (2) (3 3))
(define (split-between pred xs)
  (let loop ([xs xs] [ys '()] [xss '()])
    (match xs
      [(list)                 (reverse (cons (reverse ys) xss))]
      [(list x)               (reverse (cons (reverse (cons x ys)) xss))]
      [(list x1 x2 more ...)  (if (pred x1 x2) 
                                  (loop more (list x2) (cons (reverse (cons x1 ys)) xss))
                                  (loop (cons x2 more) (cons x1 ys) xss))])))

; The following uses split-between to implement group-consecutives.
; Inspiration: http://stackoverflow.com/questions/10368835/partitioning-a-list-in-racket

(define (consecutive? x y)
  (= (+ x 1) y))

(define (group-consecutives xs)
  (split-between (λ (x y) (not (consecutive? x y))) 
                 xs))

(group-consecutives '(1 2 3 5 6 7 9 10 11))   ; => '((1 2 3) (5 6 7) (9 10 11))
(group-consecutives '(1 2 3))                 ; => '((1 2 3))
(group-consecutives '(1 3 4 5 7 9 10 11 13))  ; => '((1) (3 4 5) (7) (9 10 11) (13))
(group-consecutives '(1))                     ; => '((1))
(group-consecutives '())                      ; => '()
