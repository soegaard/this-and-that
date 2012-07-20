#lang racket

(provide split-between)

; split-between : pred xs -> (list (list x ...) ...)
;   The list xs is split into sublists.
;   For all neighbors x1 and x2 in xs, (pred x1 x2) determines whether the list is split.
(define (split-between pred xs)
  (let loop ([xs xs]    ; elements to group
             [ys '()]   ; current group filling up
             [xss '()]) ; groups
    (match xs
      [(list)                 (cond
                                [(and (empty? ys) (empty? xss))
                                 '()]
                                [(empty? ys) 
                                 (reverse xss)]
                                [else
                                 (reverse (cons (reverse ys) xss))])]
      [(list x)               (cond
                                [(and (empty? ys) (empty? xss))
                                 (list (list x))]
                                [(empty? ys) 
                                 (reverse (cons (list x) xss))]
                                [(pred (first ys) x)
                                 (reverse (cons (list x) (cons (reverse ys) xss)))]
                                [else (reverse (cons (reverse (cons x ys)) xss))])]
      [(list x more ...)       (cond
                                 [(empty? ys)
                                  (loop more (list x) xss)]
                                 [(pred (first ys) x)
                                  (loop more (list x) (cons (reverse ys) xss))]
                                 [else
                                  (loop more (cons x ys) xss)])])))


(module* test #f
  (require rackunit)
  ; The following uses split-between to implement group-consecutives.
  ; Inspiration: http://stackoverflow.com/questions/10368835/partitioning-a-list-in-racket
  (define (consecutive? x y)
    (= (+ x 1) y))
  
  (define (group-consecutives xs)
    (split-between (λ (x y) (not (consecutive? x y))) 
                   xs))

  (check-equal? (group-consecutives '(1 2 3 5 6 7 9 10 11)) '((1 2 3) (5 6 7) (9 10 11)))
  (check-equal? (group-consecutives '(1 2 3))               '((1 2 3)))
  (check-equal? (group-consecutives '(1 3 4 5 7 9 10 11 13))'((1) (3 4 5) (7) (9 10 11) (13)))
  (check-equal? (group-consecutives '(1))                   '((1)))
  (check-equal? (group-consecutives '())                    '()))


