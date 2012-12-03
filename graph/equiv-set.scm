#lang racket
;;; equiv-set.scm  --  Jens Axel Søgaard 

; This module implements equivalence relations.

; Reference: [Cormen et al., p. 505-508]

; Each element in bits contains the index of
; parent in the rooted tree. The root contains it's own
; index. Two elements are equivalent if and only if
; the trees which they belong to has the same root.
; The rank of each is node is an upper bound on the
; height if the node.


(provide make equivalent? join!)

(define-struct equiv-set (bits ranks))


(define (make n)
  ; Return {0}{1}{2}...{n-1}
  (make-equiv-set (let ([bits (make-vector n 0)])
                    (do ([i 0 (add1 i)])
                      [(= i n) bits]
                      (vector-set! bits i i)))
                  (make-vector n 1)))


(define (link! S x y)
  (let ([bits  (equiv-set-bits S)]
        [ranks (equiv-set-ranks S)])
    (let ([rank-x (vector-ref ranks x)]
          [rank-y (vector-ref ranks y)])
      (if (> rank-x rank-y)
          (vector-set! bits y x)
          (begin
            (vector-set! bits x y)
            (when (= rank-x rank-y) 
              (vector-set! ranks y (add1 rank-y))))))))

(define (join! S x y)
  ; Join the equivalence classes of x and y
  (link! S (find! S x) (find! S y)))

(define (find! S x)
  (let* ([bits (equiv-set-bits S)]
         [bits-x (vector-ref bits x)])
    (unless (= x bits-x)
      (vector-set! bits x (find! S (vector-ref bits x))))
    (vector-ref bits x)))

(define (equivalent? S x y)
  (= (find! S x) (find! S y)))
