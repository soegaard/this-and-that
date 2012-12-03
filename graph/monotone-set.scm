#lang racket
;;; monotone-set.scm  --  Jens Axel Søgaard  25 march 2004

(provide (rename-out [init-monotone-set set])
         empty? delete-some! delete! member?)

;;; MONOTONE SET

; A MONOTONE SET is a set of integers, where only removals are allowed.
; The following operations are supported:

; (set n)           =   {0,1,...,n-1}                              
; (empty? M)       <=>  M=?
; (delete-some! M)      returns m such that m in M and m not in M'
;                       where M' is the state of M after
; (delete! M i)         deletes i from M
; (member? M i)    <=>  i in M

(define-struct monotone-set (size bits start) #:mutable)

; The "bit" vector makes MEMBER? and DELETE  O(1).
; The index start makes DELETE-SOME! amortized  O(1)

; (monotone-set n) = {0,1,...,n-1}
;  renamed when exported
(define (init-monotone-set n)
(when (not (and (integer? (inexact->exact n)) (>= n 0)))
    (error "monotone-set: n must be a non-negative integer, given: " n))
(make-monotone-set n (make-vector n #t) 0))

(define (empty? M)
  (zero? (monotone-set-size M)))

(define (delete-some! M)
  (when (empty? M)
    (error "delete-some!: Can't delete element from empty monotone set."))
  (let loop ([i (monotone-set-start M)])
    (cond
      [(vector-ref (monotone-set-bits M) i)  (begin
                                               (set-monotone-set-start! M i)
                                               (vector-set! (monotone-set-bits M) i #f)
                                               (set-monotone-set-size! M (sub1 (monotone-set-size M)))
                                               i)]
      [else                                  (loop (add1 i))])))

(define (delete! M i)
  (when (not (< -1 i (vector-length (monotone-set-bits M))))
    (error "delete!: Index out of range for the monotone set: " M i))
  (when (member? M i)
    (vector-set! (monotone-set-bits M) i #f)
    (set-monotone-set-size! M (sub1 (monotone-set-size M)))))

(define (member? M i)
  (when (not (< -1 i (vector-length (monotone-set-bits M))))
    (error "member?: Index out of range for the monotone set: " M i))
  (vector-ref (monotone-set-bits M) i))
