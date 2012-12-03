#lang racket
;;; non-deterministic-set.scm   --  Jens Axel Søgaard

; A NON-DETERMINISTIC SET implements bounded integer sets.
; The datastructure are called non-deterministic sets,
; due to delete-some!, which runs in amortized O(1).

;  make-set     : type size [(list integer)] -> NS
;    return a NS representing either the empty set, or
;    the set with elements given in the optional argument
;    (all of which must be less than size).
;    The type element, which is either 'stack or 'queue,
;    determines the discipline used by delete-some! .
;    If type is 'queue delete-some! deletes the oldest
;    element in the set; if type is 'stack it deletes the
;    youngest.

;  empty?       : NS -> boolean
;    returns #t if NS is empty, otherwise #f

;  insert!      : NS integer -> void
;    insert the given integer in the set

;  delete-some! : NS -> integer
;    return an element from the set and destructively remove
;    it from the set

;  member?      : NS integer -> integer
;    return #t if the integer is in the set, otherwise #f

;; TIME ANALYSIS

;  make-set      O(n)

;  empty?        O(1)
;  insert!       O(1)
;  delete-some!  O(1) amortized
;  member?       O(1)


;;; NON DETERMINISTIC SET

(provide make-set empty? insert! delete-some! member?)

(require (prefix-in bc: "bounded-stack-and-queue.scm"))

(define-struct non-deterministic-set (size bits collection) #:mutable)

(define (make-set type n . initial-contents)
  (when (not (and (integer? (inexact->exact n)) (>= n 0)))
    (error "set: The size of an non-deterministic set must be a non-negative integer, given: " n))
  (let ([NS (make-non-deterministic-set 0
                                        (make-vector n #f)
                                        (bc:make-empty type n))])
    (for-each (lambda (e) (insert! NS e))
              initial-contents)
    NS))

(define (empty? NS)
  (zero? (non-deterministic-set-size NS)))

(define (delete-some! NS)
  (when (empty? NS)
    (error "Can't delete from empty set"))
  (let* ([collection (non-deterministic-set-collection NS)]
         [i (bc:remove! collection)])
    (vector-set! (non-deterministic-set-bits NS) i #f)
    (set-non-deterministic-set-size! NS (sub1 (non-deterministic-set-size NS)))
    i))

(define (insert! NS i)
  (when (not (< -1 i (vector-length (non-deterministic-set-bits NS))))
    (error "insert!: index out of range in the non-deterministic set, given: " NS i))
  (when (not (member? NS i))
    (vector-set! (non-deterministic-set-bits NS) i #t)
    (bc:insert! (non-deterministic-set-collection NS) i)
    (set-non-deterministic-set-size! NS (add1 (non-deterministic-set-size NS)))))

(define (member? NS i)
  (when (not (< -1 i (vector-length (non-deterministic-set-bits NS))))
    (error "insert!: index out of range in the non-deterministic set, given: " NS i))
  (vector-ref (non-deterministic-set-bits NS) i))

;;; TEST

;(require non-deterministic-set)
;
;(define Q (make-set 'queue 3))
;(insert! Q 1)
;(insert! Q 2)
;(insert! Q 0)
;(if (equal? (list (delete-some! Q)
;                  (delete-some! Q)
;                  (delete-some! Q))
;            (list 1 2 0))
;    'passed
;    '|FAILED|)
;
;(define S (make-set 'stack 3))
;(insert! S 1)
;(insert! S 2)
;(insert! S 0)
;(if (equal? (list (delete-some! S)
;                  (delete-some! S)
;                  (delete-some! S))
;            (list 0 2 1))
;    'passed
;    '|FAILED|)






