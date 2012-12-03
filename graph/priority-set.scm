#lang racket
;;; priority-set.scm  --  Jens Axel S?gaard  --  Easter 2004

(provide make-empty
         empty?
         insert!
         delete-min!
         find-min
         member?
         change-priority!
         change-attribute!
         priority)

(require "evector.scm" ; (planet "evector.scm" ("soegaard" "evector.plt"))
         (prefix-in pq: "priority-queue.scm")
         (lib "match.ss" "mzlib"))

; Associated with each (integer) node n is an elm-strucrure. 
; The index is the place in the heap (used by change-priority!),
; attribute can be used for anything and priority is
; the elements priority in the the heap.

(define-struct elm (index attribute priority) #:mutable)
(define-struct priority-set (pq attributes) #:mutable)

(define make-empty
  (case-lambda
    [()  (make-empty 0)]
    [(n) (letrec ([index-change-hook (lambda (v i)
                                       (set-elm-index! (evector-ref (priority-set-attributes ps) v)
                                                       i))]
                  [ps                (make-priority-set (pq:make-empty n index-change-hook)
                                                        (make-evector 0 #f #t))])
           ps)]))

(define (empty? ps)
  (pq:empty? (priority-set-pq ps)))

(define (insert! ps v a p)
  (match ps
    [($ priority-set pq as)
     (evector-set! as v (make-elm 0 a p))
     (pq:insert! pq v p)]))

(define (delete-min! ps)
  (when (empty? ps)
    (error "delete-min!: Received empty priority set; " ps))
  (match ps
    [($ priority-set pq as)
     (let-values ([(min pri) (pq:delete-min! pq)])
       (let ([min-elm (evector-ref as min)])
         (evector-set! as min #f)
         (values min (elm-attribute min-elm) (elm-priority min-elm))))]))

(define (find-min ps)
  (when (empty? ps)
      (error "find-min: Received empty priority set; " ps))
  (match ps
    [($ priority-set pq as)
     (let-values ([(min pri) (pq:find-min pq)])
       (values min 
               (elm-attribute (evector-ref as min))
               pri))]))

(define (member? ps v)
  (match ps
    [($ priority-set _ as) (and (< v (evector-length as))
                                (evector-ref as v)
                                #t)]))

(define (change-priority! ps v p)
  (match ps
    [($ priority-set pq as)
     (let ([e (evector-ref as v)])
       (pq:change-priority! pq (elm-index e) p)
       (set-elm-priority! e p))]))

(define (change-attribute! ps v a)
  (match ps
    [($ priority-set pq as)
     (let ([e (evector-ref as v)])
       (set-elm-attribute! e a))]))

(define (priority ps v)
  (match ps
    [($ priority-set pq as)
     (elm-priority (evector-ref as v))]))

;(require priority-set)
;
(define ps (make-empty))
(insert! ps 2 'c 3)
(insert! ps 3 'd 4)
(insert! ps 0 'a 1)
(insert! ps 1 'b 2)
;
(change-priority! ps 3 0)

(priority ps 3)

(delete-min! ps)
(delete-min! ps)
(delete-min! ps)
(delete-min! ps)


