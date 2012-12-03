#lang racket
;;; priority-queue.scm  --  Jens Axel Søgaard  --  Easter 2004

(provide make-empty
         empty?
         insert!
         delete-min!
         find-min
         change-priority!)

(require "evector.scm"
         ; (planet "evector.scm" ("soegaard" "evector.plt"))
         (lib "match.ss" "mzlib"))

(define-struct priority-queue (size values priorities index-change-hook) #:mutable)

(define make-empty
  (case-lambda
    [()                    (make-empty 0 (lambda (n i) 'skip))]
    [(n)                   (make-empty n (lambda (n i) 'skip))]
    [(n index-change-hook) (let ([vs (make-evector n #f #t)]
                                 [ps (make-evector n #f #t)])
                             (make-priority-queue 0 vs ps index-change-hook))]))

(define (empty? pq)
  (zero? (priority-queue-size pq)))

(define (insert! pq v p)
  (match pq
    [($ priority-queue size vs ps _)
     (adjust-up! pq v p size)
     (set-priority-queue-size! pq (+ size 1))]))


(define-syntax store!
  (syntax-rules ()
    [(_ pq i v p)
     (begin
       (evector-set! (priority-queue-values pq) i v)
       (evector-set! (priority-queue-priorities pq) i p)
       ((priority-queue-index-change-hook pq) v i))]))

(define (adjust-up! pq v p i)
  (match pq
    [($ priority-queue size vs ps _)
     (cond
       [(= i 0)  (begin
                   (evector-set! vs 0 v)
                   (evector-set! ps 0 p))]
       [else     (let ([parent (quotient i 2)])
                   (cond
                     [(< p (evector-ref ps parent)) (begin
                                                      (store! pq i (evector-ref vs parent) (evector-ref ps parent))
                                                      (adjust-up! pq v p (quotient i 2)))]
                     [else                          (store! pq i v p)]))])]))

(define (delete-min! pq)
  ; TODO: INSERT NON-EMPTY CHECK
  (match pq
    [($ priority-queue size vs ps _)
     (let ([min-value (evector-ref vs 0)]
           [min-pri   (evector-ref ps 0)])
       (set-priority-queue-size! pq (sub1 size))
       (adjust-down! pq 
                     (evector-ref vs (sub1 size))
                     (evector-ref ps (sub1 size))
                     0)
       (values min-value min-pri))]))

(define (adjust-down! pq v p i)
  (match pq
    [($ priority-queue size vs ps _)
     (let* ([left  (+ (* 2 i) 1)]
            [right (+ left 1)])
       (cond
         [(and (< left size)
               (< (evector-ref ps left) p))  (begin
                                               (store! pq i (evector-ref vs left) (evector-ref ps left))
                                               (adjust-down! pq v p left))]
         [(and (< right size)
               (< (evector-ref ps right) p)) (begin
                                               (store! pq i (evector-ref vs right) (evector-ref ps right))
                                               (adjust-down! pq v p right))]
         [else                               (store! pq i v p)]))]))

(define (find-min pq)
  (match pq
    [($ priority-queue size vs ps _) 
     (values (evector-ref vs 0)
             (evector-ref ps 0))]))

(define (change-priority! pq i p)
  (match pq
    [($ priority-queue size vs ps _)
     (if (> p (evector-ref ps i))
         (adjust-down! pq (evector-ref vs i) p i)
         (adjust-up! pq (evector-ref vs i) p i))]))

#;
(begin
  (define p (make-empty))
  (insert! p 'a 1)
  (insert! p 'b 2)
  (insert! p 'c 3)
  (insert! p 'd 0)
  
  (delete-min! p)
  (delete-min! p))
