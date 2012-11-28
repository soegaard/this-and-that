#lang racket

;;; This module implements 1-dimensional range trees.
;;; A set of n objects each with a x-coordinate is used to build
;;; a 1d-range tree. After construction the tree can be used to
;;; find which objects are in a given interval.
;;; The running time for construction is O(log n).
;;; The running time for a query is O(k + log n), where
;;; k is the number of found points.

(define-struct empty-1d-tree ()             #:transparent)
(define-struct cell-1d-tree  (y left right) #:transparent)
(define-struct leaf-1d-tree  (objects)      #:transparent)

(define (1d-tree? obj)
  (or (empty-1d-tree? obj)
      (cell-1d-tree? obj)
      (leaf-1d-tree? obj)))

(provide/contract
 (1d-tree? (-> any/c boolean?)) 
 (struct empty-1d-tree ()) 
 (struct cell-1d-tree ((y any/c) (left 1d-tree?) (right 1d-tree?)))
 (struct leaf-1d-tree ((objects (listof any/c)))) 
 (build-1d-tree    (-> (-> any/c real?) (listof any/c) 1d-tree?))
 (objects-in-range (-> (-> any/c real?) 1d-tree? real? real?   (listof any/c))))

(define (build-1d-tree ->coord objects)
  (define (half n) (sub1 (floor (/ n 2))))
  (define (object<= o1 o2) (<= (->coord o1) (->coord o2)))
  (define (object< o1 o2)  (<  (->coord o1) (->coord o2)))
  (define (object= o1 o2)  (=  (->coord o1) (->coord o2)))
  (define (all-equal? objects)
    (or (empty? objects)
        (empty? (cdr objects))
        (let ((o (car objects)))
          (for/and ([obj (in-list objects)])
            (object= obj o)))))  
  (define (kth-largest-object objects i) 
      ; 0<= i < (length list) 
      (cond 
        [(all-equal? objects) (car objects)]
        [else
         (let* ([N (length objects)]       
                [pivot (list-ref objects (random N))])
           (let-values ([(lte gt) (partition (λ (o) (object< o pivot)) objects)])
             (let ([N-lte (length lte)])
               (if (< i N-lte)
                   (kth-largest-object lte i)
                   (kth-largest-object gt (- i N-lte))))))]))
  (define (partition2 objects median N/2)
    (let loop ([os  objects] 
               [lt  '()]
               [eq  '()]
               [gt  '()]
               [len-lt 0]
               [len-eq 0])
      (cond
        [(or (= len-lt N/2) (empty? os))
         (if (= len-lt N/2)
             (values lt (append os gt))
             (let-values ([(eq1 eq2) (split-at eq (- N/2 len-lt))])
               (values (append lt eq1) (append eq2 gt))))]
        [(< (->coord (car os)) median)
         (loop (cdr os) (cons (car os) lt) eq gt (+ len-lt 1) len-eq)]
        [(= (->coord (car os)) median)
         (loop (cdr os) lt (cons (car os) eq)  gt len-lt (+ len-eq 1))]
        [else
         (loop (cdr os) lt eq (cons (car os) gt) len-lt len-eq)])))
  
  (define (build1 objects)
    (cond
      [(empty? objects)        (empty-1d-tree)]
      [(empty? (cdr objects))  (leaf-1d-tree objects)]
      [(all-equal? objects)    (leaf-1d-tree objects)]
      [else
       (let* ([N (length objects)]
              [pivot (kth-largest-object objects (half N))])
         (let-values ([(lte gt) (partition2 objects (->coord pivot) (quotient N 2))])
           #;(if (or (empty? lte) (empty? gt))
               (begin
                 (display 
                  (list (map ->coord lte)
                        (map ->coord gt)
                        (map ->coord objects)))
                 (newline)
                 (/ 0 0))
               'ok)
           (cell-1d-tree (->coord pivot) (build1 lte) (build1 gt))))]))  
  (build1 objects))



(define (path-to-leaf ->coord t x)
  ; return list of all subtrees on the search path of the coordinate x
  (define (search t)
    (cond
      [(empty-1d-tree? t)         '()]
      [(leaf-1d-tree? t)          (list t)]
      [(<= x (cell-1d-tree-y t))  (cons t (search (cell-1d-tree-left t)))]
      [else                       (cons t (search (cell-1d-tree-right t)))]))
  (search t))


(define (find-split-node ->coord t x1 x2)
  ; find the node where the search paths for x1 and x2 diverge
  ; x1 < x2
  (define (find t)
    (cond
      [(empty-1d-tree? t) #f]
      [(leaf-1d-tree? t)  t]
      [(<= x2 (cell-1d-tree-y t)) (find (cell-1d-tree-left t))]
      [(> x1  (cell-1d-tree-y t)) (find (cell-1d-tree-right t))]
      [else t]))
  (find t))


(define (objects-in-subtree t)
  (cond
    [(empty-1d-tree? t) '()]
    [(leaf-1d-tree? t)  (leaf-1d-tree-objects t)]
    [else               (append (append-map objects-in-subtree (list (cell-1d-tree-left t)))
                                (append-map objects-in-subtree (list (cell-1d-tree-right t))))]))

(define (objects-in-range ->coord t x1 x2)
  (define left cell-1d-tree-left)
  (define right cell-1d-tree-right)
  (define (filter-objects objects)
    (filter (λ (o) (and (<= x1 (->coord o) x2))) objects))
  (define (objects-right-of-path t x)
    (cond
      [(leaf-1d-tree? t)          (filter-objects (leaf-1d-tree-objects t))]
      [(<= x (cell-1d-tree-y t))  (append (objects-in-subtree (right t))
                                          (objects-right-of-path (left t) x))]
      [else                       (objects-right-of-path (right t) x)]))
  (define (objects-left-of-path t x)
    (cond
      [(leaf-1d-tree? t)          (filter-objects (leaf-1d-tree-objects t))]
      [(<= x (cell-1d-tree-y t))  (objects-left-of-path (left t) x)]
      [else                       (append (objects-in-subtree (left t))
                                          (objects-left-of-path (right t) x))]))
  (let ([s (find-split-node ->coord t x1 x2)])
    (if (leaf-1d-tree? s) 
        (filter-objects (leaf-1d-tree-objects s))
        (append (objects-right-of-path (cell-1d-tree-left s)  x1)
                (objects-left-of-path  (cell-1d-tree-right s) x2)))))

; example:
#;(objects-in-range values 
                    (build-1d-tree values '(1 2 3 4 5 6 7 8 9 10 11))
                    3.2 8.9)

#;(build-1d-tree (lambda (x) (vector-ref x 1)) '(#(0.0 0.0) #(0.0 186.0) #(0.0 50.0)))

