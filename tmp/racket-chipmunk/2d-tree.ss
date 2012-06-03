#lang racket
(require "1d-tree.rkt")

;;; This module implements 2-dimensional range trees.

(define-struct empty-2d-tree ()                         #:transparent)
(define-struct cell-2d-tree  (x left right associated)  #:transparent)
(define-struct leaf-2d-tree  (objects associated)       #:transparent)

(define (2d-tree? obj)
  (or (empty-2d-tree? obj)
      (cell-2d-tree? obj)
      (leaf-2d-tree? obj)))

(provide/contract
 (2d-tree? (-> any/c boolean?)) 
 (struct empty-2d-tree ()) 
 (struct cell-2d-tree ((x real?) (left 2d-tree?) (right 2d-tree?) (associated 1d-tree?)))
 (struct leaf-2d-tree ((objects (listof any/c)) (associated 1d-tree?)))
 (build-2d-tree (-> (-> any/c (vectorof real?)) (listof any/c) 2d-tree?))
 (objects-in-2d-range (-> (-> any/c (vectorof real?)) 2d-tree? real? real? real? real?   (listof any/c))))

(define (build-2d-tree ->coord objects)
  (define (x-coord o) (vector-ref (->coord o) 0))
  (define (y-coord o) (vector-ref (->coord o) 1))
  (define (half n) (sub1 (floor (/ n 2))))
  (define (object<= o1 o2) (<= (x-coord o1) (x-coord o2)))
  (define (object< o1 o2)  (<  (x-coord o1) (x-coord o2)))
  (define (object= o1 o2)  (=  (x-coord o1) (x-coord o2)))
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
         (let-values ([(lt gte) (partition (λ (o) (object< o pivot)) objects)])
           (let ([N-lt (length lt)])
             (if (< i N-lt)
                 (kth-largest-object lt i)
                 (kth-largest-object gte (- i N-lt))))))]))
  
  (define (build objects)
    (let ([associated (build-1d-tree y-coord objects)])
      (cond
        [(empty? objects)        (empty-2d-tree)]
        [(empty? (cdr objects))  (leaf-2d-tree objects associated)]
        [(all-equal? objects)    (leaf-2d-tree objects associated)]
        [else
         (let* ([N (length objects)]
                [pivot (kth-largest-object objects (half N))])
           (let*-values ([(lte gt) (partition (λ (o) (object<= o pivot)) objects)]
                         [(lte gt) (if (null? gt)
                                       (split-at (sort objects object<) (half N))
                                       (values lte gt))])
             (cell-2d-tree (x-coord pivot) (build lte) (build gt) associated)))])))
  (build  objects))

#;(define (path-to-leaf ->coord t x)
    ; return list of all subtrees on the search path of the coordinate x
    (define (search t)
      (cond
        [(empty-2d-tree? t)                        '()]
        [(leaf-2d-tree? t)                         (list t)]
        [(<= x (->coord (cell-2d-tree-object t)))  (cons t (search (cell-2d-tree-left t)))]
        [else                                      (cons t (search (cell-2d-tree-right t)))]))
    (search t))


(define (find-split-node ->coord t x1 x2)
  ; find the node where the search paths for x1 and x2 diverge
  ; x1 < x2
  (define (find t)
    (cond
      [(empty-2d-tree? t) #f]
      [(leaf-2d-tree? t)  t]
      [(<= x2 (cell-2d-tree-x t)) (find (cell-2d-tree-left t))]
      [(> x1  (cell-2d-tree-x t)) (find (cell-2d-tree-right t))]
      [else t]))
  (find t))


#;(define (objects-in-subtree t)
    (cond
      [(empty-2d-tree? t) '()]
      [(leaf-2d-tree? t)  (leaf-2d-tree-objects t)]
      [else               (append (append-map objects-in-subtree (list (cell-2d-tree-left t)))
                                  (append-map objects-in-subtree (list (cell-2d-tree-right t))))]))

(define (objects-in-2d-range ->coord t x1 x2 y1 y2)
  (define left cell-2d-tree-left)
  (define right cell-2d-tree-right)
  (define (associated t)
    ((if (leaf-2d-tree? t) leaf-2d-tree-associated cell-2d-tree-associated) t))
  (define (x-coord o) (vector-ref (->coord o) 0))  
  (define (y-coord o) (vector-ref (->coord o) 1))
  (define (filter-objects objects)
    (filter (λ (o) (and (<= x1 (x-coord o) x2) 
                        (<= y1 (y-coord o) y2)))
            objects))
  (define (objects-right-of-path t x)
    (cond
      [(leaf-2d-tree? t)          (filter-objects (leaf-2d-tree-objects t))]
      [(<= x (cell-2d-tree-x t))  (append (objects-in-range y-coord (associated (right t)) y1 y2)
                                          (objects-right-of-path (left t) x))]
      [else                       (objects-right-of-path (right t) x)]))
  (define (objects-left-of-path t x)
    (cond
      [(leaf-2d-tree? t)          (filter-objects (leaf-2d-tree-objects t))]
      [(<= x (cell-2d-tree-x t))  (objects-left-of-path (left t) x)]
      [else                       (append (objects-in-range y-coord (associated (left t)) y1 y2)
                                          (objects-left-of-path (right t) x))]))
  (let ([s (find-split-node ->coord t x1 x2)])
    (if (leaf-2d-tree? s) 
        (filter-objects (leaf-2d-tree-objects s))
        (append (objects-right-of-path (cell-2d-tree-left s)  x1)
                (objects-left-of-path  (cell-2d-tree-right s) x2)))))

; example:
#;(objects-in-2d-range values
                       (build-2d-tree values '(#(1 2) #(3 4) #(5 6) #(7 8) #(9 10)))
                       3 7 3 9)

