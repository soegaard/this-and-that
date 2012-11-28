#lang racket
(require racket/trace)

; sorted
(define xs (vector 3 10 19 23 30 37 49 59 62 70 80 89 100 105 100))

(define-struct tree (x l r) #:transparent)

(define (sorted-vector->tree v)
  (define (vr i) (vector-ref v i))
  ; l inclusive, m exclusive
  (define (loop l m)
    (cond
      [(= l m)       #f]
      [(= (+ l 1) m) (vr l)]
      [else          (let ([median (quotient (+ l m) 2)])
                       (tree (vr median)
                             (loop l median)
                             (loop (+ 1 median) m)))]))
  (loop 0 (vector-length v)))


(define t (sorted-vector->tree xs))

(define (path t x)
  (cond
    [(not (tree? t))     (list t)]
    [(= (tree-x t) x)    (list t)]
    [(< x (tree-x t))    (cons t (path (tree-l t) x))]
    [else                (cons t (path (tree-r t) x))]))

(define (tree->list t)
  (cond
    [(not t) '()]
    [(not (tree? t)) (list t)]
    [else (append (tree->list (tree-l t)) 
                  (list (tree-x t))
                  (tree->list (tree-r t)))]))

#;(define (path-to t x)
    (map tree-x (path t x)))

(define (find-split-node t x1 x2)
  ; x1 < x2
  (cond
    [(not (tree? t)) t]
    [(< x2 (tree-x t)) (find-split-node (tree-l t) x1 x2)]
    [(> x1 (tree-x t)) (find-split-node (tree-r t) x1 x2)]
    [else t])) 

; (trace tree->list)

; (path t 4)

; (path-to t 4)

; (define (subtree-elements path acc)

(let ([x1 18] [x2 77])
  (let ([s (find-split-node t x1 x2)])
    (cond
      [(and (not (tree? t)) (<= x1 t x2)) 
       (list t)]
      [(not (tree? t))                    
       '()]
      [else 
       (append 
        (apply append (map (lambda (t) (if (tree? t) (tree->list (tree-r t)) (list t))) (reverse (cdr (path s x1)))))
        (apply append (map (lambda (t) (if (tree? t) (tree->list (tree-l t)) (list t))) (reverse (cdr (path s x2))))))])))



(define (min-tree t1 t2)
  (cond 
    [(and (tree? t1) (tree? t2))
     (if (< (tree-x t1) (tree-x t2)) t1 t2)]
    [(tree? t1)
     (if (< (tree-x t1) t2) t1 t2)]
    [(tree? t2)
     (if (< t1 (tree-x t2)) t1 t2)]
    [(< t1 t2) t1]
    [else t2]))

(define (max-tree t1 t2)
  (cond 
    [(and (tree? t1) (tree? t2))
     (if (>= (tree-x t1) (tree-x t2)) t1 t2)]
    [(tree? t1)
     (if (>= (tree-x t1) t2) t1 t2)]
    [(tree? t2)
     (if (>= t1 (tree-x t2)) t1 t2)]
    [(>= t1 t2) t1]
    [else t2]))        

;(define (combine-trees ts)
  (define (value t)
    (if (tree? t)
        (tree-x t)
        t))
  (define (combine ts l m)
    (cond
      [(= l m) 
       '()]
      [(= (+ l 1) m)
       (list (vector-ref ts l))]
      [(= (+ l 2) m)
       (let* ([t1 (vector-ref ts l)]
              [t2 (vector-ref ts (+ l 1))])
         (list (tree (value t1) t1 t2)))]
      [else
       (list (combine ts l (+ (quotient (+ l m) 2) 1))
             (combine ts (+ (quotient (+ l m) 2) 1) m))]))
  #;(let loop ([ts ts])
    (if (= (vector-length ts) 1)
        (vector-ref ts 0)
        (loop (list->vector (flatten (combine ts 0 (vector-length ts)))))))
  ;)

;(trace combine-trees)


;(combine-trees xs)

