#lang scheme

#|  denest.ss: A k-D tree datastructure for partitioning a set of points in R^n.
    Copyright (C) 2010 Will M. Farr <wmfarr@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(require srfi/67)

(define-struct empty-kd-tree
  ()
  #:transparent)

(define-struct cell-kd-tree
  (dim objects left right)
  #:transparent)

(define (kd-tree? obj)
  (or (empty-kd-tree? obj)
      (cell-kd-tree? obj)))

(provide/contract
 (kd-tree? (-> any/c boolean?))
 (struct empty-kd-tree
   ())
 (struct cell-kd-tree
   ((dim natural-number/c)
    (objects (listof any/c))
    (left kd-tree?)
    (right kd-tree?)))
 (objects->bounds (-> (-> any/c (vectorof real?))
                      (listof any/c)
                      (values (vectorof real?) (vectorof real?))))
 (objects->kd-tree
  (-> (-> any/c (vectorof real?))
      (listof any/c)
      kd-tree?)))

(define (incorporate-point! x low high)
  (for ((i (in-naturals))
        (x (in-vector x))
        (l (in-vector low))
        (h (in-vector high)))
    (when (< x l)
      (vector-set! low i x))
    (when (> x h)
      (vector-set! high i x))))

(define (objects->bounds ->coords objects)
  (let ((n (vector-length (->coords (car objects)))))
    (let ((low (make-vector n +inf.0))
          (high (make-vector n -inf.0)))
      (for ((obj (in-list objects)))
        (incorporate-point! (->coords obj) low high))
      (values low high))))

(define (longest-dimension low high)
  (let-values (((dim dx)
                (for/fold ((dim -1) (dx-max -inf.0))
                    ((l (in-vector low))
                     (h (in-vector high))
                     (i (in-naturals)))
                  (let ((dx (- h l)))
                    (if (> dx dx-max)
                        (values i dx)
                        (values dim dx-max))))))
    dim))

(define (split-bounds low high dim x)
  (let ((n (vector-length low)))
    (let ((new-low (vector-copy low))
          (new-high (vector-copy high)))
      (vector-set! new-low dim x)
      (vector-set! new-high dim x)
      (values new-low new-high))))

(define (all-equal? objs (compare default-compare))
  (or (null? objs)
      (let ((obj0 (car objs)))
        (for/and ((obj (in-list objs))) (=? compare obj0 obj)))))

(define (find-nth-sorted objects n (compare default-compare))
  (cond
   ((null? objects)
    (error 'find-nth-sorted "no objects"))
   ((null? (cdr objects))
    (when (not (= n 0))
      (error 'find-nth-sorted "index out of range"))
    (car objects))
   ((all-equal? objects) (car objects))
   (else
    (let ((N (length objects)))
      (when (>= n N)
        (error 'find-nth-sorted "index out of range"))
      (let ((pivot (list-ref objects (random N))))
        (let-values (((lte gt)
                      (partition
                       (lambda (obj)
                         (<=? compare obj pivot))
                       objects)))
          (let ((N-lte (length lte)))
            (if (< n N-lte)
                (find-nth-sorted lte n compare)
                (find-nth-sorted gt (- n N-lte) compare)))))))))

(define (coord-compare c1 c2)
  (vector-compare real-compare c1 c2))

(define (objects->kd-tree ->coords objects)
  (cond
   ((null? objects) (make-empty-kd-tree))
   ((null? (cdr objects)) (make-cell-kd-tree objects (make-empty-kd-tree) (make-empty-kd-tree)))
   ((all-equal? objects (lambda (o1 o2) (coord-compare (->coords o1) (->coords o2))))
    (make-cell-kd-tree objects (make-empty-kd-tree) (make-empty-kd-tree)))
   (else
    (let ((N (length objects)))
      (let-values (((low high)
                    (objects->bounds ->coords objects)))
        (let* ((dim (longest-dimension low high))
               (compare (lambda (o1 o2) (real-compare (vector-ref (->coords o1) dim)
                                                      (vector-ref (->coords o2) dim)))))
          (let ((pivot (find-nth-sorted objects (sub1 (floor (/ N 2))) compare)))
            (let-values (((lte gt)
                          (partition (lambda (obj) (<=? compare obj pivot)) objects)))
              (make-cell-kd-tree dim 
                                 objects
                                 (objects->kd-tree ->coords lte)
                                 (objects->kd-tree ->coords gt))))))))))





