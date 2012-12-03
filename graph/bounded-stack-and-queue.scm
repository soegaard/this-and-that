#lang racket
;;; bounded-stack-and-queue.scm  -- Jens Axel Søgaard

(provide insert! remove! (rename-out [make-empty-bounded-collection make-empty]))

(require (lib "match.ss" "mzlib"))

(define-struct bounded-collection (stack/queue elements size first) #:mutable)

(define (make-empty-bounded-collection type n)
  (make-bounded-collection type (make-vector n '()) 0 0))

(define (insert! BC x)
  (match BC
    [($ bounded-collection 'stack elements size _)
     (vector-set! elements size x)
     (set-bounded-collection-size! BC (add1 size))]
    [($ bounded-collection 'queue elements size first)
     (let ([len (vector-length elements)])
       (vector-set! elements (modulo (+ size first) len) x)
       (set-bounded-collection-size! BC (add1 size)))]
    [else
     (error "huh" (list (bounded-collection-stack/queue BC)
                        (bounded-collection-elements BC)
                        (bounded-collection-size BC)
                        (bounded-collection-first BC)))]))

(define (remove! BC)
  (match BC
    [($ bounded-collection 'stack elements size _)
     (begin0
       (vector-ref elements (sub1 size))
       (set-bounded-collection-size! BC (sub1 size)))]
    [($ bounded-collection 'queue elements size first)
     (let ([len (vector-length elements)])
       (begin0
         (vector-ref elements first)
         (set-bounded-collection-first! BC (modulo (add1 first) len))
         (set-bounded-collection-size! BC (sub1 size))))]))

