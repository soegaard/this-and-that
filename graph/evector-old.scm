#lang racket
;;; evector.scm  --  Jens Axel S?gaard  -- 26 mar 2004

;;; EXTENSIBLE VECTOR

; This module provides extensible vectors called evectors.
; Setting LENGTH will increase the vectors length,
; new entries will be filled with FILL. Internally the
; vectors of size 16, 32, 64, ... is used in order to
; ensure amotized time O(1). Note that this implies that
; the space used by an evector is not changed by lowering
; the length.

(provide make-evector 
         (rename-out [%evector? evector?])
         evector-ref evector-set!
         (rename-out [%evector-length evector-length])
         (rename-out [set-%evector-length! set-evector-length!])
         evector-sub-fill!
         (rename-out [%evector-fill evector-fill])
         (rename-out [set-%evector-fill! set-evector-fill!])
         evector)

(define MIN-LENGTH 16)
(define DEFAULT-FILL '())
(define DEFAULT-EXPAND #f)

(define-struct %evector (length vector fill automatic-expansion-on-set!?) #:mutable)

(define make-evector
  (case-lambda 
    [(k)                  (make-evector k DEFAULT-FILL DEFAULT-EXPAND)]
    [(k fill)             (make-evector k fill DEFAULT-EXPAND)]
    [(k fill automatic)   (let ([len (max k MIN-LENGTH)])
                            (make-%evector k (make-vector len fill) fill 
                                           (or (eq? automatic 'automatic-expansion-on-set!)
                                               (eq? automatic #t))))]))

(define (evector-length v)
  (unless (%evector? v) (error "evector-length: expects arguments of type <evector>; given" v))
  (vector-length (%evector-vector v)))


(define (evector-ref v i)
  (unless (%evector? v)                (error "evector-ref: expects arguments of type <evector>; given" v))
  (unless (< -1 i (%evector-length v))  (error "evector-ref: index out of range; given: " v i))
  (vector-ref (%evector-vector v) i))

(define (evector-set! v i val)
  (unless (%evector? v)                (error "evector-set!: expects arguments of type <evector>; given" v))
  (unless (>= i 0)                     (error "evector-set!: index must be a non-negative integer: " v i))
  
  (cond
    [(< i (%evector-length v))                   (vector-set! (%evector-vector v) i val)]
    [(%evector-automatic-expansion-on-set!? v)  (begin
                                                  (set-evector-length! v (add1 i))
                                                  (evector-set! v i val))]
    [else                                        (error "evector-set!: index out of range; given: " v i val)]))


(define (set-evector-length! v l)
  (let ([max-len (vector-length (%evector-vector v))])
    (cond
      [(<= 0 l max-len) (set-%evector-length! v l)]
      [(> l max-len)    (begin
                          (expand-evector! v l)
                          (let ([old-len (%evector-length v)])
                            (set-evector-length! v l)
                            (evector-sub-fill! v old-len l)))])))

(define evector-sub-fill! 
  (case-lambda
    [(v start end)      (evector-sub-fill! v start end (%evector-fill v))]
    [(v start end fill) (let ([w    (%evector-vector v)]
                              [fill (%evector-fill v)])
                          (do ([i start (add1 i)])
                            [(= i end) (void)]
                            (vector-set! w i fill)))]))

(define (expand-evector! v l)
  (cond
    [(<= (* 2 l) (%evector-length v))  (void)]
    [else                              (let* ([new-size   (do ([len (* 2 (vector-length (%evector-vector v))) (* 2 len)])
                                                            [(<= (* 2 l) len) len])]
                                              [new-vector (make-vector new-size (%evector-fill v))]
                                              [old-vector (%evector-vector v)]
                                              [old-size   (vector-length old-vector)]
                                              [length     (%evector-length v)])
                                         (do ([i 0 (add1 i)])
                                           [(= i length) (void)]
                                           (vector-set! new-vector i (vector-ref old-vector i)))
                                         (set-%evector-vector! v new-vector))]))

(define (evector . xs)
  (vector->evector (list->vector xs)))
