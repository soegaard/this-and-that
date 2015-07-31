#lang racket
(provide declare)

; (declare ([id type] ...) body ...)
;   Declares that id is bound to a struct of type type.
;   For each field f of the struct, an identifier id.f is
;   bound in the body .... which acceses the field f og the 
;   struct id is bound to. In other words:
;     id.f         == (type-f id)
;   If there are mutable fields id.f! is can be used to
;   alter the field f of id.
;     (id.f! expr) == (set-type-f! id expr)

(require 
 (for-syntax racket/struct-info
             racket/syntax))

(begin-for-syntax 
  (define (syntax-map f stx)
    (map f (syntax->list stx))))

(define-syntax (declare stx)
  (syntax-case stx ()
    [(_ () . body) 
     #'(let () . body)]
    [(_ ([id type] decl ...) . body)
     (let ()
       ; if s is a sub, then sub-str -> s.str 
       (define (accessor->dot a)
         (define type-str (format "~a" (syntax-e #'type)))
         (define acc-str  (format "~a" (syntax-e a)))
         (define suffix (substring acc-str (add1 (string-length type-str))))
         (format-id stx "~a.~a" #'id suffix))
       (define (mutator->dot m)
         (and (symbol? (syntax-e m))
              (let ()
                (define type-str (format "~a" (syntax-e #'type)))
                (define mut-str  (format "~a" (syntax-e m)))
                (define suffix (substring mut-str (+ 5 (string-length type-str))))
                (format-id stx "~a.~a" #'id suffix))))
       (define (mutator->++ m)
         (and (symbol? (syntax-e m))
              (let ()
                (define type-str (format "~a" (syntax-e #'type)))
                (define mut-str  (format "~a" (syntax-e m)))
                (define suffix (substring mut-str (+ 5 (string-length type-str))))
                (format-id stx "~a.~a++" #'id suffix))))
       (define (mutator->+= m)
         (and (symbol? (syntax-e m))
              (let ()
                (define type-str (format "~a" (syntax-e #'type)))
                (define mut-str  (format "~a" (syntax-e m)))
                (define suffix (substring mut-str (+ 5 (string-length type-str))))
                (format-id stx "~a.~a+=" #'id suffix))))
       (define (mutator->acc m)
         (and (symbol? (syntax-e m))
              (let ()
                (define type-str (format "~a" (syntax-e #'type)))
                (define mut-str  (format "~a" (syntax-e m)))
                (define suffix! (substring mut-str (+ 5 (string-length type-str))))
                (define suffix  (substring suffix! 0 (sub1 (string-length suffix!))))
                (format-id stx "~a-~a" #'type suffix))))
       (with-syntax ([(info constructor predicate? accessors mutators super)
                      (extract-struct-info (syntax-local-value #'type))])
         (with-syntax ([(s.accessor ...) (syntax-map accessor->dot #'accessors)]
                       [(accessor ...)   #'accessors]
                       [(s.mutator ...)  (filter values (syntax-map mutator->dot #'mutators))]
                       [(mutator ...)    (filter values (syntax->list #'mutators))]
                       [(s.++ ...)  (filter values (syntax-map mutator->++ #'mutators))]
                       [(++ ...)    (filter values (syntax->list #'mutators))]
                       [(s.+= ...)  (filter values (syntax-map mutator->+= #'mutators))]
                       [(+= ...)    (filter values (syntax->list #'mutators))]
                       [(mut-acc ...)  (filter values (syntax-map mutator->acc #'mutators))])
           #'(let-syntax ([s.accessor (λ (so) #'(accessor id))] ...)
               (let-syntax ([s.mutator (λ (so) (syntax-case so () [(_ val) #'(mutator id val)]))] ...)
                 (let-syntax ([s.++ (λ (so) (syntax-case so () [(_) #'(mutator id (add1 (mut-acc id)))]))] ...)
                   (let-syntax ([s.+= (λ (so) (syntax-case so () [(_ val) #'(mutator id (+ (mut-acc id) val))]))] ...)
                     (declare (decl ...) . body))))))))]
    [_ (error)]))


(module+ test 
  (require rackunit)
  (struct sub (str start end) #:mutable)
  
  (define text "This is a long string, from which we want to extract substrings.")
  (define sub1 (sub text 0 4))
  (define (sub->string s)
    (declare ([s sub])
      (substring s.str s.start s.end)))
  (define (skip-one s)
    (declare ([s sub])
      (s.start! (+ s.start 1))))
  (define (skip-two s)
    (declare ([s sub])
      (s.start!+= 2)))
  (check-equal? (sub->string sub1) "This")
  (skip-one sub1)
  (check-equal? (sub->string sub1) "his")
  (skip-two sub1)
  (check-equal? (sub->string sub1) "s")
  (declare ([sub1 sub])
    (sub1.start!++))
  (check-equal? (sub->string sub1) ""))
