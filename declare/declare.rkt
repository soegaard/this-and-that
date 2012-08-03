#lang racket
(provide declare)

; (declare ([id type] ...) body ...)
;   Declares that id is bound to a struct of type type.
;   For each field f of the struct, an identifier id.f is
;   bound in the body .... which acceses the field f og the 
;   struct id is bound to. In other words:
;     id.f == (type-f id)

(require 
 (for-syntax racket/struct-info
             unstable/syntax
             racket/syntax))

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
         (define suffix (substring acc-str (add1 (string-length type-str)) (string-length acc-str)))
         (format-id stx "~a.~a" #'id suffix))       
       (with-syntax ([(info constructor predicate? accessors mutators super)
                      (extract-struct-info (syntax-local-value #'type))])
         (with-syntax ([(s.accessor ...) (syntax-map accessor->dot #'accessors)]
                       [(accessor ...)   #'accessors])
           #'(let-syntax ([s.accessor (λ (so) #'(accessor id))] ...) 
               (declare (decl ...) . body)))))]
    [_ (error)]))


(module+ test 
  (require rackunit)
  (struct sub (str start end))
  
  (define text "This is a long string, from which we want to extract substrings.")
  (define sub1 (sub text 0 4))
  (define (sub->string s)
    (declare ([s sub])
             (substring s.str s.start s.end)))
  (check-equal? (sub->string sub1) "This"))