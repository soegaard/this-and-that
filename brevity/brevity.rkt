#lang racket
; An application like (foo bar) expands to (#%app foo bar).
; The standard meaning of #%app is to apply foo to bar.
; This file shows how to define a new #%app that 
; treat (foo bar) and {foo bar} differently.

(module common racket/base
  (provide curly?)
  ; curly? : syntax -> boolean
  ;  return #t if the syntax property 'paren-shape is #\{
  (define (curly? stx)
    (let ([p (syntax-property stx 'paren-shape)])
      (and p (eqv? p #\{)))))

(module ref racket
  (provide (rename-out [app #%app]))
  (require (for-syntax (submod ".." common)))
  
  ; SYNTAX
  ;   (app arg ...) works as standard (#%app arg ...)
  ;   {app arg0 arg ...) dispatches on the type of arg0.
  ;   E.g. {"abc" 1} returns #\b
  (define-syntax (app stx)
    (syntax-case stx ()
      [(_ val index) 
       (curly? stx)
       (syntax/loc stx
         (let* ([v val] [i index])
           (cond
             [(string? v)   (string-ref v i)]
             [(vector? v)   (vector-ref v i)]
             [(list? v)     (list-ref v i)]
             [(hash? v)     (hash-ref v i)]
             [(sequence? v) (sequence-ref v i)]
             [else          (v i)])))]
      [(_ . more) 
       (syntax/loc stx (#%app . more))])))

(module sub racket
  (provide (rename-out [app #%app]))
  (require (for-syntax (submod ".." common)) 
           (submod ".." ref))
  
  ; In the module sub, we extend the idea from ref.
  ; {arg0 arg1 arg1} will extract a subsequence from the sequence arg0.  
  (define-syntax (app stx)
    (syntax-case stx (_)
      
      [(? val index1)
       (curly? stx)
       (quasisyntax/loc stx
         (let ([v val] [i index1])
           (cond
             [(string? v)   (substring v i (string-length v))]
             [(vector? v)   (vector-copy v i (vector-length v))]
             [(list? v)     (list-tail v i)]
             [(hash? v)     #,(syntax/loc stx (error 'ref-app 
                                             "the (v i _) syntax is not supported for hash tables"))]
             [(sequence? v) (sequence-tail v i)]
             [else #,(syntax/loc stx (#%app val index1 #\_))])))] 
      
      [(? val _ index2)
       (curly? stx)
       (quasisyntax/loc stx
         (let ([v val]
               [j index2])
           (cond
             [(string? v)   (substring v 0 j)]
             [(vector? v)   (vector-copy v 0 j)]
             [(list? v)     (take v j)]
             [(hash? v)     #,(syntax/loc stx (error 'ref-app 
                                              "the (v _ i) syntax is not supported for hash tables"))]
             ; [(sequence? v) (sequence-take v j)]
             [else #,(syntax/loc stx (#%app val #\_ index2))])))]
      [(? val index1 _)
       (curly? stx)
       (quasisyntax/loc stx
         (let ([v val] [i index1])
           (cond
             [(string? v)   (substring v i (string-length v))]
             [(vector? v)   (vector-copy v 0 (vector-length v))]
             [(list? v)     (drop v i)]
             [(hash? v)     #,(syntax/loc stx (error 'ref-app 
                                              "the (v i _) syntax is not supported for hash tables"))]
             ; [(sequence? v) (sequence-take v j)]
             [else #,(syntax/loc stx (#%app val index1 #\_ ))])))]
      
      [(__ val index1 index2)
       (curly? stx)
       (syntax/loc stx
         (let ([s val] [i index1] [j index2])
           (cond
             [(string? s)   (substring s i j)]
             [(vector? s)   (vector-copy s i j)]
             [(list? s)     (take (drop s i) j)]
             [(hash? s)     (hash-ref s i j)]
             ; [(sequence? s) (sequence-take (sequence-tail s i) (- j i))]
             [else (s i j)])))]
      
      [(_ . more) 
       (syntax/loc stx (#%app . more))])))

(module sum racket
  (provide (rename-out [app #%app]))
  (require (for-syntax (submod ".." common)) 
           (submod ".." sub))
  
  ; This makes {+ ...} behave as string concatenation.
  (define-syntax (app stx)
    (syntax-case stx (+)
      [(_ + a b c ...)
       (curly? stx)
       #'(cond
           [(and (string? a) (string? b))
            (app (string-append a b))])])))


(require 'sub)
{"foobar" 3}   ; #\b
{"foobar" 3 4} ; "b"
{"foobar" 3 5} ; "ba"
{"foobar" _ 3} ; "foo"
{"foobar" 3 _} ; "bar"  ( _ mean "end" )





