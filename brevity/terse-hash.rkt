#lang racket

(module terse-hash racket
  (provide (rename-out [app #%app]))
  
  (begin-for-syntax
    (define (curly? stx)
      (let ([p (syntax-property stx 'paren-shape)])
        (and p 
             (or (eqv? p #\{)
                 (and (pair? p)
                      (or (eqv? (car p) #\{)
                          (eqv? (cdr p) #\{))))))))
    
  (define-syntax (app stx)
    (syntax-case stx 
      (? equal? eq? eqv? weak? hash hasheq hasheqv 
         make make-eqv make-eq 
         make-weak make-weak-eqv make-weak-eq
         make-immutable make-immutable-eqv make-immutable-eq
         ! set! set*! set
         ref ref! has-key?
         update update! remove remove! 
         map keys values ->list for-each
         count copy)
      [{_}
       (curly? stx)
       (syntax/loc stx (make-hash))]
      [{_ ? s} 
       (curly? stx)
       (syntax/loc stx (hash-? s))]
      [{_ equal? s} 
       (curly? stx)
       (syntax/loc stx (hash-equal? s))]
      [{_ eqv? s} 
       (curly? stx)
       (syntax/loc stx (hash-eqv? s))]
      [{_ eq? s} 
       (curly? stx)
       (syntax/loc stx (hash-eq? s))]
      [{_ weak? s} 
       (curly? stx)
       (syntax/loc stx (hash-weak? s))]
      [{_ hash . more} 
       (curly? stx)
       (syntax/loc stx (hash . more))]
      [{_ hasheq . more} 
       (curly? stx)
       (syntax/loc stx (hasheq . more))]
      [{_ hasheqv . more} 
       (curly? stx)
       (syntax/loc stx (hasheqv . more))]
      [{_ make . more} 
       (curly? stx)
       (syntax/loc stx (make-hash . more))]
      [{_ make-eqv . more} 
       (curly? stx)
       (syntax/loc stx (make-hasheqv . more))]
      [{_ make-eq . more} 
       (curly? stx)
       (syntax/loc stx (make-hasheq . more))]
      [{_ make-weak . more} 
       (curly? stx)
       (syntax/loc stx (make-weak-hash . more))]
      [{_ make-weak-eqv . more} 
       (curly? stx)
       (syntax/loc stx (make-weak-hasheqv . more))]
      [{_ make-weak-eq . more} 
       (curly? stx)
       (syntax/loc stx (make-weak-hasheq . more))]
      [{_ make-immutable . more} 
       (curly? stx)
       (syntax/loc stx (make-hash-immutable . more))]
      [{_ make-immutable-eqv . more} 
       (curly? stx)
       (syntax/loc stx (make-immutable-hasheqv . more))]
      [{_ make-immutable-eq . more} 
       (curly? stx)
       (syntax/loc stx (make-immutable-hasheq . more))]
      [{_ ref h k} 
       (curly? stx)
       (syntax/loc stx (hash-ref h k v))]
      [{_ ref h k f} 
       (curly? stx)
       (syntax/loc stx (hash-ref h k f))]
      [{_ has-key? h k} 
       (curly? stx)
       (syntax/loc stx (hash-hash-key? h k))]
      [{_ ref! h k v} 
       (curly? stx)
       (syntax/loc stx (hash-ref! h k v))]
      [{_ ! h k v} 
       (curly? stx)
       (syntax/loc stx (hash-set! h k v))]
      [{_ set! h k v} 
       (curly? stx)
       (syntax/loc stx (hash-set! h k v))]
      [{_ set*! h . more} 
       (curly? stx)
       (syntax/loc stx (hash-set*! h . more))]
      [{_ set h k v} 
       (curly? stx)
       (syntax/loc stx (hash-set h k v))]
      [{_ update! h k u} 
       (curly? stx)
       (syntax/loc stx (hash-update! h k u))]
      [{_ update! h k u f} 
       (curly? stx)
       (syntax/loc stx (hash-update! h k u f))]
      [{_ update h k u} 
       (curly? stx)
       (syntax/loc stx (hash-update h k u))]
      [{_ remove! h k} 
       (curly? stx)
       (syntax/loc stx (hash-remove! h k))]
      [{_ remove h k} 
       (curly? stx)
       (syntax/loc stx (hash-remove h k))]
      [{_ map h f} 
       (curly? stx)
       (syntax/loc stx (hash-map h f))]
      [{_ keys h} 
       (curly? stx)
       (syntax/loc stx (hash-keys h))]
      [{_ values h} 
       (curly? stx)
       (syntax/loc stx (hash-values h))]
      [{_ ->list h} 
       (curly? stx)
       (syntax/loc stx (hash->list h))]
      [{_ for-each h f} 
       (curly? stx)
       (syntax/loc stx (hash-for-each h f))]
      [{_ count h} 
       (curly? stx)
       (syntax/loc stx (hash-count h))]
      [{_ copy h} 
       (curly? stx)
       (syntax/loc stx (hash-copy h))]
      [{_ h k} 
       (curly? stx)
       (syntax/loc stx (hash-ref h k))]
      [{_ h k f} 
       (curly? stx)
       (syntax/loc stx (hash-ref h k f))]
      [(_ . more) 
       (syntax/loc stx (#%app . more))])))

(require 'terse-hash)
(provide (all-defined-out))
