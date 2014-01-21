#lang racket
;;;
;;; Updatable Structs
;;;

;;; This module defined updatable-struct, which works
;;; as struct but also defines an updater.

;;; This experimental version only supports the syntax
;;;     (updatable-struct name (field ...))
;;; but that ought to be easy to fix.

;;; Example

;; > (define a-foo (foo 1 2))
;; > (update-foo a-foo [bar 41])
;; (foo 41 2)
;; > (update-foo a-foo [baz 42])
;; (foo 1 42)
;; > (update-foo a-foo [bar 41] [baz 42])
;; (foo 41 42)
;; > (update-foo a-foo [baz 42] [bar 41])
;; (foo 41 42)

(require (for-syntax syntax/parse racket/syntax))

(begin-for-syntax
  (define-splicing-syntax-class maybe-super
    (pattern (~seq super:id))
    (pattern (~seq)))
  
  (define-syntax-class field-option
    (pattern #:mutable)
    (pattern #:auto))
  
  (define-syntax-class field
    (pattern field:id #:with (option ...) '())
    (pattern [field:id option:field-option ...])))

(define-syntax (updatable-struct stx)
  (syntax-parse stx
    [(_ name:id (field:field ...))
     (let ([fields (syntax->list #'(field ...))])
       (define (fields-but field) (for/list ([f fields] #:unless (equal? f field)) f))
       (define (accessor-id field) (format-id stx "~a-~a" #'name field))
       (with-syntax 
           ([updater        (format-id stx "update-~a" #'name)]
            [(accessor ...) (map accessor-id fields)])
         (syntax/loc stx
           (begin
             (struct name (field ...) #:transparent)
             (define-syntax (updater so)
               (define field-names (list #'field ...))
               (define fields (syntax->list #'(field ...)))
               (syntax-parse so
                 [(_ se [fid val] (... ...))
                  (define fields-to-update  (syntax->datum #'(fid (... ...))))
                  (define value-expressions (syntax->datum #'(val (... ...))))
                  (define fid-alist (map cons fields-to-update value-expressions))
                  (with-syntax 
                      ([(new (... ...)) (for/list ([f fields] [a (syntax->list #'(accessor ...))])
                                          (cond [(assoc (syntax->datum f) fid-alist) => cdr]
                                                [else (with-syntax ([acc a])
                                                        #'(acc a-name))]))])
                    (syntax/loc so
                      (let ([a-name se])
                        (name new (... ...)))))]))))))]))

(module+ test (require rackunit)
  (updatable-struct foo (bar baz))
  (define a-foo (foo 1 2))
  (check-equal? (update-foo a-foo)                   (foo  1  2))
  (check-equal? (update-foo a-foo [bar 41])          (foo 41  2))
  (check-equal? (update-foo a-foo          [baz 42]) (foo  1 42))
  (check-equal? (update-foo a-foo [bar 41] [baz 42]) (foo 41 42)))

