;; jas-match.scm  --  Jens Axel Søgaard  -- 13/14 dec 2003

;;; PURPOSE

; This is a very naïve implementation of a subset of
; the pattern matcher plt-match.ss, which builds upon
; Wright's pattern matcher, but uses constructor notation
; in the patterns. 

; The idea was too see, how far I could get using nothing
; but syntax-rules. 

;;; INSTRUCTIONS OF USE

; The user macros are
;    (match expr (pattern <guard> expr) ...)    , <guard> can be omitted
;    (match-lambda (pattern expr ...) ...)
;    (match-let ((pattern expr) ...) expr ...)
;    (match-let* ((pattern expr) ...) expr ...)

; The syntax of patterns are a subset of the one in:
;     <http://download.plt-scheme.org/scheme/plt-clean-cvs/collects/mzlib/plt-match.ss>

; The semantics of the match functions are explained in
;     <http://download.plt-scheme.org/scheme/docs/html/mzlib/mzlib-Z-H-22.html#node_chap_22>

; Notably features missing:
;   - quasi-patterns
;   - set! and get!
;   - match-define and match-letrec
;   - the ooo and ook extension in list and vector patterns
;   - structures (easily added but they are non portable)

;;; IMPLEMENTATION

; The implementation is divided into layers, each layer
; handles one aspect of the pattern matching process.

; The main macro from the user perspective is the match macro.
;   (match expr [(pattern expr ...) ...])
; which binds the value to be matched to a variable, and leaves
; the real work to guarded match. Match also handles the case
; of multple patterns.

; The macro guarded-match 
;    (guarded-match var pattern success failure)
; expands to success if the value bound to var matches the pattern,
; otherwise it expands to failure.
; Guarded-match takes care of guards and then macro calls logical-match.

; The macro logical-match 
;    (logical-match var pattern success failure)
; expands to success if the value bound to var matches the pattern,
; otherwise it expands to failure.
; Logical-match takes care of patterns of the form
;   (and pattern ...)
;   (or  pattern ...)
;   (not pattern pattern ...)
;   (?   expr pattern ...)
; and then macro calls compound-match.

; The macro compound-match 
;    (compound-match var pattern success failure)
; expands to success if the value bound to var matches the pattern,
; otherwise it expands to failure.
; Compound-match takes care of patterns of the form
;   (cons pattern pattern)
;   (list pattern ...)
;   (list-rest pattern ... pattern)
;   (vector pattern pattern ...)
;   (app expr pattern)
; and then macro calls simple-match.

; The macro simple-match 
;    (simple-match var pattern success failure)
; expands to success if the value bound to var matches the pattern,
; otherwise it expands to failure.
; Simple-match takes care of patterns of the form
;   (quote symbol)
;   (quote datum)
;   pattern-var
;   literal
; and possible macro calls literal-match.

; The macro literal-match 
;    (literal-match var pattern success failure)
; expands to success if the value bound to var matches the pattern,
; otherwise it expands to failure.
; Literal-match takes care of patterns of atoms of the form
;   the empty list
;   booleans
;   strings
;   numbers
;   characters
; and compound literals.

(define-syntax symbol??
  ;; From Oleg's "How to write symbol? with syntax-rules.
  ;; <http://okmij.org/ftp/Scheme/macro-symbol-p.txt>
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)	; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)	; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
     (let-syntax
         ((test
	   (syntax-rules ()
	     ((test maybe-symbol t f) t)
	     ((test x t f) f))))
       (test abracadabra kt kf)))))

(define-syntax id-eq??
  ;; From Oleg's "How to write symbol? with syntax-rules.
  ;; <http://okmij.org/ftp/Scheme/macro-symbol-p.txt>
  (syntax-rules ()
    ((id-eq?? id b kt kf)
     (let-syntax
         ((id (syntax-rules ()
                ((id) kf)))
          (ok (syntax-rules ()
                ((ok) kt))))
       (let-syntax
           ((test (syntax-rules ()
                    ((_ b) (id)))))
         (test ok))))))

(define (literal? datum)
  (or (string? datum)
      (number? datum)
      (char? datum)
      (null? datum)
      (boolean? datum)))


(define-syntax literal-match
  (syntax-rules ()
    [(_ var () success failure)        (if (null? var)  success failure)]
    [(_ var #t success failure)        (if (eq? var #t) success failure)]
    [(_ var #f success failure)        (if (eq? var #f) success failure)]
    [(_ var literal success failure)   (if (and (literal? var)
                                               (equal? var literal))
                                          success
                                          failure)]))

(define-syntax simple-match
  ; (simple-match var pattern success failure)
  ;     If the value bound to var matches pattern then the 
  ;     expression expands into a let binding the pattern variables
  ;     in the pattern to the matched (sub)values, success becomes the
  ;     body of the let. Otherwise the macro call expands to failure.
  (syntax-rules (quote)
    [(_ var (quote symbol/datum)     success failure)  (if ((symbol?? symbol/datum eq? equal?) var 'symbol/datum)
                                                          success 
                                                          failure)]
    [(_ var name/literal             success failure)  (symbol?? name/literal
                                                                ; pattern variable
                                                                (let ([name/literal var])
                                                                  success)
                                                                ; literal
                                                                (literal-match var name/literal success failure))]))

(define-syntax compound-match 
  (syntax-rules (cons list list-rest app vector)
    [(_ var (cons p1 p2)            success failure)   (if (pair? var)
                                                          (match (car var)
                                                            [p1 (match (cdr var) 
                                                                  [p2 success] 
                                                                  [_ failure])]
                                                            [_  failure])
                                                          failure)]
    
    ; Note: Patterns with ooo is handles in super-match
    [(_ var (list)                   success failure)  (compound-match var () success failure)]
    [(_ var (list p1)                success failure)  (compound-match var (cons p1 ()) success failure)]
    [(_ var (list p1 p2 ...)         success failure)  (compound-match var (cons p1 (list p2 ...)) success failure)]
    
    [(_ var (vector p1 ...)          success failure)  (let ([vector-var (vector->list var)])
                                                        (compound-match vector-var (list p1 ...) success failure))]
    
    [(_ var (list-rest p1 p2)        success failure)  (compound-match var (cons p1 p2) success failure)]
    [(_ var (list-rest p1 p2 p3 ...) success failure)  (compound-match var (cons p1 (list-rest p2 p3 ...)) success failure)]
    
    [(_ var (app expr p1)            success failure)  (let ([new-var (expr var)])
                                                        (match new-var p1 success failure))]
    [(_ var pattern          success failure)  (simple-match var pattern success failure)]))


(define-syntax logical-match 
  (syntax-rules (and or not ?)
    [(_ var (and)            success failure)  success]
    [(_ var (and p1)         success failure)  (compound-match var p1 success failure)]
    [(_ var (and p1 p2 ...)  success failure)  (compound-match var p1 
                                                              (logical-match var (and p2 ...) success failure)
                                                              failure)]

    [(_ var (or p1)          success failure)  (compound-match var p1 success failure)]
    [(_ var (or p1 p2 ...)   success failure)  (compound-match var p1 success 
                                                              (logical-match var (or p2 ...) success failure))]
    
    [(_ var (not p)          success failure)  (logical-match var p failure success)]
    [(_ var (not p1 p2 ...)  success failure)  (logical-match var (and (not p1) (not p2) ...) failure success)]
    
    [(_ var (? expr p ...)   success failure)  (if expr
                                                  (logical-match var (and p ...) success failure)
                                                  failure)]

    [(_ var pattern          success failure)  (compound-match var pattern success failure)]))


(define-syntax guarded-match
  (syntax-rules ()
    [(_ var pattern success failure)          (logical-match var pattern success failure)]
    [(_ var pattern guard success failure)    (guarded-match var pattern (if guard success failure) failure)]))


(define-syntax match
  (syntax-rules ()
    [(_ expr)                                  (let ([v expr])
                                                 'no-match)]
    [(_ expr [pattern template]
             clauses ...)                      (let ([v expr])
                                                 (guarded-match v pattern
                                                                template
                                                                (match v clauses ...)))]
    [(_ expr [pattern guard template]
             clauses ...)                      (let ([v expr])
                                                 (guarded-match v pattern guard
                                                                template
                                                                (match v clauses ...)))]))

(define-syntax match-lambda
  (syntax-rules ()
    [(_ (pat expr ...) ...)         (lambda (x) (match x (pat expr ...) ...))]))

(define-syntax match-lambda*
  (syntax-rules ()
    [(_ (pat expr ...) ...)         (lambda x   (match x (pat expr ...) ...))]))

(define-syntax match-let*
  (syntax-rules ()
    [(_ () body ...)                                (begin body ...)]
    [(_ ((pat expr)) body ...)                      ((match-lambda (pat body ...)) expr)]
    [(_ ((pat expr) (pat2 expr2) ...) body ...)     (match-let* ([pat expr])
                                                      (match-let* 
                                                          ((pat2 expr2) ...) 
                                                        body ...))]))

(define-syntax match-let 
  (syntax-rules ()
    [(_ () body ...)               (begin body ...)]
    [(_ ((pat expr) ...) body ...) (match-let* ([(list pat ...) (list expr ...)]) body ...)]))


;; Test

(define-syntax test-simple 
  (syntax-rules ()
    [(_ value pattern success failure) (let ([test-simple-var value])
                                        (simple-match test-simple-var pattern success failure))]))
'SIMPLE
(test-simple '() () 'ok 'fail)
(test-simple 1 1 'ok 'fail)
(test-simple 1 2 'fail 'ok)
(test-simple 'foo 'foo 'ok 'fail)
(test-simple 'foo 'bar 'fail 'ok)

(define-syntax test-compound 
  (syntax-rules ()
    [(_ value pattern success failure) (let ([test-compund-var value])
                                        (compound-match test-compund-var pattern success failure))]))
'COMPOUND
(test-compound (cons 1 "foo") (cons 1 "foo") 'ok 'fail)
(test-compound (cons 1 2) (cons a b) (if (= a 1) 'ok 'fail1) 'fail2)
(test-compound (list 1 2 3) (list a b c) (if (= (+ a b c) 6) 'ok 'fail1) 'fail2)
(test-compound (vector 1 2 3) (vector a b c) (if (= (+ a b c) 6) 'ok 'fail1) 'fail2)


(define-syntax test-logical 
  (syntax-rules ()
    [(_ value pattern success failure) (let ([test-logical-var value])
                                        (logical-match test-logical-var pattern success failure))]))

'LOGICAL
(test-logical (cons 1 2) 
              (and (cons a b) (cons 1 c) (cons d 2))
              (if (equal? (list a b c d)
                          (list 1 2 2 1))
                  'ok
                  'fail1)
              'fail2)
(test-logical (cons 1 2) 
              (or 1 "foo" (cons 1 3) (cons 1 2) #\c)
              'ok
              'fail)
(test-logical (cons 1 2)
              (not (cons a b))
              'fail
              'ok)
(test-logical (cons 1 2)
              (not (cons a b))
              'fail
              'ok)
(test-logical (cons 1 2)
              (not 1 2 "foo" (cons 3 4))
              'ok
              'fail)
'GUARDED
(guarded-match (cons 42 2) (cons a b) (even? a) 'ok   'fail)
(guarded-match (cons 43 2) (cons a b) (even? a) 'fail 'ok)

'FULL
(match (cons 1 2)
           [()         'empty]
           [(cons 1 b) (if (= b 2) 'ok 'fail)])

(match (cons 1 (cons 2 3))
            [()         'empty]
            [(cons 1 (cons 2 b)) (if (= b 3) 'ok 'fail)])

(match 'foo
            ['foo 'ok]
            [else 'fail])
(match 'foo
            ['bar 'fail]
            [else 'ok])


'MATCH-LET*
(match-let* ([(list x y z)    (list 1 2 3)]
             [(vector a b c)  (vector 4 5 6)])
  (if (= (+ x y z a b c) 21)
      'ok
      'fail))

(match-let* ([(list x y)    (list 1 2)]
            [(vector a b)   (vector 3 x)])
  (if (= (+ x y a b) 7)
      'ok
      'fail))

'MATCH-LET
(match-let ([(list x y z)    (list 1 2 3)]
            [(vector a b c)  (vector 4 5 6)])
  (if (= (+ x y z a b c) 21)
      'ok
      'fail))
