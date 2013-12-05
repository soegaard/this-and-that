#lang racket
(require (for-syntax syntax/parse)
         syntax/strip-context)

(provide (rename-out [exported-parse parse]))

(define (exported-parse line col pos src-name str)
  (parse (lex line col pos str src-name)))

;;;
;;; LEXER
;;;

; In this experimental parser, the stream of tokens is represented
; as a list of Racket syntax objects.

; Numbers, symbols etc. are read by the Racket function read-syntax.
; The readtable is modified in order disallow +, -, ... in symbols.

(define operators+delimiters
  '(+ - * / ^ |(| |)| |[| |]| |,| ! < <= <> > >=))

;; A syntax object that has the "original?" property:
(define orig-stx (read-syntax #f (open-input-string "dummy")))

(define (lex line col pos 
             [port (current-input-port)] [src-name #'here])
  (when (string? port)
    (set! port (open-input-string port))) ; receives strings from read-between...
  (port-count-lines! port) 
  (set-port-next-location!	port line col pos)
  (parameterize 
      ([current-readtable
        (make-readtable 
         #f
         #\< 'terminating-macro lex<     ; < <= <> 
         #\> 'terminating-macro lex>     ; > >=
         #\= 'terminating-macro (λ _ '=)
         #\+ 'terminating-macro (λ _ '+)
         #\- 'terminating-macro (λ _ '-)
         #\* 'terminating-macro (λ _ '*)
         #\/ 'terminating-macro (λ _ '/)
         #\^ 'terminating-macro (λ _ '^)
         #\( 'terminating-macro (λ _ '|(|)
         #\) 'terminating-macro (λ _ '|)|)
         #\[ 'terminating-macro (λ _ '|[|)
         #\] 'terminating-macro (λ _ '|]|)
         #\{ 'terminating-macro (λ _ '|{|)
         #\} 'terminating-macro (λ _ '|}|)
         #\, 'terminating-macro (λ _ '|,|)
         #\! 'terminating-macro (λ _ '|!|))])
    (define (read-token port) (read-syntax src-name port))
    (define (has-underscore? str) (regexp-match #rx"_" str))
    (define (replace-underscore stx)
      (define sym (strip stx))
      (define str (and (symbol? sym) (symbol->string sym)))
      (cond [str (define str- (string-replace str "_" "-" #:all? #t))
                 (datum->syntax stx (string->symbol str-) stx orig-stx)]
            [else stx]))
    (for/list ([token (in-port read-token port)])
      (replace-context token 
                       (replace-underscore token)))))

(define lex< 
  (case-lambda
    [(ch port) (lex< ch port #f #f #f #f)]
    [(ch port src line col pos)
     (match (peek-char port)
       [#\=  (read-char port) 
             (define srcloc (vector src line col pos 2))
             (datum->syntax #f '<= srcloc orig-stx)]
       [#\>  (read-char port) 
             (define srcloc (vector src line col pos 2))
             (datum->syntax #f '<> srcloc orig-stx)]
       [else (define srcloc (vector src line col pos 1))
             (datum->syntax #f '< srcloc orig-stx)])]))

(define lex> 
  (case-lambda
    [(ch port) (lex> ch port #f #f #f #f)]
    [(ch port src line col pos)
     (match (peek-char port)
       [#\=  (read-char port) 
             (define srcloc (vector src line col pos 2))
             (datum->syntax #f '>= srcloc orig-stx)]
       [else (define srcloc (vector src line col pos 1))
             (datum->syntax #f '> srcloc orig-stx)])]))

; strip : token -> datum
(define (strip token)
  (if (syntax? token)
      (syntax->datum token)
      token))

;;;
;;; PRATT PARSER
;;;

; A Pratt parser is flexible and user-modifiable.

; References:
;   Beatiful Code p. 132 
;   ftp://ftp.cs.indiana.edu/pub/scheme-repository/code/lang/pratt.scm

; The Pratt parser works on a stream of tokens.

(define tokens 'defined-by-parse)

(define (operator/delimiter? token)
  (member (strip token) operators+delimiters))

(define (variable? token)
  (and (symbol? (strip token))
       (not (operator/delimiter? token))))

(define (literal? token)
  (not (or (operator/delimiter? token)
           (variable? token))))

; peek : -> token
;   return the next token if any otherwise return eof
(define (peek)
  (match tokens
    ['()        eof]
    [(cons t _) t]))

; pop : -> token
;   return the next token if any and advance the stream,
;   otherwise return eof
(define (pop)
  (match tokens
    ['()         eof]
    [(cons t ts) (set! tokens ts)
                 t]))

(define (skip stripped-token)
  (unless (equal? (strip (peek)) stripped-token)
    (error 'skip (~a "expected " stripped-token " got " (peek))))
  (pop))

; Each token has a pair of functions nud and led associated
; as well as a binding power.The parsing is controlled by 
; these functions. The names are abbreviations of
; "NUll Denomination" and "LEft Denomiation".

(define nuds (make-hash)) ; stripped-token -> nud 
(define leds (make-hash)) ; stripped-token -> led
(define bps  (make-hash)) ; stripped-token -> binding-power

; led : token -> led
;   return associated led,
;   the default is "error: missing operator"
(define (led t)
  (or (hash-ref leds (strip t) #f)
      (λ (left) (error (string->symbol (~a t ".led"))
                       "Missing operator"))))

; nud : token -> nud
;   return associated nud,
;   the default is "error: undefined"
(define (nud t)
  (cond 
    [(literal? t)  (λ() t)]
    [(variable? t) (λ() t)]
    [else
     (or (hash-ref nuds (strip t) #f)
         (λ () (error (string->symbol (~a t ".nud"))
                      "Undefined")))]))

; bp : token -> integer
;   returns the associated binding power (bp),
;   the default is bp is 0.
(define (bp t)
  (hash-ref bps (strip t) 0))

; parse-expr : integer -> expr
;  parse tokens in stream until a token with a higher 
;  binding power is met (note bp(+) < bp(*)).
(define (parse-expr rbp)
  (let loop ([left ((nud (pop)))])
    (if (> (bp (peek)) rbp)
        (loop ((led (pop)) left))
        left)))

; parse : tokens -> expr
;  setup environment for parse-expr
(define (parse ts)
  (set! tokens ts)
  (parse-expr 0))

; Before parsing can begin, we must associate
; parsing rules and binding powers with our tokens.

; (define-infix token op bp)
; will associate the token its binding power and 
; with its led:
;  led = (λ (left) (list op left (parse-expr bp)))
; the result is a left associative binary infix operator.

(define-syntax (define-infix stx)
  (syntax-parse stx
    [(_ token op bp)
     (syntax/loc stx
       (begin
         (hash-set! 
          leds token 
          (λ(left) #`(#,op #,left #,(parse-expr bp))))
         (hash-set! bps  token bp)))]))

; Right associtive operators need to decrement
; the binding power in the recursive call.
(define-syntax (define-infixr stx)
  (syntax-parse stx
    [(_ token op bp)
     (syntax/loc stx
       (begin
         (hash-set! 
          leds token 
          (λ(left) #`(#,op #,left #,(parse-expr (- bp 1)))))
         (hash-set! bps  token bp)))]))

(define-syntax (define-prefix stx)
  (syntax-parse stx
    [(_ token op nud)
     (syntax/loc stx 
       (hash-set! nuds token nud))]
    [(_ token op)
     (syntax/loc stx
       (define-prefix token op (λ() #`(#,op #,(parse-expr 80)))))]))


; The following infix operations are left associative.

; Application f[x,...] parses as (f x ...)
(hash-set! leds '|[| 
           (λ(left) 
             #`(#,left 
                #,@(let loop ([es (list (parse-expr 200))])
                     (match (strip (peek))
                       ['|,| (skip '|,|) 
                             (loop (cons (parse-expr 200) es))]
                       [else  (begin0 
                                (reverse es)
                                (skip '|]|))])))))
(hash-set! bps  '|[| 200)

; Prefix 
(define-prefix '- #'-)   ; 80
(define-prefix '! #'not) ; 80
; Grouping
(define-prefix '|(| '_ 
  (λ() (begin0 (parse-expr 0) (skip '|)|))))

; Infix
(define-infixr '^ #'expt 80)

(define-infix '*  #'*  70)
(define-infix '/  #'/  70)

(define-infix '+  #'+  60)
(define-infix '-  #'-  60)

(define-infix '=  #'=  50)
(define-infix '<  #'<  50)
(define-infix '>  #'>  50)

(define-infix '<= #'<= 50) 
(define-infix '>= #'>= 50) 
(define-infix '<> #'not= 50)  
(define (not= x y) (not (= x y)))

; (define-infixr && '&& 40)
; (define-infixr // '// 40)

#;(module* test #f
  (require rackunit)
  (define (test str) (syntax->datum (parse (lex str))))
  ; literals
  (check-equal? (test "1")    1)
  (check-equal? (test "1e3")  1000.0)
  (check-equal? (test "1.3")  1.3)
  (check-equal? (test "#f")   #f)
  (check-equal? (test "#t")   #t)
  (check-equal? (test "#\\a") #\a)
  (check-equal? (test "'foo") ''foo)
  ; variables
  (check-equal? (test "x")     'x)
  (check-equal? (test "x2")    'x2)
  (check-equal? (test "x2.0")  'x2.0)
  ; (check-equal? (test "x_y")   'x-y)
  ; arithmetic  
  (check-equal? (test "1+2")   '(+ 1 2))
  (check-equal? (test "1+2+3") '(+ (+ 1 2) 3))
  (check-equal? (test "1*2")   '(* 1 2))
  (check-equal? (test "1*2*3") '(* (* 1 2) 3))
  (check-equal? (test "1^2")   '(expt 1 2))
  (check-equal? (test "1^2^3") '(expt 1 (expt 2 3)))  
  (check-equal? (test "1+2*3") '(+ 1 (* 2 3)))
  ; grouping
  (check-equal? (test "(1+2)*3") '(* (+ 1 2) 3))
  ; unary
  (check-equal? (test "-1")    '(- 1))
  (check-equal? (test "---1")  '(- (- (- 1))))
  (check-equal? (test "!#t")   '(not #t))
  ; application  
  (check-equal? (test "sin[x]")    '(sin x))
  (check-equal? (test "list[x,y]") '(list x y))
  ; comparisions
  (check-equal? (test "x=y")    '(= x y))
  (check-equal? (test "x<y")    '(< x y))
  (check-equal? (test "x>y")    '(> x y))
  (check-equal? (test "x<=y")   '(<= x y))
  (check-equal? (test "x>=y")   '(>= x y))
  (check-equal? (test "x<>y")   '(not= x y))
  (check-equal? (test "!(x=y)") '(not (= x y)))  
  ; lexical information
  (check-equal? (eval (first (lex "1"))) 1)
  (check-equal? (eval (parse (lex "1+2*3" #'src))) 7))

