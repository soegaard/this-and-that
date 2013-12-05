#lang racket
(require (for-syntax syntax/parse))
;;;
;;; Pratt Parser
;;;

; A Pratt parser is flexible and user-modifiable.

; References:
;   Beatiful Code p. 132 
;   ftp://ftp.cs.indiana.edu/pub/scheme-repository/code/lang/pratt.scm

; The Pratt parser works on a stream of tokens.
; In this experimental parser, the stream of tokens is represented
; as a list of Racket symbols.

(define tokens '(1 * 2 + 3 * 4 * 5))

; Numbers are the only literals in this example.

(define (literal? token)
  (number? token))

; Variable names are represented as strings.

(define (variable? token)
  (string? token))

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

; Each token has a pair of functions nud and led associated
; as well as a binding power.The parsing is controlled by 
; these functions. The names are abbreviations of
; "NUll Denomination" and "LEft Denomiation".

(define nuds (make-hash)) ; token -> nud 
(define leds (make-hash)) ; token -> led
(define bps  (make-hash)) ; token -> binding power

; led : token -> led
;   return associated led,
;   the default is "error: missing operator"
(define (led t)
  (define led (hash-ref leds t #f))
  (or led
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
     (or (hash-ref nuds t #f)
         (λ () (error (string->symbol (~a t ".nud"))
                      "Undefined")))]))

; bp : token -> integer
;   returns the associated binding power (bp),
;   the default is bp is 0.
(define (bp t)
  (hash-ref bps t 0))

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

(define-syntax (define-infix stx)
  (syntax-parse stx
    [(_ token op bp)
     (syntax/loc stx
       (begin
         (hash-set! leds 'token (λ(left) (list op left (parse-expr bp))))
         (hash-set! bps 'token bp)))]))

; The following infix operations are left associative.

(define-infix + '+ 60)
(define-infix - '- 60)
(define-infix * '* 70)
(define-infix / '/ 70)

(define-infix =  '=  50)
(define-infix <  '<  50)
(define-infix >  '>  50)
(define-infix <= '<= 50)
(define-infix >= '>= 50)

; Right associtive operators need to decrement
; the binding power in the recursive call.
(define-syntax (define-infixr stx)
  (syntax-parse stx
    [(_ token op bp)
     (syntax/loc stx
       (begin
         (hash-set! leds 'token (λ(left) (list op left (parse-expr (- bp 1)))))
         (hash-set! bps 'token bp)))]))

(define-infixr ^  '^ 80)
(define-infixr && '&& 40)
(define-infixr // '// 40)

(hash-set! leds 'LP (λ(left) (list left (parse-expr 200))))
(hash-set! bps  'LP 200)
(hash-set! leds 'RP (λ(left) left))
(hash-set! bps  'RP 200)


(parse '(1 + 2 * "sin" LP "x" RP LP "y" RP + 3 * "x" ^ 4))



