#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (for-syntax syntax/parse
                     racket/syntax))

(define-lex-abbrevs 
  [exactness           (:or "#e" "#i")]  
  [number10            (:or exact10 inexact10)]
  [exact10             (:or exact-integer10 exact-rational10 exact-complex10)]
  [exact-integer10     (:seq (:? sign) unsigned-integer10)]
  [unsigned-integer10  (:+ digit10)]
  [exact-rational10    (:seq (:? sign) unsigned-rational10)]
  [unsigned-rational10 (:seq unsigned-integer10 "/" unsigned-integer10)]
  [exact-complex10     (:seq exact-rational10 sign unsigned-rational10 "i")]
  [inexact10           (:or inexact-real10 inexact-complex10)]
  [inexact-real10      (:or (:seq (:? sign) inexact-normal10) 
                            (:seq sign inexact-special10))]
  [inexact-unsigned10  (:or inexact-normal10 inexact-special10)]
  [inexact-normal10    (:seq inexact-simple10 (:? (:seq exp-mark10 exact-integer10)))]
  [inexact-simple10    (:or (:seq digits-hash-10 (:? ".") (:* "#"))
                            (:seq (:? unsigned-integer10) "." digits-hash-10) 
                            (:seq digits-hash-10 "/" digits-hash-10))]
  [inexact-special10   (:or "inf.0" "nan.0" "inf.f" "nan.f")]
  [digits-hash-10      (:seq (:+ digit10) (:* "#"))]
  [inexact-complex10   (:or (:seq (:? inexact-real10) sign inexact-unsigned10 "i")
                            (:seq inexact-real10 "@" inexact-real10))]
  [sign                (:or "+" "-")]
  [digit16             (:or digit10 (char-range #\a #\f))]
  [digit10             (char-range #\0 #\9)]
  [digit8              (char-range #\0 #\7)]
  [digit2              (:or #\0 #\1)]
  [exp-mark16          (:or "s" "l")]
  [exp-mark10          (:or exp-mark16 (char-range #\d #\f))]
  [exp-mark8           exp-mark10]
  [exp-mark2           exp-mark10]
  [general-number      (:seq (:? exactness) number10)])

(define-syntax (define-reader stx)
  (define (make-read-id id)
    (format-id id "read-~a" (syntax-e id)))
  (syntax-parse stx
    [(_ id:identifier lexeme->value token:identifier)
     (with-syntax ([read-id (make-read-id #'id)])
       (syntax/loc stx
         (begin
           (provide read-id)
           (define read-id
             (lexer
              [token (lexeme->value lexeme)]
              [(eof) eof])))))]))

(define-syntax (define-readers stx)
  (syntax-parse stx
    [(_ (id:identifier ...) lexeme->value)
     (syntax/loc stx
       (begin
         (define-reader id lexeme->value id)
         ...))]))

(define-readers 
  (exactness 
   digits-hash-10
   sign
   digit16 digit10 digit8 digit2
   exp-mark16 exp-mark10 exp-mark8 exp-mark2)
  values)

(define-readers (number10 
                 unsigned-integer10
                 unsigned-rational10
                 exact10
                 exact-integer10
                 exact-rational10
                 exact-complex10)
  string->number)

(define (string->inexact s)
  (define x (string->number s))
  (cond
    [(not x)      x]
    [(inexact? x) x]
    [(exact? x)   (exact->inexact x)]
    [else         x]))
      
(define-readers (inexact10
                 inexact-real10
                 inexact-unsigned10
                 inexact-normal10
                 inexact-simple10
                 inexact-special10
                 inexact-complex10)
  string->inexact)

(define-reader number string->number general-number)

(define (test reader str)
  (reader (open-input-string str)))

(test read-number    "")
(test read-inexact10 "3.0")
(test read-inexact10 "3")
(test read-inexact10 "")
(test read-digit16   "f")
(test read-exactness "#i")

(define ip (open-input-string "-35##--4e3"))
(read-number ip)
(read-char ip)
(read-number ip)
