#lang racket
; This module exports read and read-syntax that
; behaves almost like standard read and read-syntax.
; The exceptions are given by the reader spec lines:
; #lang readtable
; reader-spec
; ...
; #lang ...
;
; Here reader-spec is one of
;   char1 char2 module-path identifier
;   char        module-path identifier
; 

(require syntax/readerr 
         "converter.rkt"
         "parse-reader-spec.rkt")

(provide make-modified-readtable)

(define (make-read-between spec)
  (define (maybe-escape char)
    ; characters that are part of the regexp syntax 
    ; need special attention
    (define quoted (regexp-quote (string char)))
    (if (equal? quoted (string char))
        quoted
        (string-append "\\" quoted)))  
  (define reg (pregexp ; matches anything until first stop-key
               (string-append 
                "([^\\" (maybe-escape (spec-stop-key spec)) "]*)")))
  (define (read-between src in)
    (define-values (line col pos) (port-next-location in))
    (define convert (spec-convert spec))
    (define str (bytes->string/utf-8 (first (regexp-match reg in))))
    (if (procedure-arity-includes? convert 5)
        (convert line col pos src str)
        (convert line col pos str)))
  read-between)

(define (make-action spec)
  (define read-between (make-read-between spec))
  (define stop (spec-stop-key spec))
  (case-lambda
    [(ch in) ; triggered by read or read-recursive
     (check-after 
      stop (read-between (object-name in) in) in (object-name in))]
    [(ch in src-name line col pos) ; triggered by read-syntax
     (check-after 
      stop (read-between src-name in) in src-name)]))

(define (make-modified-readtable specs)
  (define args
    (append*
     (for/list ([s specs])
       (list (spec-key s) (spec-mode s) (make-action s)))))
  (apply make-readtable (current-readtable) args))
  
(define (check-after stop val in src)
  ; (regexp-match #px"^\\s*" in) ; skip whitespace
  (let ([ch (peek-char in)])
    (unless (equal? ch stop) (bad-ending stop ch src in))
    (read-char in))
  val)

(define (bad-ending stop ch src in)
  (let-values ([(line col pos) (port-next-location in)])
    ((if (eof-object? ch)
         raise-read-error
         raise-read-eof-error)
     (~a "expected a closing: " stop)
     src line col pos
     (if (eof-object? ch) 0 1))))
