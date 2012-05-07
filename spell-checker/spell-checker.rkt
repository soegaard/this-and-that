#lang racket
;;; Jens Axel Søgaard - May 4th 2012

; This file implements a small spell checker. 
; The training dictionary is in "big.txt".
;
; How the algorithm behind works is described by Peter Norvig in
;     http://www.norvig.com/spell-correct.html
;
; The original Scheme implementation is described in the following blog post:
;     http://blog.scheme.dk/2007/04/writing-spelling-corrector-in-plt.html
;
; This version uses Racket's builtin sets and comprehensions (not available back then).
; The inspiration of updating the spell checker is due to this post by in StackOverflow
;     http://stackoverflow.com/questions/10449052/


; words : port -> list-of-strings
(define (words text)
  (for/list ([word (regexp-split #rx"[^a-zA-Z]" text)])
    (string-downcase (bytes->string/latin-1 word))))

; train : list-of-strings -> hash-table-from-strings-to-numbers
(define (train features)
  (let ([model (make-hash)])
    (for ([word features])
      (hash-update! model word add1 1))
    model))

(define NWORDS (train (words (open-input-file "big.txt"))))

(define (word-count word)
  (hash-ref NWORDS word 0))

(define (known? word)
  (hash-ref NWORDS word #f))

(define (known words)
  (for/set ([w words] #:when (known? w)) w))

(define (concat-it spec)
  (define (underscore? o) (eq? o '_))
  ; spec :: = '()  | ( string num num  . spec ) | (string num _) | |
  (define (subs spec)
    (match spec
      [(list) '()]
      [(list-rest (? string? s) (? number? n1) (? number? n2) spec)
       (cons (substring s n1 n2) (subs spec))]
      [(list-rest (? string? s) (? number? n1) (? underscore? _) spec)
       (cons (substring s n1 (string-length s)) (subs spec))]
      [(list-rest (? string? s) (? underscore? _) (? number? n2) spec)
       (cons (substring s 0 n2) (subs spec))]
      [(list-rest (? string? s) (? number? n) spec)
       (cons (string (string-ref s n)) (subs spec))]
      [(list-rest (? symbol? s) spec)
       (cons (symbol->string s) (subs spec))]
      [(list-rest (? char? c) spec)
       (cons (string c) (subs spec))]
      [(list-rest (? string? s) spec)
       (cons s (subs spec))]
      [else (error)]))
  (apply string-append (subs spec)))

(define-syntax (concat stx)
  (syntax-case stx (_)
    [(string-it spec ...)
     #`(concat-it
        (list #,@(map (lambda (so)
                        (syntax-case so (_)
                         [_ #''_]
                         [else so]))
                     (syntax->list #'(spec ...)))))]))

(define (edits1 word)
  (define alphabet "abcdefghijklmnopqrstuvwxyz")
  (define n (string-length word))
  (define deletes    (for/set  ([i n])                    (concat word 0 i     word (+ i 1) _)))
  (define transposes (for/set  ([i (- n 1)])              (concat word 0 i     word (+ i 1)  word i  word (+ i 2) _)))
  (define inserts    (for*/set ([i (+ n 1)] [c alphabet]) (concat word 0 i  c  word i _)))
  (define replaces   (for*/set ([i n] [c alphabet])       (concat word 0 i  c  word (+ i 1) _)))
  (set-union deletes transposes replaces inserts))

(define (known-edits2 word)
  (for*/set ([e1 (edits1 word)] [e2 (edits1 e1)] #:when (known? e2)) e2))

(define (correct word)
  (define (falsify s) (if (set-empty? s) #f s))
  (let ([candidates 
         (or (falsify (known (set word)))
             (falsify (known (edits1 word)))
             (falsify (known-edits2 word))
             (falsify (list->set (list word))))])
    (and (not (set-empty? candidates))
         (argmax word-count (set->list candidates)))))
