#lang racket
(provide parse)

; TODO: Write a real parser.
;       Done! See "pratt.rkt"
; This one does not support unary minus, parentheses, applications, etc.

#;(define (parse src str)
    (datum->syntax #f 'x (vector src #f #f #f #f)))

(define (parse-sum src str)
  (define exprs (regexp-split "\\+" str))
  (if (> (length exprs) 1)
      (datum->syntax 
       #f (cons #'+ (map (λ (e) (parse-minus src e)) exprs)))
      (parse-minus src str)))

(define (parse-minus src str)
  (define str2 (regexp-replace "->" str "+ARROW+"))  ; TODO PRESERVE ARROWS
  (define exprs2 (regexp-split "\\-" str2))
  (define exprs (map (λ (s) (regexp-replace "\\+ARROW\\+" s "->")) exprs2))
  (if (> (length exprs) 1)
      (datum->syntax 
       #f (cons #'- (map (λ (e) (parse-mult src e)) exprs)))
      (parse-mult src str)))

(define (parse-mult src str)
  (define exprs (regexp-split "\\*" str))
  (if (> (length exprs) 1)
      (datum->syntax 
       #f (cons #'* (map (λ (e) (parse-div src e)) exprs)))
      (parse-div src str)))

(define (parse-div src str)
  (define exprs (regexp-split "\\/" str))
  (if (> (length exprs) 1)
      (datum->syntax 
       #f (cons #'/ (map (λ (e) (parse-atom src e)) exprs)))
      (parse-atom src str)))

(define (parse-atom src str)
  (define str2 (regexp-replace "_" str "-"))
  (read-syntax src (open-input-string str2)))

         
(define (parse src str)
  (displayln (list "parsing: " str))
  (define result (parse-sum src str))
  (displayln result)
  result)

