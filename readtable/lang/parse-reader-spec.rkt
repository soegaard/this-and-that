#lang racket
(provide parse-spec-line
         (struct-out spec))

(struct spec (key stop-key mode convert) #:transparent)

(define (parse-spec-line line)
  (with-input-from-string line
   (λ()
     (define key       (read-char))
     (define stop-key  (read-char))
     (define mode      (read))
     (define mod-path  (read))
     (define parser-id (read))
     (define more      (read-line))
     (define convert   (dynamic-require mod-path parser-id))
     (spec key stop-key mode convert))))
