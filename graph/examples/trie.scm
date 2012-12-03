;;; trie.scm  --  Jens Axel Søgaard

(require (lib "42.ss" "srfi")
         (planet "evector.scm" ("soegaard" "evector.plt")))

; A quick implementation of tries inspired by page 163-165 in 
; Chris Okasaki's "Purely Functional Data Structures".

; A trie is a finite map which maps keys in form of lists over
; a base type to values. 

; We represent as a triple:

(define-struct trie (end value map) (make-inspector))

; where 
;   end    is a boolean
;   value  is of the base type
;   map    is a finite map from the base type to tries

; A trie containing "ca", "car" and "o" can be drawn as
; a tree, where end and value is drawn at the nodes, and
; the map contains the labels of the subtrees.

;         #f,-
;          /\
;      c  /  \ o
;        /    \
;      #f,-  #t,3
;      /      
;   a /
;    /
;  #t,1
;   |
; r |
;   |
;  #t,2  

; End indicates that a certain key sequence has an associated value.
; In the above trie, the sequence (list #\c) has no associated value,
; where as (list #\c #\a) is associated to a 1.

; Here is one way to represent the finite map:

#;
(
(require (prefix fm: (planet "finite-map.scm" ("soegaard" "galore.plt"))))

(define (fm:lookup k m)
  (cond
    [(fm:get k m) => cdr]
    [else            #f]))

(define (fm:bind k v m)
  (fm:insert k v m))
)

; and here is another:

(begin
(define (fm:empty)
  '())
(define (fm:empty? m)
  (null? m))
(define (fm:lookup k m)
  (cond
    [(assoc k m) => cdr]
    [else           #f]))
(define (fm:bind k v m)
  (cons (cons k v) m))
)


; empty : -> trie
;   return an empty trie
(define empty
  (let ([e (make-trie #f #f (fm:empty))])
    (lambda () e)))

; empty? : trie -> boolean
;   determine whether the trie is empty
(define (empty? t)
  (and (not (trie-end t))
       (fm:empty? (trie-map t))))

(define (handle-not-found t)
  (if (eq? t #f)
      (empty)
      t))

; bind : (list base) object trie -> trie
;   extend the trie t with a binding of the key 
;   sequence ks to the value x
(define (bind ks x t)
  (cond
    [(null? ks) 
     (make-trie #t x (trie-map t))]
    [else
     (let ([k  (car ks)]
           [ks (cdr ks)]
           [m  (trie-map t)]
           [v  (trie-value t)]
           [e  (trie-end t)])
       (let* ([t  (handle-not-found (fm:lookup k m))]
              [t1 (bind ks x t)])
         (make-trie e v (fm:bind k t1 m))))]))

; lookup : (list base) trie -> (union value #f)
;   return either the value associated to the key sequence ks,
;   or return #f if no association is found
(define (lookup ks t)
  (cond
    [(and (null? ks) (not (trie-end t)))
     #f] ; not found
    [(null? ks)
     (trie-value t)]
    [else
     (lookup (cdr ks) 
             (handle-not-found (fm:lookup (car ks) (trie-map t))))]))



;;; TEST

(define (insert-word w v t)
  (bind (string->list w) v t))
 
(define (lookup-word w t)
  (lookup (string->list w) t))

(define (chop s)
  (substring s 0 (sub1 (string-length s))))

(define (legal? w)
  (and (= (string-length w) 3)
       (every?-ec (:string c w)
                  (char-alphabetic? c))))

(define (insert-word-if-legal w t)
  (let ([w (string-downcase w)])
    (if (legal? w)
        (insert-word w #t t)
        t)))

(require "../integer-graphs-with-edges.scm")
;(load "../integer-graph.scm")

(display "Building dictionary...\n")

(define words (evector))     ; word index to string
(define dictionary (empty))  ; trie: word to index

(define (index->word n)
  (evector-ref words n))

(let* ([ip (open-input-file "dict/english.0")])
  (do-ec (:port line ip read-line)
         (:let w (string-downcase (chop line)))
         (if (legal? w))
         (begin
           (set! dictionary (insert-word w (evector-length words) dictionary))
           (evector-push! words w)))
  (close-input-port ip))

(display (format "Words in dictionary: ~a\n" (evector-length words)))


(display "Building graph...\n")

(define word-graph (make-graph #f))

(define (insert-word-in-graph w)
  (let ([wi (lookup-word w dictionary)])
    (do-ec (: _ (index i) w)
           (do-ec (: c "abcdefghijklmnopqrstuvwxy")
                  (:let w2 (string-copy w))
                  (begin
                    (string-set! w2 i c)
                    (let ([w2i (lookup-word w2 dictionary)])
                      (if w2i
                          (add-edge word-graph wi w2i))))))))

(do-ec (: i (evector-length words))
       (insert-word-in-graph (evector-ref words i)))


(display "Computing connected components...\n")

(define components
  (let* ([cc          (connected-components word-graph)]
         [components  (make-evector 0 '())])
    (set-evector-length! components (add1 (max-ec (: c cc) c)))
    (do-ec (: c (index i) cc)
           (evector-set! components c
                         (cons i
                               (evector-ref components c))))
    (vector->list (evector->vector components))))

(display (map (lambda (component)
                (map index->word component))
              components))
(newline)

(display "Computing spanning forrest...\n")
(define forrest (spanning-forrest word-graph))


(require (prefix is: (planet "set.scm"  ("soegaard" "galore.plt")))) 

(define (spanning-tree->longest-subpath t)
  ; t is a list of edges in form of conses of nodes
  (let ([nodes (foldl (lambda (e ns) (is:insert* (list (car e) (cadr e)) ns))
                      (is:empty)
                      t)])
    (display (map index->word (is:elements nodes))) (newline)
    ; NOTE!!!!!!!!!!!
    (if (<= (is:size nodes) 60)
        (find-longest-path-in-tree word-graph (is:elements nodes))
        '())))

(display "Computing long subpaths...\n")
(for-each (lambda (component)
            (if (not (null? component))
                (begin 
                  (display "length: ") (display (length component)) (newline)
                  (display (map index->word
                                (find-a-long-path word-graph component (length component) index->word)))))
            (newline))
  components)
;
;(display "Computing longest subpaths...\n")
;(for-each (lambda (t) 
;            (display (map index->word
;                          (spanning-tree->longest-subpath t)))
;            (newline))
;          forrest)


;(display "Computing leafs...\n")
;(map (lambda (i) (evector-ref words i))
;     (leafs word-graph))

