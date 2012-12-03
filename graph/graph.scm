#lang racket
;;; matrix-graphs.scm   --  Jens Axel Søgaard

(require (prefix-in ms: "monotone-set.scm")
         (prefix-in ns: "non-deterministic-set.scm"))

(define-syntax thunk (syntax-rules () [(_ e ...) (lambda () e ...)]))


(define MAX-NODES 100)   ; TODO: Use an extensible vector

(define-struct node (index) #:mutable)

(define-struct graph 
               (oriented? edges nodes no-nodes nodes-vector) ; where edges and nodes are hash tables
  #:mutable) 
(define %make-graph make-graph)

(define (Make-graph n oriented?)
  (let ([edges        (make-hasheqv)]
        [nodes        (make-hasheqv)]
        [no-nodes     0]
        [nodes-vector (make-vector MAX-NODES #f)])
    (%make-graph oriented? edges nodes no-nodes nodes-vector)))

(define (lookup-node G node)
  (hash-ref (graph-nodes G) node (thunk #f)))

(define (add-node G node)
  (when (not (hash-ref (graph-nodes G) node (thunk #f)))
    (set-node-index! node (graph-no-nodes G))
    (vector-set! (graph-nodes-vector G) (graph-no-nodes G) node)
    (set-graph-no-nodes! G (add1 (graph-no-nodes G)))
    (hash-set! (graph-nodes G) node #t)))

(define (nodes G)
  (hash-map (graph-nodes G) (lambda (n v) n)))

(define (lookup-edge G from to)
  (hash-ref (graph-edges G) (cons from to) (thunk #f)))

(define (add-edge G from to)
  (add-node G from)
  (add-node G to)
  (cond
    [(graph-oriented? G) (hash-set! (graph-edges G) (cons from to) #t)]
    [else                (when (and (not (lookup-edge G from to))
                                    (not (lookup-edge G to from)))
                           (hash-set! (graph-edges G) (cons to from) #t))]))

(define (edges G)
  (hash-map (graph-edges G) (lambda (e v) e)))

(define (make-edge G from to)
  (cond
    [(lookup-edge G from to)        => (lambda (e) e)]
    [(and (not (graph-oriented? G))
          (lookup-edge G from to))  => (lambda (e) e)]
    [else                              (let ([new-edge (make-edge from to)])
                                         (add-edge G new-edge)
                                         new-edge)]))

(define (in-edges G n)
  (let ([n (if (number? n) 
               (vector-ref (graph-nodes-vector G) n)
               n)])
    (cond
      [(graph-oriented? G) (filter (lambda (p) (eq? (cdr p) n))
                                   (edges G))]
      [else                (filter (lambda (p) (and (eq? (cdr p) n)
                                                    (not (eq? (car p) (cdr p)))))
                                   (edges G))])))


(define (color-graph G color-node color-edge . type-of-PPN)  
  ; We use the following transition system:
  
  ;   i)  X->-O  becomes  X=>=X        calls color-node and color-edge
  ;  ii)  X->-X  becomes  X=>=X        calls color-edge
  ; iii) If there are no pink nodes
  ;        O    becomes    X           calls color-node
  
  ; If the graph isn't oriented then read ->- as ---.
  
  ; Not oriented: A pink node is a red node X with at least one white edge X---? .
  ; Oriented:     A pink node is a red node X with at least one *outgoing* white edge X->-? .
  
  ; Notation:    O   white node
  ;              X   red node
  ;             ---  white edge
  ;             ===  red edge
  
  ; The set PPN (Potentially Pink Nodes) can either use a stack or a queue
  ; strategy, when nodes are deleted. The argument type-of-PPN determines this.
  ; The default strategy is a stack.

  (define-syntax for-each-in-neighbour
    ; (for-each-in-neighbour-to w f)  calls f with each neighbour node v, where (v,w) in G
    (syntax-rules () [(_ w f) (for-each (lambda (v) (f (node-index v))) (map car (in-edges G w)))]))
  
  ; Handle the optional argument type-of-PPN.
  (match type-of-PPN
    [(list)        (set! type-of-PPN 'stack)]
    [(list type)   (set! type-of-PPN type)]
    [else          (error "Unknown type")])
  
  (let ([PPN (ns:make-set type-of-PPN (graph-no-nodes G))]   ; Potentially Pink Nodes  
        [WN  (ms:set (graph-no-nodes G))])  ; White Nodes
    (do () [(and (ns:empty? PPN) (ms:empty? WN))  (void)]
      (cond
        ; Transition iii)
        [(ns:empty? PPN)  (let ([v (ms:delete-some! WN)])   
                            (color-node v)
                            (ns:insert! PPN v))]
        [else             (let ([v (ns:delete-some! PPN)])     
                            (for-each-in-neighbour v
                                                   (lambda (w)
                                                     (cond
                                                       ; Transition ii)
                                                       [(ns:member? PPN w)      (color-edge v w)]
                                                       ; Transition i)
                                                       [(ms:member? WN w)       (begin
                                                                                  (ms:delete! WN w)
                                                                                  (color-edge v w)
                                                                                  (color-node w)
                                                                                  (ns:insert! PPN w))]
                                                       [else                    'skip]))))]))))
  
  ;;; TEST
    
  (define G  (Make-graph 10 #f))
  
  (define n0 (make-node 0))
  (define n1 (make-node 1))
  (define n2 (make-node 2))
  (define n3 (make-node 3))
  (define n4 (make-node 4))
  (define n5 (make-node 5))
  (define n6 (make-node 6))
  (define n7 (make-node 7))
  (define n8 (make-node 8))
  
  (add-node G n0)
  (add-node G n1)
  (add-node G n2)
  (add-node G n3)
  (add-node G n4)
  (add-node G n5)
  (add-node G n6)
  (add-node G n7)
  (add-node G n8)
  
  
  (add-edge G n1 n2)
  (add-edge G n1 n0)
  (add-edge G n1 n8)
  (add-edge G n2 n3)
  (add-edge G n2 n7)
  (add-edge G n3 n4)
  (add-edge G n3 n5)
  (add-edge G n4 n5)
  (add-edge G n5 n6)
  (add-edge G n6 n7)
  (add-edge G n6 n8)
  (add-edge G n8 n0)
  
  (color-graph G
               (lambda (n)     (display n) (newline))
               (lambda (n1 n2) (display (list n1 n2)) (newline)))
