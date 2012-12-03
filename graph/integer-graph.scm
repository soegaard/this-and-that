;;; integer-graph.scm   --  Jens Axel Søgaard


(require (prefix ms: "monotone-set.scm")
         (prefix ns: "non-deterministic-set.scm")
         (planet "evector.scm" ("soegaard" "evector.plt"))
         (prefix is: (planet "list-set.ss"   ("soegaard" "galore.plt")))
         (prefix es: (planet "list-set.ss"   ("soegaard" "galore.plt")))
         (lib "67.ss" "srfi")
         (lib "42.ss" "srfi"))

;;; UTILITIES

(define-syntax thunk (syntax-rules () [(_ e ...) (lambda () e ...)]))

(define (interval m n)
  (list-ec (: i m n) i))

;;; REPRESENTATION

; A NODE is a non-negative integer.

; An EDGE is a pair of nodes (i.e. numbers).
(define-struct edge (from to))

; Some edges have associated weights.
(define-struct (edge/weight edge) (weight))

; A GRAPH consists of a set of nodes and a set of edges.
; The representation is a standard edge-list representation.
; More precisely: Each node v in the graph has an associated 
; set of out-edges, (i.e.a set of all nodes w such that (v,w) 
; is an edge). The out-edges are represented as an ordered
; list [almost see below]. 

; The set of nodes of the graph is represented as an extensible 
; vector. A node (number) is used as index, and the stored value
; is the associated out-edge set.

; A flag records whether the graph is oriented or not.

(define-struct graph 
               (oriented?        ; #t or #f
                edge-lists       ; an evector of sets of integers
                no-nodes)        ; the current number of nodes
               (make-inspector)) ; for debug printing
(define %make-graph make-graph)


; Sets of nodes and sets of edges will be represented using the set 
; implementations of Galore. The prefixes is and es are used
; for integer-set and edge-set respectively.
;   is = integer set
;   es = edge set   


; make-graph : boolean -> graph
;   make an empty oriented or unoriented graph
(define (make-graph oriented?)
  (%make-graph oriented? (make-evector 0 (is:empty integer-compare) 'automatic-expansion-on-set!) 0))

; graph-unoriented? : graph -> boolean
;   returns #t if the graph is unoriented, #f otherwise
(define (graph-unoriented? G)
  (not (graph-oriented? G)))

; edge-list : graph node -> integer-set
;   return the outedges of the node n in the graph G
(define (edge-list G n)
  ; return the integer set representing the edge-list 
  ; associated with the node n
  (evector-ref (graph-edge-lists G) n))

; lookup-edge : graph node node -> boolean
;   is (from,to) an edge in G?
(define (lookup-edge G from to)
  (is:member? to (edge-list G from)))

; add-edge : graph node node -> 
;  destructively, add the edge (from,to) to the graph G 
(define (add-edge G from to)
  (let ([el (graph-edge-lists G)])
    (when (>= (max from to) (evector-length el))
      (set-evector-length! el (add1 (max from to))))
    (evector-set! el from (is:insert to (evector-ref el from)))
    (when (graph-unoriented? G)
      (evector-set! el to (is:insert from (evector-ref el to))))
    (set-graph-no-nodes! G (max (add1 from) (add1 to) (graph-no-nodes G)))))

; edges : graph -> (list (cons node node))
;   return a list of all edges (conses) of the graph G
(define (edges G)
  ; HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  ; HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  ; HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  ; HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  (let ([all (do ([v 0    (add1 v)]
                  [es '() (append (map (lambda (w) (cons v w))
                                       (es:elements (edge-list G v)))
                                  es)])
               [(= v (evector-length (graph-edge-lists G))) es])])
    (cond
      [(graph-oriented? G)   all]
      [(graph-unoriented? G) (filter (match-lambda [(v . w) (<= v w)])
                                     all)])))

(define (in-edges G n)
  ; return list of all in-going edges of n
  ; self-edges are not returned
  (filter (cond
            [(graph-oriented? G)   (match-lambda [(v . w) (= w n)])]
            [(graph-unoriented? G) (match-lambda [(v . w) (and (not (= v w)) (or (= v n) (= w n)))])])
          (edges G)))

(define (in-neighbours G n)
  ; return a list of all nodes which have an edge towards n
  (map (match-lambda [(v . w) (if (= v n) w v)])
       (in-edges G n)))

(define (color-graph G type color-node color-edge color-node-and-edge . optionals)
  ; TYPE is either 'stack or 'queue. Determines the discipline used by the non-deterministic set.
  (if (eq? type 'breadth-first) (set! type 'queue))
  (if (eq? type 'depth-first)   (set! type 'stack))
  
  ; We use the following transition system:
  
  ;   i)  X->-O  becomes  X=>=X        calls color-node-and-edge
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
  
  ; Remark: Each time iii) is used, it means that we have started coloring a new 
  ;         connectedness component.
  
  (let-syntax 
      ; (for-each-edge (v w) e) calls (lambda (w) e) with each neighbour node w where (v,w) in G
      ([for-each-edge (syntax-rules () 
                        [(_ (v w) e) (for-each (lambda (w) e) (es:elements (edge-list G v)))]
                        #;[(_ (v w) e) (for-each (lambda (w) e) (edge-list G v))])]
       ; (for-each-in-edge (w v) e) calls (lambda (w) e) with each neighbour node w where (w,v) in G
       [for-each-in-edge (syntax-rules () 
                           [(_ (w v) e) (for-each (lambda (w) e) (map car (in-edges G v)))])])
    
    (let ([PPN (ns:make-set type (graph-no-nodes G))]   ; Potentially Pink Nodes  
          [WN  (ms:set (graph-no-nodes G))])            ; White Nodes
      
      (let ([visit-hook (lambda (n) 'skip)])
        (match optionals
          [()            'skip]
          [(visit)       (set! visit-hook visit)])
        
        (do () [(and (ns:empty? PPN) (ms:empty? WN))  (void)]
          (cond
            ; Transition iii)
            [(ns:empty? PPN)  (let ([v (ms:delete-some! WN)])   
                                (color-node v)
                                (ns:insert! PPN v))]
            [else             (let ([v (ns:delete-some! PPN)])
                                (visit-hook v)
                                (for-each-edge (v w)                                                      
                                               (cond
                                                 ; Transition ii)
                                                 [(ns:member? PPN w)      (color-edge v w)]
                                                 ; Transition i)
                                                 [(ms:member? WN w)       (begin
                                                                            (ms:delete! WN w)
                                                                            (color-node-and-edge v w w)
                                                                            (ns:insert! PPN w))]
                                                 [else                    
                                                  ;(for-each display (list "skipped: " (list v w) "\n"))
                                                  ; (v,w) is already red
                                                  'skip
                                                  ]))
                                (if (graph-oriented? G)
                                    (for-each-in-edge (w v)                                                      
                                                      (cond
                                                        ; Transition ii)
                                                        [(ns:member? PPN w)      (color-edge w v)]
                                                        ; Transition i)
                                                        [(ms:member? WN w)       (begin
                                                                                   (ms:delete! WN w)
                                                                                   (color-node-and-edge w v w)
                                                                                   (ns:insert! PPN w))]
                                                        [else                    
                                                         ;(for-each display (list "skipped: " (list v w) "\n"))
                                                         ; (v,w) is already red
                                                         'skip
                                                         ]))))]))))))


(define (connected-components G)
  (let ([components        (make-vector (graph-no-nodes G) 0)]
        [current-component -1])
    (color-graph G 'breadth-first
                 ; color node
                 (lambda (v) 
                   (set! current-component (add1 current-component))
                   (vector-set! components v current-component))
                 ; color edge
                 (lambda (v w)
                   'skip)
                 ; color edge and node
                 (lambda (v w n)
                   (vector-set! components w current-component)))
    components))

(define (number-depth-first G)
  (let ([numbers  (make-vector (graph-no-nodes G) 0)]
        [i        -1])
    (color-graph G 'stack
                 ; color node
                 (lambda (v) 
                   'skip)
                 ; color edge
                 (lambda (v w)
                   'skip)
                 ; color edge and node
                 (lambda (v w n)
                   'skip)
                 ; hook for delete-some
                 (lambda (v)
                   (set! i (add1 i))
                   (vector-set! numbers v i)))
    numbers))

(define (number-breadth-first G)
  (let ([numbers  (make-vector (graph-no-nodes G) #f)]
        [i        -1])
    (color-graph G 'queue
                 ; color node
                 (lambda (v) 
                   (set! i (add1 i))
                   (vector-set! numbers v i))
                 ; color edge
                 (lambda (v w)
                   'skip)
                 ; color edge and node
                 (lambda (v w n)
                   (unless (vector-ref numbers w)
                     (set! i (add1 i))
                     (vector-set! numbers w i))))
    numbers))

(define (spanning-tree G . type)
  (let ([tree '()])
    (color-graph G (match type
                     [('depth-first)   'stack]
                     [('breadth-first) 'queue]
                     [_                'stack])
                 ; color node
                 (lambda (v) 
                   'skip)
                 ; color edge
                 (lambda (v w)
                   'skip)
                 ; color edge and node
                 (lambda (v w n)
                   (set! tree (cons (list v w) tree))))
    tree))

(define (spanning-forrest G . type)
  (let ([trees '()]
        [tree  '()])
    (color-graph G (match type
                     [('depth-first)   'stack]
                     [('breadth-first) 'queue]
                     [_                'stack])
                 ; color node
                 (lambda (v)
                   (set! trees (cons tree trees))
                   (set! tree '()))
                 ; color edge
                 (lambda (v w)
                   'skip)
                 ; color edge and node
                 (lambda (v w n)
                   (set! tree (cons (list v w) tree))))
    trees))

(define (leafs G)
  (let ([edge-lists (graph-edge-lists G)])
    (list-ec (:range v 0 (graph-no-nodes G))
             (if (<= (is:size (evector-ref edge-lists v)) 1))
             v)))


(define (find-leaf G nodes)
  ; find a leaf among the nodes in nodes
  (let* ([edge-lists (graph-edge-lists G)]
         [l #f])
    (do-ec (:until (:list n nodes)
                   (begin
                     (set! l n)
                     (<= (is:size (evector-ref edge-lists n)) 1)))
           'skip)
    l))

; find-longest-path-in-tree-from : graph node (list nodes)
;   find the longest path starting in A in G consististing of nodes from nodes
(define (find-longest-path-in-tree-from G A nodes)
  ; longest-extension-of : (list node) (list node) -> (list node)
  (define (longest-extension-of path path-length available)
    (let loop ([longest         path]
               [longest-length  path-length]
               [candidates      (is:elements 
                                 (is:intersection
                                  (evector-ref (graph-edge-lists G) (car path))
                                  available))])
      (cond
        [(null? candidates)
         (values longest longest-length)]
        [else
         (let-values ([(extension extension-length)
                       (longest-extension-of (cons (car candidates) path)
                                             (+ longest-length 1)
                                             (is:delete (car candidates) available))])
           (if (> extension-length longest-length)
               (loop extension extension-length (cdr candidates))
               (loop longest   longest-length   (cdr candidates))))])))
  (let-values ([(extension length) 
                (longest-extension-of (list A) 1 (is:delete A (is:list->set nodes)))])
    extension))
  
(define (nodes G)
  (interval 0 (graph-no-nodes G)))

(define (find-longest-path-in-tree G nodes)
  (let ([A  (find-leaf G nodes)])
    (cond
      [A  (let* ([p1 (find-longest-path-in-tree-from G A nodes)]
                 [B  (car p1)]
                 [p2 (find-longest-path-in-tree-from G B nodes)])
            p2)]
      [else
       (list)])))


;;; TEST

(define (make-unoriented-graph)
  (make-graph #f))

(define (make-oriented-graph)
  (make-graph #t))

(define (edges->unoriented-graph es)
  (let ([G (make-unoriented-graph)])
    (for-each (match-lambda [(v w) (add-edge G v w)])
              es)
    G))

(define (edges->oriented-graph es)
  (let ([G (make-oriented-graph)])
    (for-each (match-lambda [(v w) (add-edge G v w)])
              es)
    G))

(define G1 
  ; [DAIMI FN 57, p. 14]
  ; Example of breadth first numbering of graph
  (edges->unoriented-graph 
   '((1 2) (1 3) (1 4)
     (2 5) (2 6)
     (3 4)
     (4 7)
     (5 8) (5 9)
     (6 7)
     (7 9)
     (8 9))))

(define G2
  (edges->unoriented-graph
   '((0 1) (0 2)
     (1 2) (1 3) (1 4)
     (2 4)
     (3 4)
     
     (5 6) (5 7)
     (6 7))))

(define G3
  ; [Cormen et al., p. 542]
  (edges->oriented-graph
   '((0 1) (0 3)
     (1 4)
     (2 4) (2 5)
     (3 1)
     (4 3))))

(define G4-names #(undershorts pants belt shirt tie jacket socks shoes watch))

(define G4
  ; [Cormen et al., p. 530]
  (edges->oriented-graph
   '((0 1) (0 7)
     (1 2) (1 7)
     (2 5)
     (3 2) (3 4)
     (4 5)
     (6 7)
     (8 8))))

(define (display-G4)
  (let ([name (lambda (n) (vector-ref G4-names n))])
    (map (match-lambda 
           [(from to) (display (name from)) (display " -> ") (display (name to)) (newline)])
         '((0 1) (0 7)
           (1 2) (1 7)
           (2 5)
           (3 2) (3 4)
           (4 5)
           (6 7)
           (8 8)))))

#;
(color-graph G4
             'depth-first
             (lambda (n)     (for-each display (list "            node > " n "\n")))
             (lambda (v w)   (for-each display (list "            edge > " (list v w)   "\n")))
             (lambda (v w n) (for-each display (list "            both > " (list v w) " " n "\n")))
             (lambda (n)     (for-each display (list "visit > " n "\n")))
             )

(define (discovery/finish-times G type)
  (let* ([discovery-times  (make-vector (graph-no-nodes G) #f)]
         [finishing-times  (make-vector (graph-no-nodes G) #f)]
         [discovery-time   -1]
         [finishing-time   -1]
         [discover  (lambda (n)
                      (unless (vector-ref discovery-times n)
                        (set! discovery-time (add1 discovery-time))
                        (vector-set! discovery-times n discovery-time)))]
         [finish    (lambda (n)
                      (set! finishing-time (add1 finishing-time))
                      (vector-set! finishing-times n finishing-time))]
         [both       (lambda (n)
                       (discover n)
                       (finish n))])
    
    (color-graph G
                 type
                 ; color node
                 both
                 ; color edge
                 (lambda (v w) 'skip)
                 ; color edge and node
                 (lambda (v w n)
                   (discover v)
                   (discover w)
                   (finish n)))
    
    (values discovery-times finishing-times)))



#;
(begin
(discovery/finish-times G1 'breadth-first)
(discovery/finish-times G1 'depth-first)
)

#;
(let ([name (lambda (n) (vector-ref G4-names n))])
  (color-graph G4
               'depth-first
               (lambda (n)   (for-each display (list "            node > " (name n) "\n")))
               (lambda (v w) (for-each display (list "            edge > " (list (name v) (name w))   "\n")))
               (lambda (v w n) (for-each display (list "          both > " (list (name v) (name w)) " " (name n) "\n")))
               (lambda (n)   (for-each display (list "visit > " (name n) "\n")))
               ))

(define depth-first-search 
  (case-lambda 
    [(G)             (depth-first-search G (lambda (n) (void)))]
    [(G finish-hook) (let* ([no-nodes      (graph-no-nodes G)]
                            [colors        (make-vector no-nodes 'white)]
                            [predecessor   (make-vector no-nodes #f)]
                            [discovery     (make-vector no-nodes #f)]
                            [finish        (make-vector no-nodes #f)]
                            [time          0])
                       (let-syntax ([set-color! (syntax-rules () [(_ v c) (vector-set! colors v c)])]
                                    [color      (syntax-rules () [(_ v)   (vector-ref  colors v)])])
                         (letrec ([visit (lambda (u)
                                           (set-color! u 'grey)
                                           (set! time (add1 time))
                                           (vector-set! discovery u time)
                                           (for-each (lambda (v)
                                                       (when (eq? (color v) 'white)
                                                         (vector-set! predecessor v u)
                                                         (visit v)))
                                                     (edge-list G u))
                                           (set-color! u 'black)
                                           (set! time (add1 time))
                                           (vector-set! finish u time)
                                           (finish-hook u))])
                           ; for each node u in G
                           (do ([u 0 (add1 u)])
                             [(= u no-nodes) (void)]
                             (if (eq? (color u) 'white)
                                 (visit u)))
        
                           (values discovery finish predecessor))))]))
      
(define (topological-sort G-dag)
  (let* ([sorted       '()]
         [finish-hook  (lambda (n)
                         (set! sorted (cons n sorted)))])
    (depth-first-search G-dag finish-hook)
    sorted))

(define (acyclic? G)
  ; An oriented graph is acyclic, if it doesn't
  ; contain any back edges.
  
  (when (not (graph-oriented? G))
    (error "acyclic?: Expects oriented graph; given " G))
  (let-values ([(d f p) (depth-first-search G)])
    (let ([back-edge? (lambda (u v)
                        (< (vector-ref d v)
                           (vector-ref d u)
                           (vector-ref f u)
                           (vector-ref f v)))])
      (not (ormap (match-lambda 
                    [(u . v) (back-edge? u v)])
                  (edges G))))))


