;;; integer-graph.scm   --  Jens Axel Søgaard

; This file contains various graph algorithms 

; This version represent edges with the edge struct
; in order to associate information with edges such as weights.

(module integer-graphs-with-edges mzscheme
  (print-struct #t)
  (require (prefix ms: "monotone-set.scm")
           (prefix ns: "non-deterministic-set.scm")
           (prefix ps: "priority-set.scm")
           (prefix es: "equiv-set.scm")
           (prefix pq: "priority-queue.scm")
           (planet "evector.scm" ("soegaard" "evector.plt"))
           (prefix srfi: (lib "1.ss" "SRFI"))
           (lib "match.ss" "mzlib")
           (lib "list.ss" "mzlib")
           (lib "67.ss" "srfi")
           (lib "42.ss" "srfi"))
  
  (provide (all-defined))
  
  ;;; UTILITIES
  
  (define-syntax thunk 
    (syntax-rules () [(_ e ...) (lambda () e ...)]))
  
  (define (interval m n)
    ; m inclusive, n exclusive
    (list-ec (: i m n) i))
  
  ;;; REPRESENTATION
  
  ; A NODE is a non-negative integer.
  
  ; A EDGE is a pair of numbers.
  (define-struct edge (from to) (make-inspector))
  
  ; Some edges have associated weigths
  (define-struct (edge/weight edge) (weight) (make-inspector))
  
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
     edge-lists       ; an evector of sets of edges
     no-nodes)        ; the current number of nodes
    (make-inspector)) ; for debug printing
  
  (define %make-graph make-graph)
  
  ; make-graph : boolean -> graph
  ;   make an empty oriented or unoriented graph
  (set! make-graph 
        ; smart constructor
        (lambda (oriented?)
          (%make-graph oriented? (make-evector 0 (es:empty edge-compare) 'automatic-expansion-on-set!) 0)))
  
  ; Sets of nodes and sets of edges will be represented using the set 
  ; implementations of Galore. The prefixes is and es are used
  ; for integer-set and edge-set respectively.
  ;   is = integer set
  ;   es = edge set   
  
  (require (prefix is: (planet "list-set.scm"  ("soegaard" "galore.plt")))) 
  (require (prefix es: (planet "list-set.scm"  ("soegaard" "galore.plt"))))
  
  ; A compare function for edges is needed in order to make sets of edges
  
  ; edge-compare : edge edge -> (union -1 0 1)
  (define (edge-compare e1 e2)
    (refine-compare
     (integer-compare (edge-from e1) (edge-from e2))
     (integer-compare (edge-to e1)   (edge-to e2))))

  
  ; graph-unoriented? : graph -> boolean
  ;   returns #t if the graph is unoriented, #f otherwise
  (define (graph-unoriented? G)
    (not (graph-oriented? G)))
  
  ; edge-list : graph node -> integer-set
  ;   return the outedges of the node n in the graph G
  (define (edge-list G n)
    (evector-ref (graph-edge-lists G) n))
  
  ; lookup-edge : graph node node -> boolean
  ;   is (from,to) an edge in G?
  (define (lookup-edge G from to)
    (is:member? to (edge-list G from)))
  
  ; add-edge : graph node node [number] -> 
  ;  destructively, add the edge (from,to) to the graph G 
  ;  NOTE:  AN EDGE CAN ONLY BE INSERTED ONCE,
  ;         INSERTING TWICE CREATES TWO EDGES
  (define add-edge 
    (case-lambda
      [(G e)
       (let ([els  (graph-edge-lists G)]
             [from (edge-from e)]
             [to   (edge-to e)])
         (when (>= (max from to) (evector-length els))
           (set-evector-length! els (add1 (max from to))))
         (evector-set! els from (es:insert e (evector-ref els from)))
         (when (graph-unoriented? G)
           (evector-set! els to (es:insert e (evector-ref els to))))
         (set-graph-no-nodes! G (max (add1 from) (add1 to) (graph-no-nodes G))))]
      [(G from to)
       (let ([els (graph-edge-lists G)])
         (when (>= (max from to) (evector-length els))
           (set-evector-length! els (add1 (max from to))))
         (evector-set! els from (es:insert (make-edge from to) (evector-ref els from)))
         (when (graph-unoriented? G)
           (evector-set! els to (es:insert (make-edge to from) (evector-ref els to))))
         (set-graph-no-nodes! G (max (add1 from) (add1 to) (graph-no-nodes G))))]
      [(G from to weight)
       (let ([el (graph-edge-lists G)])
         (when (>= (max from to) (evector-length el))
           (set-evector-length! el (add1 (max from to))))
         (evector-set! el from (es:insert (make-edge/weight from to weight) (evector-ref el from)))
         (when (graph-unoriented? G)
           (evector-set! el to (es:insert (make-edge/weight to from weight) (evector-ref el to))))
         (set-graph-no-nodes! G (max (add1 from) (add1 to) (graph-no-nodes G))))]))
  
  ; macro: (for-each-node n in G body)
  (define-syntax for-each-node
    (syntax-rules (in)
      [(_ n in G body)
       (do ([n 0 (add1 n)])
         [(= n (evector-length (graph-edge-lists G))) (void)]
         body)]))

  ; macro: (for-each-edge ...)
  (define-syntax for-each-edge
    (syntax-rules (from to in =)
      [(_ e = (u v w) from n in G body)
       (for-each (lambda (e)
                   (match e
                     [($ edge/weight u v w) body]))
         (is:elements (evector-ref (graph-edge-lists G) n)))]
      [(_ e = (u v) from n in G body)
       (for-each (lambda (e)
                   (match e
                     [($ edge u v) body]))
         (is:elements (evector-ref (graph-edge-lists G) n)))]
      [(_ (u v) from n in G body)
       (for-each (match-lambda [($ edge u v) body])
         (is:elements (evector-ref (graph-edge-lists G) n)))]
      [(_ e from v in G body)
       (for-each (lambda (e) body)
         (is:elements (evector-ref (graph-edge-lists G) v)))]
      
      [(_ (u v) to n in G body)
       (for-each (match-lambda [($ edge u v) body])
         (in-edges G n))]
      [(_ e to v in G body)
       (for-each (lambda (e) body)
         (in-edges G v))]
      [(_ e = (u v) to n in G body)
        (for-each (lambda (e)
                    (match e
                      [($ edge u v) body]))
          (in-edges G n))]
      
      [(_ (u v w) in G body)
       (for-each (match-lambda [($ edge/weight u v w) body])
         (edges G))]
      [(_ e = (u v) in G body)
       (for-each (lambda (e) (match e [($ edge u v) body]))
         (edges G))]
      [(_ e = (u v w) in G body)
       (for-each (lambda (e) (match e [($ edge/weight u v w) body]))
         (edges G))]
      [(_ e in G body)
       (for-each (lambda (e) body)
         (edges G))]))
  
  ; edges : graph -> (list (cons node node))
  ;   return a list of all edges (conses) of the graph G
  (define (edges G)
    ; return a list of all edges in G
    (let ([all '()])
      (for-each-node v in G
        (for-each-edge e from v in G 
          (set! all (cons e all))))
      all))
  
  ; HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  ; HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  ; HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  ; HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE HERE 
  
  
  ; TODO: OLD VERSION USED THIS FILTER IN THE CASE OF UNORIENTED GRAPHS
  ; [(graph-unoriented? G) (filter (match-lambda [(v . w) (<= v w)])
  ;                               all)])))
  
  (define (in-edges G n)
    ; return list of all in-going edges of n
    ; self-edges are not returned
    (filter (cond
              [(graph-oriented? G)   (match-lambda [($ edge v w) (= w n)])]
              [(graph-unoriented? G) (match-lambda [($ edge v w) (= w n) ;(and (not (= v w)) (or (= v n) (= w n)))
                                                                 ])])
            (edges G)))
  
  (define (in-neighbours G n)
    ; return a list of all nodes which have an edge towards n
    (map (match-lambda [($ edge v w) (if (= v n) w v)])
         (in-edges G n)))
  
  (define (color-graph G type color-node color-edge color-node-and-edge . optionals)
    ; TYPE is either 'depth-first (or 'stack) or 'breadth-first (or 'queue)  
    ; The type determines the discipline used by the non-deterministic set.
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
    
    (let ([PPN (ns:make-set type (graph-no-nodes G))]   ; Potentially Pink Nodes  
          [WN  (ms:set (graph-no-nodes G))])            ; White Nodes
      
      (let ([visit-hook  (lambda (n) 'skip)]
            [start-node  #f]                            
            [follow-edge? (lambda (e) #t)])
        (let loop ([optionals optionals])
          (unless (null? optionals)
            (match optionals
              ; use start in the very first transition ( i) )
              [(('start-node start) . more)  (begin
                                               (set! start-node start)
                                               (loop (cdr optionals)))]
              ; filter some edges out of G (useful for e.g. residual graphs)
              [(('follow-edge? fe?) . more)  (begin
                                               (set! follow-edge? fe?)
                                               (loop (cdr optionals)))]
              ; visit is called when a new node is seen
              [(visit)                        (set! visit-hook visit)])))
        
        (do () [(and (ns:empty? PPN) (ms:empty? WN))  (void)]
          (cond
            ; Transition iii)
            [(ns:empty? PPN)  (let ([v (if start-node
                                           (let ([v start-node])
                                             (ms:delete! WN v)
                                             (set! start-node #f)
                                             v)
                                           (ms:delete-some! WN))])
                                (color-node v)
                                (ns:insert! PPN v))]
            [else             (let ([v (ns:delete-some! PPN)])
                                (visit-hook v)
                                (for-each-edge e = (v w) from v in G
                                  (if (follow-edge? e)
                                      (cond
                                        ; Transition ii)
                                        [(ns:member? PPN w)      (color-edge v w)]
                                        ; Transition i)
                                        [(ms:member? WN w)       (begin
                                                                   (ms:delete! WN w)
                                                                   (color-node-and-edge v w w)
                                                                   (ns:insert! PPN w))]
                                        [else 'skip              ; (v,w) is already red
                                              ])))
                                (if (graph-unoriented? G)   ; TODO !!!! changes from oriented 14 jan 2006
                                    (for-each-edge e = (w v) to v in G
                                      (if (follow-edge? e)
                                          (cond
                                            ; Transition ii)
                                            [(ns:member? PPN w)      (color-edge w v)]
                                            ; Transition i)
                                            [(ms:member? WN w)       (begin
                                                                       (ms:delete! WN w)
                                                                       (color-node-and-edge w v w)
                                                                       (ns:insert! PPN w))]
                                            [else  'skip])))))])))))
  

  (define (connected-components G)
    (let ([components        (make-vector (graph-no-nodes G) 0)]
          [current -1])
      (color-graph G 'breadth-first
                   ; color node
                   (lambda (v) 
                     (set! current (add1 current))
                     (vector-set! components v current))
                   ; color edge
                   (lambda (v w)
                     'skip)
                   ; color edge and node
                   (lambda (v w n)
                     (vector-set! components w current)))
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
  
  (define (spanning-tree G type)
    (let ([tree '()])
      (color-graph G type
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
  
  (define (spanning-forrest G)
    (let ([trees '()]
          [tree  '()])
      (color-graph G 'stack
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
  
  ;;; TEST
  
  (define (make-unoriented-graph)
    (make-graph #f))
  
  (define (make-oriented-graph)
    (make-graph #t))
  
  (define (edges->unoriented-graph es)
    (let ([G (make-unoriented-graph)])
      (for-each (match-lambda [(u v)   (add-edge G u v)]
                              [(u v w) (add-edge G u v w)])
        es)
      G))
  
  (define (edges->oriented-graph es)
    (let ([G (make-oriented-graph)])
      (for-each (match-lambda [(u v)   (add-edge G u v)]
                              [(u v w) (add-edge G u v w)])
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
  
  (define G-daimi-page-16
    ; Example of minimum spanning tree of unoriented graph
    ; The weight of a minimum spanning tree is 42
    (edges->unoriented-graph
     '((0 1 8) (0 2 5)
               (1 2 10) (1 3 2) (1 4 18)
               (2 3 3) (2 5 16)
               (3 4 12) (3 5 30) (3 6 14)
               (4 6 4)
               (5 6 26))))
  
  (define G-daimi-page-38
    (edges->oriented-graph
     '((0 2 3) (0 3 4)
               (1 0 8) (1 3 6)
               (2 5 9)
               (3 2 7) (3 4 1) (3 5 5)
               (4 1 6) (4 6 3)
               (5 4 2) (5 6 2))))
  
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
                              [time          0]
                              [time++        (thunk (set! time (+ time 1)))])
                         (let-syntax ([color!   (syntax-rules () [(_ v c) (vector-set! colors v c)])]
                                      [color    (syntax-rules () [(_ v)   (vector-ref  colors v)])])
                           (letrec ([visit (lambda (u)
                                             (color! u 'grey)
                                             (time++)
                                             (vector-set! discovery u time)
                                             (for-each-edge (u v) from u in G
                                               (when (eq? (color v) 'white)
                                                 (vector-set! predecessor v u)
                                                 (visit v)))
                                             (color! u 'black)
                                             (time++)
                                             (vector-set! finish u time)
                                             (finish-hook u))])
                             
                             (for-each-node u in G
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
                      [($ edge u v) (back-edge? u v)])
                    (edges G))))))
  
  
  
  (define (single-source-minimum-path G source)
    ;;;  T[Dijkstra] in O( (V+E)log(V) )
    (let ([remaining       (ps:make-empty (graph-no-nodes G))]  
          [unseen-nodes    (ms:set (graph-no-nodes G))]         
          [distances       (make-vector (graph-no-nodes G) +inf.0)]
          [paths           (make-vector (graph-no-nodes G) #f)])  
      (let-syntax ([distance              (syntax-rules () [(_ n)     (vector-ref  distances n)])]
                   [distance!             (syntax-rules () [(_ n d)   (vector-set! distances n d)])]
                   [path                  (syntax-rules () [(_ n)     (vector-ref  paths n)])]
                   [path!                 (syntax-rules () [(_ n d)   (vector-set! paths n d)])]
                   [insert-remaining!     (syntax-rules () [(_ n e d) (ps:insert!  remaining n e d)])]
                   [delete-min-remaining! (syntax-rules () [(_)       (ps:delete-min! remaining)])])
        
        (ms:delete! unseen-nodes source)
        (distance! source 0)
        (path! source '())
        (for-each-edge e = (source v d) from source in G
          (begin
            (ms:delete! unseen-nodes v)
            (distance! v d)
            (path! v (list e))
            (insert-remaining! v e d)))
        (do () [(ps:empty? remaining) 'done]
          (let-values ([(v e d) (ps:find-min remaining)])
            (delete-min-remaining!)
            (distance! v d)
            (path! v (cons e (path (edge-from e))))
            (for-each-edge e = (v w d) from v in G
              (cond
                [(ps:member? remaining w)       (if (> (ps:priority remaining w) (+ (distance v) d))
                                                    (ps:change-priority! remaining w (+ (distance v) d)))]
                [(ms:member? unseen-nodes w)    (begin
                                                  (ms:delete! unseen-nodes w)
                                                  (insert-remaining! w e (+ (distance v) d)))]))))
        (values distances 
                (do ([i 0 (+ i 1)])
                  [(= i (graph-no-nodes G)) paths]
                  (path! i (if (path i) (reverse (path i)) #f)))))))
  
  (define (minimum-spanning-tree-Prim G)
    ;;; T[Prim] = O( E log(E) )
    (unless (graph-unoriented? G)
      (error "minimum-spanning-treee: Expects unoriented graph; given: " G))
    
    (let ([remaining       (ps:make-empty (graph-no-nodes G))]  
          [unseen-nodes    (ms:set (graph-no-nodes G))]         
          [T               (make-unoriented-graph)])
      (do () [(and (ps:empty? remaining)
                   (ms:empty? unseen-nodes)) T]
        (cond
          [(ps:empty? remaining)   (begin
                                     (let ([u (ms:delete-some! unseen-nodes)])
                                       ; add node v to the tree T
                                       (for-each-edge e = (u v w) from u in G
                                         (begin
                                           (ms:delete! unseen-nodes v)
                                           (ps:insert! remaining v e w)))))]
          [else           (let-values ([(u e w) (ps:delete-min! remaining)])
                            (add-edge T (edge-from e) (edge-to e) (edge/weight-weight e))
                            (for-each-edge e = (u v w) from u in G
                              (cond
                                [(ps:member? remaining v)    (if (> (ps:priority remaining v) w)
                                                                 (begin
                                                                   (ps:change-attribute! remaining v e)
                                                                   (ps:change-priority!  remaining v w)))]
                                [(ms:member? unseen-nodes v) (begin
                                                               (ms:delete! unseen-nodes v)
                                                               (ps:insert! remaining v e w))])))]))))
  
  (define (minimum-spanning-tree-Kruskal G)
    (unless (graph-unoriented? G)
      (error "minimum-spanning-treee: Expects unoriented graph; given: " G))
    
    (let ([remaining     (pq:make-empty)]
          [blue          (es:make (graph-no-nodes G))]
          [T             (make-unoriented-graph)])
      (for-each-edge e = (u v w) in G
        (pq:insert! remaining e w))
      (do () [(pq:empty? remaining) T]
        (let-values ([(e _) (pq:delete-min! remaining)])
          (match e
            [($ edge/weight u v w)  (cond
                                      [(not (es:equivalent? blue u v))  (begin 
                                                                          (add-edge T u v w)
                                                                          (es:join! blue u v))]
                                      [else                            'skip])])))))
  
  
  (define (leafs G)
    (let ([edge-lists (graph-edge-lists G)])
      (list-ec (:range v 0 (graph-no-nodes G))
               (if (<= (es:size (evector-ref edge-lists v)) 1))
               v)))
  
  
  (define (find-leaf G nodes)
    ; find a leaf among the nodes in nodes
    (let* ([edge-lists (graph-edge-lists G)]
           [l #f])
      (do-ec (:until (:list n nodes)
                     (begin
                       (set! l n)
                       (<= (es:size (evector-ref edge-lists n)) 1)))
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
                                    (is:list->set number-compare 
                                                  (map edge-to (es:elements (evector-ref (graph-edge-lists G) (car path)))))
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
  
  (define (find-a-long-path G start-nodes longest-possible-length . f )
    ; TODO: add arguments: forever?, constant, display-longest-so-far, f to call before displaying
    (when (null? nodes)
      (error 'find-a-long-path "The list of start-nodes must be non-empty"))
    (let ([A           (list-ref start-nodes (random (length start-nodes)))]
          [max-trials  (* #;200000 50000 (/ (sqrt (graph-no-nodes G)) (log (graph-no-nodes G))))]
          [edge-lists  (graph-edge-lists G)])
      (let loop ([longest        '()]
                 [longest-length 0]
                 [trials         0])
        (if  #; #f
             (or (> trials max-trials)
                 (>= longest-length longest-possible-length))
             longest
             (let ([new-path 
                    (let walk ([path      (list A)]
                               [used      (is:insert A (is:empty number-compare))])
                      (let* ([edges (evector-ref edge-lists (car path))]
                             [edges (if (null? (cdr path))
                                        edges
                                        ; remove the edge back
                                        (es:delete (make-edge (car path) (cadr path)) edges))])
                        (if (zero? (es:size edges))
                            path
                            (let ([edge (list-ref (es:elements edges) (random (es:size edges)))])
                              (if (es:member? (edge-to edge) used)
                                  path
                                  (walk (cons (edge-to edge) path)
                                        (es:insert (edge-to edge) used)))))))])
               (if (> (length new-path) longest-length)
                   (begin
                     (if (not (null? f))
                         (display (map (car f) new-path)))
                     (newline)
                     (loop new-path (length new-path) (+ trials 1)))
                   (loop longest  longest-length    (+ trials 1))))))))
  
  ;;;
  ;;; FLOW NETWORKS
  ;;;
  
  ; A FLOW NETWORK G=(V,E) is an oriented graph. Each edge (u,v) in E has a nonnegative
  ; capacity c(u,v)>=0. If (u,v) not-in E, then c(u,v)=0. Two special nodes are 
  ; called the source s, and the sink t.
  ; A flow is a function f:VxV->R which has
  ;  - capacity constraint   f(u,v) <= c(u,v)     for all u,v in V
  ;  - skew symmetry         f(u,v) = -f(u,v)     for all u,v in V
  ;  - flow conservation       sum  f(u,v) = 0    for all u in V\{s,t}
  ;                          u in V

  (define-struct (flow-edge edge) (capacity flow) (make-inspector))
  
  (define (maximum-flow-ford-fulkerson G s t)
    (initialize-flow-to-0 G)
    (let loop ()
      (cond
        [(find-augmenting-path G s t) 
         => (lambda (p) 
              (augment-flow-along-path G p)
              (loop))])))
  
  (define (initialize-flow-to-0 G)
    (for-each-edge e in G
      (set-flow-edge-flow! e 0)))
  
  (define (residual-capacity e)
    (- (flow-edge-capacity e)
       (flow-edge-flow e)))

  ;(color-graph G type color-node color-edge color-node-and-edge . optionals)  
  (define (find-augmenting-path G s t)
    (let ([paths        (make-vector (graph-no-nodes G) '())]
          [current-edge #f])
      (let/ec return
        (color-graph G 'breadth-first
                     ; color node
                     (lambda (v)
                       ;(display (format "color node: ~a\n" v))
                       (if (not (= v s))
                           (return #f))
                       'skip)
                     ; color edge
                     (lambda (v w)
                       ;(display (format "color edge: ~a -> ~a\n" v w))
                       (vector-set! paths w (cons current-edge (vector-ref paths v)))
                       'skip)
                     ; color edge and node
                     (lambda (v w n)
                       ;(display (format "color edge and node : ~a ~a ~a \n" v w n))
                       (vector-set! paths w (cons current-edge (vector-ref paths v)))
                       (if (= w t)
                           (return (reverse! (vector-ref paths t)))))
                     ; start at s
                     `(start-node ,s)
                     ; consider only edge with residual capacity
                     `(follow-edge? ,(lambda (e)
                                       (set! current-edge e)
                                       (> (residual-capacity e) 0))))
        #f)))
  
  
  (define (augment-flow-along-path G p)
    (let ([additional-flow 
           (apply min
                  (map residual-capacity
                       p))])
      (for-each (lambda (e)
                  (set-flow-edge-flow! e (+ (flow-edge-flow e) additional-flow)))
        p)))
     

  (define (flow-edges->flow-graph es)
    (let ([G (make-oriented-graph)])
      (for-each (match-lambda [(u v c) (add-edge G (make-flow-edge u v c 0))])
        es)
      G))
  
  (define G-cormen-page-533
    (flow-edges->flow-graph
     '(#;- (0 1 16) (0 2 13)  ; s
           (1 2 10) (1 3 12)  ; v1
           (2 1  4) (2 4 14)  ; v2
           (3 2  9) (3 5 20)  ; v3
           (4 3  7) (4 5 4)   ; v4
           )))                ; t

  ;(display (find-augmenting-path G-cormen-page-533 0 5))
   
  (maximum-flow-ford-fulkerson G-cormen-page-533 0 5)
  
  (define (display/nl o)
    (display o)
    (newline))
  
  (for-each display/nl (edges G-cormen-page-533))
  
  (define (edge-connectivity G)
    ; Cormen p. 664
    ; The edge connectivity of an undirected graph
    ; is minimum number k of edges that must be
    ; removed to disconnect the graph.
    
    ; edge-connectivity  =   min    #{p | p is path in Gt from v to t|}
    ;                       v in V
    ; where
    ;   Gt is the graph G with an node (sink) t,
    ;   such that (u,t) is an edge for all u<>v.
    
    (let* ([t          (graph-no-nodes G)]
           [inf        (graph-no-nodes G)]
           [sink-edges (make-vector t)])
      ; add an edges from all nodes to a sink t
      (for-each-node u in G
        (let ([e (make-flow-edge u t inf 0)])
          (unless (= u t)
            (vector-set! sink-edges u e)
            (add-edge G e))))
      ; initialize all capacities
      (for-each-edge e = (u v) in G
        (set-flow-edge-capacity! e (if (= v t) inf 1)))
      ; find minimum
      (min-ec (: v 0 t)
              ; "delete" edge from v to t
              (begin
                (set-flow-edge-capacity! (vector-ref sink-edges v) 0)
              ; calculate maximum flow
                (maximum-flow-ford-fulkerson G v t)
                (let ([flow (flow-into G t)])
                  ; "reinsert" edge fron v to t
                  (set-flow-edge-capacity! (vector-ref sink-edges v) inf)
                  (display (list v flow))
                  (newline)
                  flow)))))
  
  (define (flow-into G t)
    (sum-ec (: e (in-edges G t))
            (flow-edge-flow e)))
  
  (define (unoriented-edges->flow-graph es)
    (let ([G (make-oriented-graph)])
      (for-each (match-lambda [(u v) 
                               (add-edge G (make-flow-edge u v 0 0))
                               (add-edge G (make-flow-edge v u 0 0))])
        es)
      G))
  
  (define Gc1
    (unoriented-edges->flow-graph
     '((0 1) (1 2))))
  
  (define Gc2
    (unoriented-edges->flow-graph
     '((0 1) (1 2) (2 0))))

  (define Gc3
    (unoriented-edges->flow-graph
     '((0 1) (0 2) (0 3)
             (1 2) (1 3)
             (2 3))))

  #;(begin  
      (display (edge-connectivity Gc1))
      (display (edge-connectivity Gc2))
      (display (edge-connectivity Gc3))
      )
  
   )