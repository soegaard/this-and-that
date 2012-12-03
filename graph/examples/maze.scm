;;; maze.scm  --  Jens Axel Søgaard  --  15th jan 2006

; A little program to solve ascii mazes.

(require "../integer-graphs-with-edges.scm"
         (prefix map: (planet "finite-map.scm" ("soegaard" "galore.plt")))
         (lib "42.ss" "srfi")
         (lib "26.ss" "srfi"))

; Here is an example maze:

(define maze1
  (list
   "    _______________________  "
   "   |     |        |       E| "
   "   |__   |_____   |  ______| "
   "   |        |        |     | "
   "   |  ___   |_____   |     | "
   "   |  |  |           |  |  | "
   "   |  |  |  ___      |  |  | "
   "   |  |        |  |  |  |  | "
   "   |  |_____   |__|__|__|  | "
   "   |S |                    | "
   "   |__|____________________| "
   ))

(define (chop s)
  (if (string? s)
      (let ([len (string-length s)])
        (if (char=? (string-ref s (- len 1)) #\return)
            (substring s 0 (- len 1))
            s))
      s))

(define (read-line/chop p)
  (chop (read-line p)))

(define (read-maze filename)
  (let* ([port (open-input-file filename)]
         [maze (list-ec (:port line port read-line/chop) line)])
    (close-input-port port)
    maze))

(define maze2 (read-maze "mazes/input2.txt"))
(define maze3 (read-maze "mazes/input3.txt"))
(define maze4 (read-maze "mazes/input4.txt"))
(define maze5 (read-maze "mazes/input5.txt"))
(define maze6 (read-maze "mazes/input6.txt"))


; There are four different types of characters
; in a maze drawing.

(define (char->type c)
  (case c
    [(#\space) 'free]
    [(#\S)     'start]
    [(#\E)     'end]
    [else      'wall]))

; Our goal is to find a path from S to E, and
; print out the path.

; We will represent the maze as a graph. Each character
; in the drawing becomes a vertex in the graph.

(define-struct vertex (id type row col) (make-inspector))

; The graph library represents each nodes as integers,
; so we give each vertex an id-number.
; We remember the row and column of each vertex, in order
; to draw solution of the maze later.

(define (solve maze)
  ; we will make an unoriented grap
  (define graph (make-graph #f))
  
  (define coordinate->vertex (map:empty))
  (define id->vertex (map:empty))
  
  (define row car)
  (define col cadr)

  (define start-id #f)
  (define end-id #f)
  
  ; while building the graph we use this helper to
  ; connect a vertex to its neighbours
  (define (connect-to-neighbours v r c)
    ; connect the vertex v in row r and col c with its non-wall neighbours
    (do-ec (:parallel
            (: dr '( 1 -1 -1  0 0  1 1 1))
            (: dc '(-1  0  1 -1 1 -1 0 1)))
           (if (or (= dr 0) (= dc 0)))  ; coment this line out if you want all 8 directions
           (:let n (map:lookup (list (+ r dr) (+ c dc)) coordinate->vertex))
           (if (and n (member (vertex-type n) '(free start end))))
           (add-edge graph (vertex-id v) (vertex-id n) 1)))


  ; 1. Run through all characters of the given maze and assign
  ;    coordinates and ids to all vertices
  (let ([id 0])
    (do-ec (: str (index r) maze)
           (: ch  (index c) str)
           (:let v (make-vertex id (char->type ch) r c))
           (begin
             (set! coordinate->vertex  (map:insert (list r c) v coordinate->vertex))
             (set! id->vertex          (map:insert id         v id->vertex))
             (set! id (+ id 1)))))

  ; 2. Build the graph and find the start and end vertices
  
  ; for each coordinate, make a vertex and connect it to its neighbours,
  ; remember the ids of the start and end vertex
  (map:fold/key (lambda (co v a)
                  (let ([type (vertex-type v)])
                    (when (member type '(free start end))
                      (connect-to-neighbours v (row co) (col co)))
                    (case type
                      [(start) (set! start-id (vertex-id v))]
                      [(end)   (set! end-id (vertex-id v))])))
                'ignore
                coordinate->vertex)
  
  (unless start-id
    (error "No start (S) found"))

  (unless end-id
    (error "No end (E) found"))
  
  ; 3. calculate shortest path from start to end
  (let* ([path (let-values 
                  ([(distances paths)
                    (single-source-minimum-path graph start-id)])
                 (vector-ref paths end-id))]
    
         ; 4. make the maze ready for printing
         [output (list->vector (map string-copy maze))])
    (do-ec (: e path)
           (:let v (map:lookup (edge-to e) id->vertex))
           (string-set! (vector-ref output (vertex-row v))
                        (vertex-col v)
                        #\*))
    (do-ec (: line output)
           (begin
             (display line)
             (newline)))))


;;; TEST

(for-each (lambda (maze)
            (display "---------------\n")
            (solve maze)
            (display "\n\n\n"))
  (list maze1 maze2 maze3 maze4 maze5 maze6))