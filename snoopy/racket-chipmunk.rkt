#lang racket
(require 2htdp/universe 
         2htdp/image
         (for-syntax racket/syntax)
         "2d-tree.ss")

;;;
;;; Vectors
;;;

; The entities position, velocity, force, and, momentum are vectors.
; Since their units of measurment are different, we represent them
; as different types of vector-like structures.

(define-syntax (define-vector-struct stx)
  (syntax-case stx ()
    [(_ name)
     (let ([the-name (syntax-e #'name)])
       (with-syntax
           ([make-name         (format-id #'name "make-~a" the-name)]
            [name-x            (format-id #'name "~a-x" the-name)]
            [name-y            (format-id #'name "~a-y" the-name)]
            [name-add          (format-id #'name "~a-add" the-name)]
            [name-scale        (format-id #'name "~a-scale" the-name)]
            [name-scale-x      (format-id #'name "~a-scale-x" the-name)]
            [name-scale-y      (format-id #'name "~a-scale-y" the-name)]
            [name-reset-x      (format-id #'name "~a-reset-x" the-name)]
            [name-reset-y      (format-id #'name "~a-reset-y" the-name)]
            [update-name-x     (format-id #'name "update-~a-x" the-name)]
            [update-name-y     (format-id #'name "update-~a-y" the-name)]
            [name-norm-squared (format-id #'name "~a-norm-squared" the-name)])          
         #'(begin
             (define-struct name (x y) #:transparent)
             (define (name-add v w)
               (make-name (+ (name-x v) (name-x w)) (+ (name-y v) (name-y w))))
             (define (name-scale s v)
               (make-name (* s (name-x v)) (* s (name-y v))))
             (define (name-scale-x s v)
               (make-name (* s (name-x v)) (name-y v)))
             (define (name-scale-y s v)
               (make-name (name-x v) (name-y v)))
             (define (name-reset-x v)
               (make-name 0.0 (name-y v)))
             (define (name-reset-y v)
               (make-name (name-x v) 0.0))
             (define (update-name-x v vx)
               (make-name vx (name-y v)))
             (define (update-name-y v vy)
               (make-name (name-x v) vy))
             (define (name-norm-squared v)
               (let ([vx (name-x v)] [vy (name-y v)])
                 (+ (* vx vx) (* vy vy)))))))]))

(define-vector-struct position)
(define-vector-struct velocity)
(define-vector-struct force)
(define-vector-struct vect)

; The concept of Momentum is derived from the the mass and velocity.

(define momentum vect)

(define (momentum-x b)
  (* (body-mass b)
     (velocity-x (body-velocity b))))

(define (momentum-y b)
  (* (body-mass b)
     (velocity-y (body-velocity b))))

;;;
;;; Bodies
;;;

; A rigid body is represented as the position of its center of gravity,
; its velocity, the force acting on it, its mass and finally a max-velocity.

(define-struct body (position velocity force mass max-velocity) #:transparent)
; force                 = mass * acceleration    F=m*a
; momentum (of inertia) = mass * velocity        i=m*v   

(define (body-momentum b)
  (let ([m (body-mass b)]
        [v (body-velocity b)])
    (momentum (* m (velocity-x v))
              (* m (velocity-y v)))))

(define (create-body mass)
  (body (position 0.0 0.0)
        (velocity 0.0 0.0)
        (force 0.0 0.0)
        mass
        +inf.0))

(define (create-static-body)
  (create-body +inf.0))

(define (update-body-velocity b gravity dampingx dampingy dt)
  (match-let* ([(body p v f m mv) b]
               [(force gx gy) gravity]
               [(velocity vx vy) v]
               [(force fx fy) f])
    (let* ([vx (+ (* vx dampingx) (* dt (+ (/ fx m) gx)))]
           [vy (+ (* vy dampingy) (* dt (+ (/ fy m) gy)))]
           ; check agains maximum velocity
           [v^2   (+ (* vx vx) (* vy vy))]
           [scale (if (> v^2 (* mv mv)) (/ mv (sqrt v^2)) 1.0)]
           [vx    (* scale vx)]
           [vy    (* scale vy)]
           [v     (velocity vx vy)])
      (body p v f m mv))))

(define (update-body-position b dt)
  (match-let* ([(body p v f m mv) b]
               [(position px py) p]
               [(velocity vx vy) v])
    (let* ([px (+ px (* dt vx))]    ; NOTE: (+ vx v_biasx) in original Chipmunk
           [py (+ py (* dt vy))]
           [p  (position px py)])
      (body p v f m mv))))

(define (update-body body gravity damping-x damping-y dt)
  (update-body-position
   (update-body-velocity body gravity damping-x damping-y dt)
   dt))

(define (update-body-position-x b x)
  (match-let* ([(body p v f m mv) b]
               [(position px py) p])
    (let ([p (position x py)])
      (body p v f m mv))))

(define (update-body-position-y b y)
  (match-let* ([(body p v f m mv) b]
               [(position px py) p])
    (let ([p (position px y)])
      (body p v f m mv))))

(define (reset-body-forces b)
  (match-let* ([(body p v f m mv) b])
    (let ([f (force 0.0 0.0)])
      (body p v f m mv))))

(define (apply-force-to-body b force)
  (match-let* ([(body p v f m mv) b])
    (let ([f (force-add f force)])
      (body p v f m mv))))  ; NOTE: original has torque

(define (apply-impulse-to-body b impulse)
  (match-let* ([(body p v f m mv) b]
               [(velocity vx vy) v]
               [(vect ix iy) impulse])
    (let* ([vx (+ vx (/ ix m))]
           [vy (+ vy (/ iy m))]
           [v  (velocity vx vy)])
      (body p v f m mv))))

(define (body-kinetic-energy b)
  (match-let* ([(body p v f m mv) b])
    (* 0.5 (velocity-norm-squared v))))


;;;
;;; Shapes
;;;

(define-struct shape (x y) #:mutable #:transparent)

; (make-rectangle x-upper-left y-upper-left width height)
(define-struct (rectangle-shape shape) (dx dy) #:transparent #:mutable)
(define-struct (point-shape shape)     ()      #:transparent #:mutable)
(define-struct (circle-shape shape)    (r)     #:transparent #:mutable)

(define (update-shape-position! s b)
  ; use this before doing collision detection
  (match-let* ([(body p v f m mv) b]
               [(position px py) p])
    (match s
      [(rectangle-shape x y dx dy)
       (set-shape-x! s (- px (quotient dx 2)))
       (set-shape-y! s (- py (quotient dy 2)))]       
      [(point-shape x y)  
       (set-shape-x! s px)
       (set-shape-y! s py)]
      [(circle-shape x y r)
       (set-shape-x! s px)
       (set-shape-y! s py)]))
  s)

(define (shape-position s)
  (match s
    [(rectangle-shape x y dx dy) (position x y)]           
    [(point-shape x y)           (position x y)]
    [(circle-shape x y r)        (position x y)]))

;;;
;;; Bounding Boxes
;;;

(define-struct bounding-box (left bottom right top) #:transparent)

(define (bounding-box-of s)
  (cond
    [(rectangle-shape? s) (bounding-box-of-rectangle s)]
    [(circle-shape? s)    (bounding-box-of-circle s)]
    ; [(point-shape? s)     (bounding-box-of-point s)] TODO
    [else (error 'bounding-box-of "unknown shape type" s)]))

(define (bounding-box-of-circle c)
  (match-let ([(circle-shape x y r) c])
    (bounding-box (- x r) (- y r) (+ x r) (+ y r))))

(define (bounding-box-of-rectangle r)
  (match-let ([(rectangle-shape x y dx dy) r])
    (bounding-box x (+ y dy) (+ x dx) y)))

(define (bounding-boxes-intersects? a b)
  (match-let ([(bounding-box al ab ar at) a]
              [(bounding-box bl bb br bt) b])
    (and (<= al br) (<= bl ar) (<= ab bt) (<= bb at))))

(define (bounding-boxes-intersects2 a   l b r t)
  (match-let ([(bounding-box al ab ar at) a])
    (and (<= al r) (<= l ar) (<= ab t) (<= b at))))

(define (bounding-box-contains? b o)
  ; does b contain o ?
  (match-let ([(bounding-box ol ob or ot) o]
              [(bounding-box bl bb br bt) b])
    (and (<= bl ol) (>= br or) (<= bb ob) (>= bt ot))))

(define (bounding-box-merge a b)
  ; smallest bounding mox containing both a and b
  (match-let ([(bounding-box al ab ar at) a]
              [(bounding-box bl bb br bt) b])
    (bounding-box (min al bl) (min ab bb) (max ar br) (max at bt))))

(define (bounding-box-area box)
  (match-let ([(bounding-box l b r t) box])
    (* (- r l) (- t b))))

(define (bounding-box-merged-area a b)
  (match-let ([(bounding-box al ab ar at) a]
              [(bounding-box bl bb br bt) b])
    (* (- (max ar br) (min al bl)) (- (max at bt) (min ab bb)))))

(define (bounding-box-merged-area2 a  bl bb br bt)
  (match-let ([(bounding-box al ab ar at) a])
    (* (- (max ar br) (min al bl)) (- (max at bt) (min ab bb)))))

;;;
;;; COLLISION DETECTION
;;;

(define (bounding-boxes-intersection? bb1 bb2)
  (match-let 
      ([(bounding-box l b r t) bb1]
       [(bounding-box L B R T) bb2])
    (not (or (< b T)     ; 1 over 2
             (> t B)     ; 1 under 2
             (< r L)     ; 1 left of 2
             (> l R))))) ; 1 right of 2

(define (colliding? s1 s2)
  (bounding-boxes-intersection? 
   (bounding-box-of s1) (bounding-box-of s2)))


(define-struct shape-corner (x y dir object) #:transparent)

(define (shape->corners s object)
  (match-let ([(bounding-box l b r t) (bounding-box-of s)])
    (list (shape-corner l t 'nw object)
          (shape-corner l b 'sw object)
          (shape-corner r t 'ne object)
          (shape-corner r b 'se object))))

(define (shape-corner-coord s)
  (vector (shape-corner-x s) (shape-corner-y s)))

(define (shapes->2d-tree shapes objects)
  (build-2d-tree shape-corner-coord 
                 (append-map shape->corners shapes objects)))

(define (objects-in-binding-box-of shape 2d-tree)
  (match-let ([(bounding-box l b r t) (bounding-box-of shape)])
    (map (lambda (sc) 
           (match-let ([(shape-corner x y dir object) sc])
             (list object dir (position x y))))
         (objects-in-2d-range shape-corner-coord 2d-tree  l r (min t b) (max t b)))))

;;;
;;; Sprites
;;; 

(define-struct sprite (bitmap) #:transparent)


;;;
;;; General Constants
;;;

(define gravity (make-force 0.0 10.0))
(define dt 0.1)
(define damping 0.99)


;;;
;;; Background
;;; 

(define WIDTH 1400)
(define HEIGHT 700)
(define GRASS-HEIGHT 200)
(define GRASS-LEVEL (- HEIGHT GRASS-HEIGHT))

(define white-screen (empty-scene WIDTH HEIGHT))
(define grass (rectangle WIDTH GRASS-HEIGHT "solid" "green"))
(define background (underlay/xy white-screen 
                                0  GRASS-LEVEL
                                grass))

;;;
;;; Snoopy
;;;
(define snoopy-image  (scale 2 (bitmap/file "snoopy.png")))
(define snoopy-sprite (sprite snoopy-image))
(define snoopy-width  (image-width snoopy-image))
(define snoopy-height (image-height snoopy-image))
(define initial-snoopy-body   (body (position snoopy-width (- GRASS-LEVEL (quotient snoopy-height 2)))
                                    (velocity 0.0 0.0) gravity 1.0 +inf.0))
(define snoopy-shape  (rectangle-shape 0.0 0.0 snoopy-width snoopy-height))

(define (snoopy-in-air? b)
  (update-shape-position! snoopy-shape b)
  (and (not snoopy-on-platform?)
       (< (+ (shape-y snoopy-shape) snoopy-height)
          GRASS-LEVEL)))


(define snoopy-on-grass-y    (- GRASS-LEVEL (quotient snoopy-height 2)))
(define snoopy-on-platform?  #f)

;;;
;;; PLATFORM
;;;

(define PLATFORM-LEVEL 400)
(define platform-height 50)
(define platform-width  (- snoopy-width 2))
(define platform-image (rectangle platform-width platform-height "solid" "orange"))
(define platform-body  (body (position 500 (+ PLATFORM-LEVEL (quotient platform-height 2)))
                             (velocity 0.0 0.0) (force 0.0 0.0) 1.0 +inf.0))
(define platform-shape (rectangle-shape 0.0 0.0 platform-width platform-height))
(define snoopy-on-platform-y (- PLATFORM-LEVEL (quotient snoopy-height 2)))


;;;
;;; BLACK BOX
;;;

(define black-box-size 50)
(define black-box-image (rectangle black-box-size black-box-size "solid" "black"))
(define initial-black-box-body  (body (position (- (quotient WIDTH 2) (quotient black-box-size 2))
                                                (- GRASS-LEVEL (quotient black-box-size 2)))
                                      (velocity -3.0 0.0) (force 0.0 0.0) 1.0 +inf.0))
(define black-box-shape (rectangle-shape 0.0 0.0 black-box-size black-box-size))




;;;
;;; World
;;;

(define-struct world (snoopy black-box collision?) #:transparent)

(define (update-world-snoopy w b)
  (struct-copy world w [snoopy b]))

(define (update-world-black-box w b)
  (struct-copy world w [black-box b]))

(define (update-world-collision w t)
  (struct-copy world w [collision? t]))



;;;
;;; Drawing
;;;

(define (draw-on-scene scene p image)
  (define (restrain-x x) (min WIDTH (max 0 x)))
  (define (restrain-y y) (min HEIGHT (max 0 y)))
  #;(underlay/xy scene 
                 (restrain-x (position-x p))
                 (restrain-y (position-y p))
                 image)
  (place-image image 
               (position-x p)
               (position-y p)
               scene))

(define (draw-snoopy scene w)
  (draw-on-scene scene 
                 (body-position (world-snoopy w))
                 snoopy-image))

(define (draw-black-box scene w)
  (draw-on-scene scene (body-position (world-black-box w)) black-box-image))

(define (draw-platform scene)
  (draw-on-scene scene (body-position platform-body) platform-image))

(define (draw-if-collision scene w)
  (if (world-collision? w)
      (draw-on-scene scene 
                     (position (quotient WIDTH 2) (quotient HEIGHT 2))
                     (text "COLLISION" 30 "red"))
      scene))

(define (draw-bounding-box b scene)
  (draw-on-scene scene 
                 (position (/ (+ (bounding-box-left b) (bounding-box-right b)) 2)
                           (/ (+ (bounding-box-top b) (bounding-box-bottom b)) 2))
                 (rectangle (- (bounding-box-right b) (bounding-box-left b))
                            (- (bounding-box-bottom b) (bounding-box-top b))
                            "outline" "red")))

(define (draw-bounding-boxes scene bs)
  (foldl draw-bounding-box scene bs))

(define (draw-body b scene)
  (draw-on-scene scene 
                 (body-position b)
                 (circle 2 "outline" "blue")))

(define (draw-bodies scene bs)
  (foldl draw-body scene bs))

(define (draw-body-color b col scene)
  (draw-on-scene scene 
                 (body-position b)
                 (circle 2 "outline" col)))


;;;
;;; HANDLERS
;;; 

(define (draw w)
  (draw-body-color 
   (world-snoopy w) "brown"
   (draw-bodies
    (draw-bounding-boxes
     (draw-if-collision
      (draw-snoopy 
       (draw-black-box 
        (draw-platform background) w) w) w)
     (map bounding-box-of (list snoopy-shape black-box-shape)))
    (list (world-snoopy w) (world-black-box w)))))

(define (snoopy-tick w b)
  (define (remove-downwards-movement b)
    (apply-impulse-to-body 
     b (momentum 0.0 (* (momentum-y b) -1.0))))
  (let ([b (update-body b gravity damping damping dt)])
    (cond 
      [snoopy-on-platform? 
       (update-body-position-y 
        (remove-downwards-movement b)
        snoopy-on-platform-y)]
      [(snoopy-in-air? b) 
       b]      
      [else
       (update-body-position-y  
        (remove-downwards-movement b) 
        snoopy-on-grass-y)])))

(define (black-box-tick w b)
  (let ([b (update-body b (force 0.0 0.0) 1.0 1.0 dt)])
    (update-shape-position! black-box-shape b)
    (if (or (< (shape-x black-box-shape) 0)
            (> (shape-x black-box-shape) (- WIDTH 100)))
        (struct-copy body b [force (force-scale 1.0 (body-force b))])
        b)))

(define-syntax (when=> stx)
  (syntax-case stx ()
    [(_ test expr)
     #'(let ([t test])
         (when t
           (expr t)))]))

(define (tick w)  
  (let* (; snoopy
         [sb (snoopy-tick w (world-snoopy w))]         
         [w  (update-world-snoopy w sb)]
         ; black-box
         [bb (black-box-tick w (world-black-box w))]
         [w  (update-world-black-box w bb)])
    (update-shape-position! snoopy-shape sb)
    (update-shape-position! black-box-shape bb)
    (update-shape-position! platform-shape platform-body)
    ; collisions
    (let ([object-tree (shapes->2d-tree 
                        (list black-box-shape platform-shape)
                        (list bb platform-body))])
      (let ([os (objects-in-binding-box-of snoopy-shape object-tree)])
        ; os = (listof enemy-body dir position)              
        (when=> (findf (λ (info) (eq? (car info) bb)) os)
                (match-lambda 
                  [(list body dir pos) 'collision-with-black-box]))
        (when (findf (λ (info) (eq? (car info) platform-body)) os)
          (let ([dirs (map second (filter (λ (info) (eq? (car info) platform-body)) os))])
            (display dirs) (newline)
            (set! snoopy-on-platform? #t)))
        (when (not (findf (λ (info) (eq? (car info) platform-body)) os))
          (set! snoopy-on-platform? #f))))
    ; 
    (update-world-collision w (colliding? snoopy-shape black-box-shape))))


(define (handle-pad-event w pe)
  (let ([b (world-snoopy w)])
    (match-let ([(body p v f m mv) b])      
      (define (handle-left/right dir)  
        (let* ([vx (cond
                     [(snoopy-in-air? b) (velocity-x v)]
                     [(eq? dir 'left)    -40.0]
                     [(eq? dir 'right)   +40.0])]
               [v (update-velocity-x v vx)])
          (update-world-snoopy w (struct-copy body b [velocity v]))))
      
      (define (handle-jump)
        (let ([siy (if (snoopy-in-air? b) 0.0 -10.0)])
          (update-world-snoopy 
           w (apply-impulse-to-body 
              b (momentum 0.0 (* siy m (force-y f)))))))
      
      (case (string->symbol pe)
        [(left a)     (handle-left/right 'left)]
        [(right d)    (handle-left/right 'right)]
        [(up w | |)   (handle-jump)]
        [else         w]))))



;;;
;;; BIG BANG
;;; 

(big-bang (world initial-snoopy-body initial-black-box-body #f)
          (on-tick tick 0.02)
          (to-draw draw)
          (on-pad  handle-pad-event)
          ; (state #t)
          (name "Snoopy"))
