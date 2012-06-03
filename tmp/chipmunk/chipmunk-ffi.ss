#lang scheme

(require scheme/foreign
         scheme/runtime-path)
(unsafe!)

; FFI
(define chipmunk-lib (ffi-lib "libchipmunk"))

(define-syntax-rule (define-chipmunk obj typ)
  (define obj (get-ffi-obj 'obj chipmunk-lib typ)))
(define-syntax-rule (define-chipmunk* typ obj ...)
  (begin (define-chipmunk obj typ)
         ...))

;; Main
(define-chipmunk
  cpInitChipmunk
  (_fun -> _void))

(define _cpFloat _double)
(define cpFloat? real?)
(define _cpDataPointer _pointer) ; XXX
(define _size_t _ulong) ; XXX
(define _cpHashValue _size_t)

;; Chipmunk Vectors
(define-cstruct _cpVect 
  ([x _cpFloat]
   [y _cpFloat]))

(define (cpv x y) (make-cpVect x y))
(define cpvzero (cpv 0.0 0.0))

(define-chipmunk* 
  (_fun _cpVect _cpVect -> _cpVect)
  #;cpvadd #;cpvsub #;cpvproject #;cpvrotate #;cpvunrotate)
(define-chipmunk* 
  (_fun _cpVect _cpVect _cpFloat -> _cpVect)
  #;cpvlerp)
(define-chipmunk* 
  (_fun _cpVect _cpVect _cpFloat -> _bool)
  #;cpvnear)
(define-chipmunk* 
  (_fun _cpVect _cpFloat -> _cpVect)
  #;cpvmult #;cpvclamp)
(define-chipmunk* 
  (_fun _cpVect _cpVect -> _cpFloat)
  #;cpvdot #;cpvcross #;cpvdist #;cpvdistsq)
(define-chipmunk* 
  (_fun _cpVect -> _cpVect)
  #;cpvneg #;cpvperp #;cpvnormalize #;cpvnormalize_safe)
(define-chipmunk* 
  (_fun _cpVect -> _cpFloat)
  cpvlength #;cpvlengthsq cpvtoangle)
(define-chipmunk* 
  (_fun _cpFloat -> _cpVect)
  cpvforangle)

(define (cpvadd v1 v2)
  (cpv (+ (cpVect-x v1) (cpVect-x v2))
       (+ (cpVect-y v1) (cpVect-y v2))))
(define (cpvneg v1)
  (cpv (* -1 (cpVect-x v1)) (* -1 (cpVect-y v1))))
(define (cpvsub v1 v2)
  (cpv (- (cpVect-x v1) (cpVect-x v2))
       (- (cpVect-y v1) (cpVect-y v2))))
(define (cpvmult v1 s)
  (cpv (* (cpVect-x v1) s)
       (* (cpVect-y v1) s)))
(define (cpvdot v1 v2)
  (+ (* (cpVect-x v1) (cpVect-x v2))
     (* (cpVect-y v1) (cpVect-y v2))))
(define (cpvcross v1 v2)
  (- (* (cpVect-x v1) (cpVect-y v2))
     (* (cpVect-y v1) (cpVect-x v2))))
(define (cpvperp v1)
  (cpv (* -1 (cpVect-y v1)) (cpVect-x v1)))
(define (cpvrperp v1)
  (cpv (cpVect-y v1) (* -1 (cpVect-x v1))))
(define (cpvproject v1 v2)
  (cpvmult v2 (/ (cpvdot v1 v2) (cpvdot v2 v2))))
(define (cpvrotate v1 v2)
  (cpv (- (* (cpVect-x v1) (cpVect-x v2))
          (* (cpVect-y v1) (cpVect-y v2)))
       (+ (* (cpVect-x v1) (cpVect-y v2))
          (* (cpVect-y v1) (cpVect-x v2)))))
(define (cpvunrotate v1 v2)
  (cpv (+ (* (cpVect-x v1) (cpVect-x v2))
          (* (cpVect-y v1) (cpVect-y v2)))
       (- (* (cpVect-y v1) (cpVect-x v2))
          (* (cpVect-x v1) (cpVect-y v2)))))
(define (cpvlengthsq v)
  (cpvdot v v))
(define (cpvlerp v1 v2 t)
  (cpvadd (cpvmult v1 (- 1.0 t)) (cpvmult v2 t)))
(define (cpvnormalize v)
  (cpvmult v (/ 1.0 (cpvlength v))))
(define (cpvnormalize_safe v)
  (if (and (zero? (cpVect-x v))
           (zero? (cpVect-y v)))
      cpvzero
      (cpvnormalize v)))
(define (cpvclamp v len)
  (if (> (cpvdot v v)
         (* len len))
      (cpvmult (cpvnormalize v) len)
      v))
(define (cpvdist v1 v2)
  (cpvlength (cpvsub v1 v2)))
(define (cpvdistsq v1 v2)
  (cpvlengthsq (cpvsub v1 v2)))
(define (cpvnear v1 v2 dist)
  (< (cpvdistsq v1 v2)
     (* dist dist)))
(define (cpv->string v)
  (format "(~a,~a)" (cpVect-x v) (cpVect-y v)))

;; Chipmunk bounding boxes
(define-cstruct _cpBB 
  ([l _cpFloat]
   [b _cpFloat]
   [r _cpFloat]
   [t _cpFloat]))

(define-chipmunk* 
  (_fun _cpBB _cpBB -> _bool)
  #;cpBBintersects #;cpBBcontainsBB)
(define-chipmunk* 
  (_fun _cpBB _cpVect -> _bool)
  #;cpBBcontainsVect)
(define-chipmunk* 
  (_fun _cpBB _cpVect -> _cpVect)
  cpBBClampVect cpBBWrapVect)

(define (cpBBintersects a b)
  (and (<= (cpBB-l a) (cpBB-r b))
       (<= (cpBB-l b) (cpBB-r a))
       (<= (cpBB-b a) (cpBB-t b))
       (<= (cpBB-b b) (cpBB-t a))))
(define (cpBBcontainsBB bb other)
  (and (< (cpBB-l bb) (cpBB-l other))
       (> (cpBB-r bb) (cpBB-r other))
       (< (cpBB-b bb) (cpBB-b other))
       (> (cpBB-t bb) (cpBB-t other))))
(define (cpBBcontainsVect bb v)
  (and (< (cpBB-l bb) (cpVect-x v))
       (> (cpBB-r bb) (cpVect-x v))
       (< (cpBB-b bb) (cpVect-y v))
       (> (cpBB-t bb) (cpVect-y v))))

;; Chipmunk rigid bodies
(define _cpBodyVelocityFunc
  (_fun _pointer ; XXX cpBody
        _cpVect
        _cpFloat
        _cpFloat
        ->
        _void))

(define _cpBodyPositionFunc
  (_fun _pointer ; XXX cpBody
        _cpFloat
        ->
        _void))

(define-cstruct _cpBody 
  (; Integration Functions
   [velocity_func _cpBodyVelocityFunc]
   [position_func _cpBodyPositionFunc]
   ; Mass Properties
   [m _cpFloat]
   [m_inv _cpFloat]
   [i _cpFloat]
   [i_inv _cpFloat]
   ; Positional Properties
   [p _cpVect]
   [v _cpVect]
   [f _cpVect]
   [a _cpFloat]
   [w _cpFloat]
   [t _cpFloat]
   [rot _cpVect]
   ; User Definable Fields
   [data _cpDataPointer]
   ; Internally Used Fields
   [v_bias _cpVect]
   [w_bias _cpFloat]
   ))

(define-chipmunk
  cpBodyAlloc
  (_fun -> _cpBody-pointer))
(define-chipmunk
  cpBodyInit
  (_fun (_ptr io _cpBody)
        _cpFloat _cpFloat
        -> _cpBody-pointer))
(define-chipmunk
  cpBodyNew
  (_fun _cpFloat _cpFloat
        -> _cpBody-pointer))
(define-chipmunk*
  (_fun _cpBody-pointer -> _void)
  cpBodyDestroy cpBodyFree)
(define-chipmunk*
  (_fun _cpBody-pointer _cpFloat -> _void) 
  cpBodySetMass cpBodySetMoment cpBodySetAngle)

;;; Integration Functions
#;(define-chipmunk
    cpBodySlew
    (_fun _cpBody-pointer
          _cpVect
          _cpFloat
          ->
          _void))
(define-chipmunk
  cpBodyUpdateVelocity
  _cpBodyVelocityFunc)
(define-chipmunk
  cpBodyUpdatePosition
  _cpBodyPositionFunc)

;;; Coordinate Conversion Functions
(define-chipmunk*
  (_fun _cpBody-pointer _cpVect -> _cpVect)
  #;cpBodyLocal2World
  #;cpBodyWorld2Local)

(define (cpBodyLocal2World body* v)
  (cpvadd (cpBody-p body*)
          (cpvrotate v (cpBody-rot body*))))
(define (cpBodyWorld2Local body* v)
  (cpvunrotate (cpvsub v (cpBody-p body*)) 
               (cpBody-rot body*)))

;;; Applying Forces and Torques
(define-chipmunk*
  (_fun _cpBody-pointer _cpVect _cpVect -> _void)
  #;cpBodyApplyImpulse
  cpBodyApplyForce)

(define (cpBodyApplyImpulse body j r)
  (set-cpBody-v! body (cpvadd (cpBody-v body) (cpvmult j (cpBody-m_inv body))))
  (set-cpBody-w! body (+ (cpBody-w body) (* (cpBody-i_inv body) (cpvcross r j)))))

(define-chipmunk
  cpBodyResetForces
  (_fun _cpBody-pointer -> _void))
#;(define-chipmunk
  cpApplyDampedSpring
  (_fun _cpBody-pointer _cpBody-pointer
        _cpVect _cpVect
        _cpFloat _cpFloat _cpFloat _cpFloat
        -> _void))

;; Chipmunk collision shapes
(define _cpCollisionType _uint)
(define _cpLayers _uint)
(define _cpGroup _uint)

(define-cstruct _cpSegmentQueryInfo 
  ([shape _pointer] ; XXX _cpShape-pointer/null
   [t _cpFloat]
   [n _cpVect]))

(define _cpShapeType
  (_enum '(CP_CIRCLE_SHAPE CP_SEGMENT_SHAPE CP_POLY_SHAPE CP_NUM_SHAPES)))

(define-cstruct _cpShapeClass 
  ([type _cpShapeType]
   [cacheData _pointer] ; XXX
   [destroy _pointer] ; XXX
   [pointQuery _pointer] ; XXX
   [segmentQuery _pointer] ; XXX
   ))

(define-cstruct _cpShape
  ([klass _cpShapeClass-pointer]
   [body _cpBody-pointer]
   [bb _cpBB]
   ; Surface properties.
   [e _cpFloat]
   [u _cpFloat]
   [surface_v _cpVect]
   ; User Definable Fields
   [data _cpDataPointer]
   [collision_type _cpCollisionType]
   [group _cpGroup]
   [layers _cpLayers]
   ; Internally Used Fields
   [id _cpHashValue]))
  
(define-chipmunk*
  (_fun _cpShape-pointer -> _void)
  cpShapeDestroy cpShapeFree)
(define-chipmunk
  cpShapeCacheBB
  (_fun _cpShape-pointer -> _cpBB))
(define-chipmunk
  cpResetShapeIdCounter
  (_fun -> _void))

;;; Circle Shapes
(define-cstruct (_cpCircleShape _cpShape)
  ([c _cpVect]
   [r _cpFloat]
   [tc _cpVect]))

(define-chipmunk cpCircleShapeAlloc
  (_fun -> _cpCircleShape-pointer))
(define-chipmunk cpCircleShapeInit
  (_fun _cpCircleShape-pointer
        _cpBody-pointer
        _cpVect _cpFloat
        -> _cpCircleShape-pointer))
(define-chipmunk cpCircleShapeNew
  (_fun _cpBody-pointer
        _cpFloat _cpVect
        -> _cpCircleShape-pointer))

;;; Segment Shapes
(define-cstruct (_cpSegmentShape _cpShape)
  ([a _cpVect]
   [b _cpVect]
   [n _cpVect]
   [r _cpFloat]
   [ta _cpVect]
   [tb _cpVect]
   [tn _cpVect]))

(define-chipmunk cpSegmentShapeAlloc
  (_fun -> _cpSegmentShape-pointer))
(define-chipmunk cpSegmentShapeInit
  (_fun _cpSegmentShape-pointer
        _cpBody-pointer
        _cpVect _cpVect _cpFloat
        -> _cpSegmentShape-pointer))
(define-chipmunk cpSegmentShapeNew
  (_fun _cpBody-pointer
        _cpVect _cpVect _cpFloat
        -> _cpSegmentShape-pointer))

;;; Polygon Shapes
(define-cstruct _cpPolyShapeAxis
  ([n _cpVect]
   [d _cpFloat]))
(define-cstruct (_cpPolyShape _cpShape)
  ([numVerts _int]
   [verts _cpVect-pointer]
   [axes _cpPolyShapeAxis-pointer]
   [tVerts _cpVect-pointer]
   [tAxes _cpPolyShapeAxis-pointer]))   

(define-chipmunk cpPolyShapeAlloc
  (_fun -> _cpPolyShape-pointer))
(define-chipmunk cpPolyShapeInit
  (_fun _cpPolyShape-pointer
        _cpBody-pointer (numVerts : _int = (vector-length v)) (v : (_vector i _cpVect)) _cpVect
        -> _cpPolyShape-pointer))
(define-chipmunk cpPolyShapeNew
  (_fun _cpBody-pointer (numVerts : _int = (vector-length v)) (v : (_vector i _cpVect)) _cpVect
        -> _cpPolyShape-pointer))
(define-chipmunk cpPolyShapeGetNumVerts
  (_fun _cpPolyShape-pointer -> _int))
(define-chipmunk cpPolyShapeGetVert
  (_fun _cpPolyShape-pointer _int -> _cpVect))

;; Chipmunk joints
(define-cpointer-type _cpConstraint-pointer)

(define-chipmunk*
  (_fun _cpConstraint-pointer -> _void)
  cpConstraintDestroy cpConstraintFree)

;;; Pin Joints
(define-cpointer-type _cpPinJoint-pointer)
(define-chipmunk
  cpPinJointAlloc
  (_fun -> _cpPinJoint-pointer))
(define-chipmunk
  cpPinJointInit
  (_fun _cpPinJoint-pointer _cpBody-pointer _cpBody-pointer _cpVect _cpVect -> _cpPinJoint-pointer))
(define-chipmunk
  cpPinJointNew
  (_fun _cpBody-pointer _cpBody-pointer _cpVect _cpVect -> _cpConstraint-pointer))

;;; Slide Joints
(define-cpointer-type _cpSlideJoint-pointer)
(define-chipmunk
  cpSlideJointAlloc
  (_fun -> _cpSlideJoint-pointer))
(define-chipmunk
  cpSlideJointInit
  (_fun _cpSlideJoint-pointer _cpBody-pointer _cpBody-pointer _cpVect _cpVect _cpFloat _cpFloat -> _cpSlideJoint-pointer))
(define-chipmunk
  cpSlideJointNew
  (_fun _cpBody-pointer _cpBody-pointer _cpVect _cpVect _cpFloat _cpFloat -> _cpConstraint-pointer))

;;; Pivot Joints
(define-cpointer-type _cpPivotJoint-pointer)
(define-chipmunk
  cpPivotJointAlloc
  (_fun -> _cpPivotJoint-pointer))
(define-chipmunk
  cpPivotJointInit
  (_fun _cpPivotJoint-pointer _cpBody-pointer _cpBody-pointer _cpVect -> _cpPivotJoint-pointer))
(define-chipmunk
  cpPivotJointNew
  (_fun _cpBody-pointer _cpBody-pointer _cpVect -> _cpConstraint-pointer))

;;; Groove Joints
(define-cpointer-type _cpGrooveJoint-pointer)
(define-chipmunk
  cpGrooveJointAlloc
  (_fun -> _cpGrooveJoint-pointer))
(define-chipmunk
  cpGrooveJointInit
  (_fun _cpGrooveJoint-pointer _cpBody-pointer _cpBody-pointer _cpVect _cpVect _cpVect -> _cpGrooveJoint-pointer))
(define-chipmunk
  cpGrooveJointNew
  (_fun _cpBody-pointer _cpBody-pointer _cpVect _cpVect _cpVect -> _cpConstraint-pointer))

;; Chipmunk spaces
(define-cstruct _cpContact
  ([p _cpVect]
   [n _cpVect]
   [dist _cpFloat]
   [r1 _cpVect]
   [r2 _cpVect]
   [nMass _cpFloat]
   [tMass _cpFloat]
   [bounce _cpFloat]
   [jnAcc _cpFloat]
   [jtAcc _cpFloat]
   [jBias _cpFloat]
   [bias _cpFloat]
   [hash _cpHashValue]))
    
(define-cstruct _cpSpace
  ([iterations _int]
   [elasticIterations _int]
   [gravity _cpVect]
   [damping _cpFloat]))

(define-chipmunk cpSpaceAlloc
  (_fun -> _cpSpace-pointer))
(define-chipmunk cpSpaceInit
  (_fun _cpSpace-pointer _int -> _cpSpace-pointer))
(define-chipmunk cpSpaceNew
  (_fun -> _cpSpace-pointer))
#;(define-chipmunk*
  (_fun _cpSpace-pointer -> _void)
  cpSpaceDestroy cpSpaceFree cpSpaceFreeChildren)

(define-chipmunk*
  (_fun _cpSpace-pointer _cpShape-pointer -> _void)
  cpSpaceAddShape cpSpaceAddStaticShape
  cpSpaceRemoveShape cpSpaceRemoveStaticShape)
(define-chipmunk*
  (_fun _cpSpace-pointer _cpBody-pointer -> _void)
  cpSpaceAddBody cpSpaceRemoveBody)
(define-chipmunk*
  (_fun _cpSpace-pointer _cpConstraint-pointer -> _void)
  cpSpaceAddConstraint cpSpaceRemoveConstraint)

(define _cpCollFunc
  (_fun _cpShape-pointer _cpShape-pointer
        _cpContact-pointer
        (how-many-contacts : _int)
        _cpFloat _cpDataPointer
        -> _bool))
#;(define-chipmunk
  cpSpaceAddCollisionPairFunc
  (_fun _cpSpace-pointer
        _cpCollisionType
        _cpCollisionType
        _cpCollFunc
        _cpDataPointer
        -> _void))
#;(define-chipmunk
  cpSpaceRemoveCollisionPairFunc
  (_fun _cpSpace-pointer
        _cpCollisionType
        _cpCollisionType
        -> _void))
#;(define-chipmunk
  cpSpaceSetDefaultCollisionPairFunc
  (_fun _cpSpace-pointer
        _cpCollFunc
        _cpDataPointer
        -> _void))

(define _cpSpacePointQueryFunc
  (_fun _cpShape-pointer
        _cpDataPointer
        ->
        _void))
(define-chipmunk
  cpSpacePointQuery
  (_fun _cpSpace-pointer _cpVect _cpLayers _cpLayers _cpSpacePointQueryFunc _cpDataPointer
        -> _void))
(define-chipmunk
  cpSpacePointQueryFirst
  (_fun _cpSpace-pointer _cpVect _cpLayers _cpLayers
        -> _cpShape-pointer))

#;(define-chipmunk*
  (_fun _cpSpace-pointer _cpFloat _int
        -> _void)
  cpSpaceResizeStaticHash cpSpaceResizeActiveHash)
#;(define-chipmunk
  cpSpaceRehashStatic
  (_fun _cpSpace-pointer -> _void))

(define-chipmunk
  cpSpaceStep
  (_fun _cpSpace-pointer _cpFloat -> _void))

;; Main
(define-chipmunk
  cpMomentForCircle
  (_fun _cpFloat _cpFloat _cpFloat _cpVect
        -> _cpFloat))
(define-chipmunk
  cpMomentForSegment
  (_fun _cpFloat _cpVect _cpVect
        -> _cpFloat))
(define-chipmunk
  cpMomentForPoly
  (_fun _cpFloat (numVerts : _int = (vector-length v)) (v : (_vector i _cpVect)) _cpVect
        -> _cpFloat))

(provide (all-defined-out))