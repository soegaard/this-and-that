#lang scribble/manual
@(require (for-label scheme/base
                     scheme/gui
                     scheme/contract
                     "main.ss"))

@title{Chipmunk 2.5D Physics}
@author{@(author+email "Jay McCarthy" "jay@plt-scheme.org")}

An FFI to the @link["http://code.google.com/p/chipmunk-physics/"]{Chipmunk 2.5D rigid body physics engine}.

@defmodule[(planet jaymccarthy/chipmunk)]

This module re-exports @schememodname[(planet jaymccarthy/chipmunk/chipmunk-ffi)] with a few changes and helpers.

First, @schemeidfont{cpInitChipmunk} and @schemeidfont{cpResetShapeIdCounter} are called.

Second, the following are provided:

@defproc[(gen-collision-type) exact-nonnegative-integer?]

Returns a unique integer to use as a collision type.

@defproc[(cpSpaceAddCollisionPairFunc
          [space cpSpace?]
          [typea exact-nonnegative-integer?]
          [typeb exact-nonnegative-integer?]
          [callback
           (-> cpShape? cpShape? (vectorof cpContact?) cpFloat?
               boolean?)])
         void]

Adds a collision pair function where the callback has less work to do.

@defproc[(cpSpaceSetDefaultCollisionPairFunc
          [space cpSpace?]
          [callback
           (-> cpShape? cpShape? (vectorof cpContact?) cpFloat?
               boolean?)])
         void]

Sets the default collision pair function where the callback has less work to do.

@defproc[(cpvadd [x cpVect?] ...)
         cpVect?]

Variable arity @schemeidfont{cpvadd}.

@section{FFI}

@defmodule[(planet jaymccarthy/chipmunk/chipmunk-ffi)]

This provides a straight-forward transliteration of the Chipmunk API. Refer to its @link["http://code.google.com/p/chipmunk-physics/wiki/Documentation"]{documentation} and @link["http://code.google.com/p/chipmunk-physics/source/browse/#svn/trunk"]{headers} for more details.

