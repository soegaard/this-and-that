#lang readtable 
{} terminating-macro "clojure-maps-parser.rkt" parse
#lang racket
(require bind)

(def h {a 1 b 2 c 3})

(with ([h :hash])
  (h 'a))


