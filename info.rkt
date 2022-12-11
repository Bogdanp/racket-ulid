#lang info

(define license 'BSD-3-Clause)
(define version "1.0")
(define collection "ulid")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "rackcheck-lib"
                     "rackunit-lib"
                     "scribble-lib"))
(define scribblings '(("ulid.scrbl")))
