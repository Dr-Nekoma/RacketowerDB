#lang info
(define collection "RacketowerDB")
(define deps '("racket"))
(define build-deps '("scribble-lib"
		     "racket-doc"
		     "rackunit-lib"
		     "threading-lib"
		     "beautiful-racket"))
(define scribblings '(("scribblings/RacketowerDB.scrbl" ())))
(define pkg-desc "A simple database")
(define version "1.0")
(define pkg-authors '(lemos magueta))
(define license '(MIT))
