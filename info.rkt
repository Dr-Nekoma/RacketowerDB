#lang info
(define collection "RacketowerDB")

(define pkg-desc "A simple database")
(define version "1.0")
(define pkg-authors '(lemos magueta))
(define license '(MIT))

(define deps
  '("racket"
    "threading-lib"
    "beautiful-racket"
    "struct-update-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))

(define compile-omit-files
  '("guix.scm"))

(define scribblings
  '(("scribblings/RacketowerDB.scrbl" ())))
