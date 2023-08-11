#lang racket

(require racket/base)
(provide build-ndf-filename)

(define (build-ndf-filename name)
  (string-append "ndf/" (string-append name ".ndf")))
