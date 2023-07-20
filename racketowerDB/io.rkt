#lang racket

(module writer racket
  (require racket/base)
  (require "ast.rkt")
  (require threading)
  (provide write-row-to-disk)
    
  (define (convert-literal table attribute-name literal)
    (let* ((attribute (hash-ref (get-field fields table)
                                attribute-name))
           (type (get-field type attribute))
           (position (get-field position attribute)))
      `(,position . ,(send literal serialize type))))
  (define (convert-row table row)
    (~>
     (foldl (lambda (elem acc)
              (cons (convert-literal table (car elem) (cdr elem))
                    acc)) (list) row)
     (sort _ (lambda (a b) (< (car a) (car b))))
     (map cdr _)
     (bytes-join _ #"")))
  (define (write-row-to-disk schema table-name row)
    (let ((entity (hash-ref schema table-name)))
      (cond
        [(is-a? entity table%)
         (let* ((converted-row (convert-row entity row))
                (row-id (get-field row-id entity))
                (total-size (send entity fields-size))
                (off-set (* row-id total-size))
                (file-name (string-append "ndf/" (string-append table-name ".ndf")))
                (out (open-output-file file-name #:exists 'replace)))
           (write-bytes converted-row out)
           (close-output-port out))]
        [(is-a? entity procedure%) (println "Don't write procedures yet")]))))
