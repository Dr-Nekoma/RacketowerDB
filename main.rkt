#lang racket
(require
  racket/serialize
  (rename-in RacketowerDB/ast (procedure? procedura?))
  (submod RacketowerDB/io writer)
  (submod RacketowerDB/io reader)
  ;(submod RacketowerDB/language parser)
  (submod RacketowerDB/backend server)
  RacketowerDB/util)

;; review if this is needed for eval-syntax
(dynamic-require 'RacketowerDB/ast 0)

(module+ test
  (require rackunit)
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
(module+ main

  (define field-name (field 0 (type 'VARCHAR 7)))
  (define field-editor (field 1 (type 'VARCHAR 10)))
  (define field-year (field 1 (type 'INTEGER 4)))
  (define field-age (field 2 (type 'INTEGER 4)))


  (define constraint-1
    #`(lambda [rows]
        (andmap
          (lambda [row]
            (andmap
              (lambda [raw-field]
                (let [(raw-name (car raw-field))
                      (raw-value (cdr raw-field))]
                  (if (equal? raw-name "AGE")
                    (>= (integer32-value raw-value) 50)
                    #t)))
              row))
          rows)))

    (define constraint-2
    #`(lambda [rows]
        (andmap
          (lambda [row]
            (andmap
              (lambda [raw-field]
                (let [(raw-name (car raw-field))
                      (raw-value (cdr raw-field))]
                  (if (equal? raw-name "EDITOR")
                    (>= (string-length (stringl-value raw-value)) 5)
                    #t)))
              row))
          rows)))

  (define programmer-table
    (table "table" 0
      (make-hash `(("NAME" . ,field-name)
                   ("AGE" . ,field-age)
                   ("EDITOR" . ,field-editor)))
      (list constraint-1 constraint-2)))

  (define car-table
    (table "table" 0
      (make-hash `(("MODEL" . ,field-name)
                   ("YEAR" . ,field-year)))
      (list)))

  (define procedure-test (procedure "procedure"))
  (define schema (make-hash (list)))

  (define row1
    `(("NAME" . ,(stringl "Nathan"))
      ("EDITOR" . ,(stringl "Visual Studio Code"))
      ("AGE" . ,(integer32 123))))

  (define row2
    `(("NAME" . ,(stringl "Lemos"))
      ("EDITOR" . ,(stringl "Emacs"))
      ("AGE" . ,(integer32 100))))

  (define row3
    `(("MODEL" . ,(stringl "Ford"))
      ("YEAR" . ,(integer32 1999))))

  (define row4
    `(("MODEL" . ,(stringl "Abc"))
      ("YEAR" . ,(integer32 2013))))

  (check-local-constraints programmer-table (list row1 row2))
  (hash-set! schema "CAR" car-table)
  (hash-set! schema "TEST" procedure-test)
  (hash-set! schema "PROGRAMMER" programmer-table)
  (write-schema-to-disk schema)
  (set! schema (read-schema-from-disk "schema"))
  (check-local-constraints (hash-ref schema "PROGRAMMER") (list row1 row2))
  (println schema)
  (set! schema (write-rows-to-disk schema "PROGRAMMER" (list row1 row2)))
  (println (read-table-values-from-disk schema "PROGRAMMER"))
  (write-table-to-disk programmer-table "PROGRAMMER")
  (let ((read-table (read-table-from-disk "PROGRAMMER")))
    (hash-set! schema "PROGRAMMER" read-table)
    (set! schema (write-rows-to-disk schema "PROGRAMMER" (list row1 row2)))
    (println schema)))

  ;;(exit-handler)
  ;;(server-entrypoint)

  ;;(require racket/cmdline)
  ;;(define who (box "world"))
  ;;(command-line
  ;;  #:program "my-program"
  ;;  #:once-each
  ;;  [("-n" "--name") name "Who to say hello to" (set-box! who name)]
  ;;  #:args ()
  ;;  (printf "hello ~a~n" (unbox who))))
