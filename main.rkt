#lang racket
(require
  racket/serialize
  (rename-in RacketowerDB/ast (procedure? procedura?))
  (submod RacketowerDB/io writer)
  (submod RacketowerDB/io reader)
  ;(submod RacketowerDB/language parser)
  (submod RacketowerDB/backend server)
  RacketowerDB/bplustree
  RacketowerDB/util
  RacketowerDB/pagination)

;; review if this is needed for eval-syntax
(dynamic-require 'RacketowerDB/ast 0)

(module+ test
  (require rackunit)
  
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
                    (<= (integer32-value raw-value) 50)
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

  (define row0
    `(("NAME" . ,(stringl "Marinho"))
      ("EDITOR" . ,(stringl "Kakoune"))
      ("AGE" . ,(integer32 29))))
  
  (define row1
    `(("NAME" . ,(stringl "Nathan"))
      ("EDITOR" . ,(stringl "Visual Studio Code"))
      ("AGE" . ,(integer32 23))))

  (define row2
    `(("NAME" . ,(stringl "Lemos"))
      ("EDITOR" . ,(stringl "Emacs"))
      ("AGE" . ,(integer32 24))))

  (define row3
    `(("NAME" . ,(stringl "Magueta"))
      ("EDITOR" . ,(stringl "Emacs"))
      ("AGE" . ,(integer32 24))))

  (define row4
    `(("MODEL" . ,(stringl "Model X"))
      ("YEAR" . ,(integer32 2015))))

  (define row5
    `(("MODEL" . ,(stringl "Model S"))
      ("YEAR" . ,(integer32 2012))))

  (define row6
    `(("MODEL" . ,(stringl "R1S"))
      ("YEAR" . ,(integer32 2021))))

  (define row7
    `(("MODEL" . ,(stringl "Beetle"))
      ("YEAR" . ,(integer32 1938))))

  (define row8
    `(("MODEL" . ,(stringl "Mach 5"))
      ("YEAR" . ,(integer32 1967))))

  (define row9
    `(("MODEL" . ,(stringl "Mach 6"))
      ("YEAR" . ,(integer32 1967))))
    
  (hash-set! schema "PROGRAMMER" programmer-table)
  (hash-set! schema "CAR" car-table)
  (define programmer-list (list row0 row1 row2 row3))
  (define car-list (list row4 row5 row6 row7 row8 row9))

  (test-case
      "Write mocked data into disk with PROGRAMMER table"
    (set! schema (write-rows-to-disk schema "PROGRAMMER" programmer-list))
    (check-equal? (table-row-id (hash-ref schema "PROGRAMMER")) 4))
  
  (test-case
      "Write mocked data into disk with CAR table"
    (set! schema (write-rows-to-disk schema "CAR" car-list))
    (check-equal? (table-row-id (hash-ref schema "CAR")) 6))

  (test-case
      "Read written data from disk with CAR table"
    (check-equal? (read-table-values-from-disk schema "CAR") (map make-hash car-list)))

  (test-case
      "Check for constraints in the PROGRAMMER table"
    (check-true (check-local-constraints (hash-ref schema "PROGRAMMER") programmer-list)))

  (test-case
      "Check writing schema into disk"
    (write-schema-to-disk schema)
    (check-match (read-schema-from-disk "schema") schema))

  (test-case
      "Check writing table PROGRAMMER into disk"
    (write-table-to-disk programmer-table "PROGRAMMER")
    (check-match (read-table-from-disk "PROGRAMMER")  programmer-table))
  (test-case
      "Check writing table CAR into disk"
    (write-table-to-disk car-table "CAR")
    (check-match (read-table-from-disk "CAR") car-table))

  (test-case
      "Query integer values from persisted PROGRAMMER table"
    (check-equal? (search schema (query "PROGRAMMER" "AGE" 24)) (map make-hash (list row2 row3))))
  (test-case
      "Query integer values from persisted CAR table"
    (check-equal? (search schema (query "CAR" "YEAR" 1967)) (map make-hash (list row8 row9)))))

(module+ main
  (println "Welcome to RacketowerDB!")
  (println "TODO: Server yet to be implemented xD"))
