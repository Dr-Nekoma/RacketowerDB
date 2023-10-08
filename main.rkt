#lang racket

(module+ test
  (require rackunit)
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
(module+ main
  (require racket/base)
  (require racket/serialize)
  (require racket/class)
  (require (submod RacketowerDB/io writer))
  (require (submod RacketowerDB/io reader))
  (require (submod RacketowerDB/language parser))
  (require (submod RacketowerDB/backend server))
  (require RacketowerDB/util)
  
  (require RacketowerDB/ast)
  
  (let* ((field-name (new field% [position 1]
                          [type (new type% [name 'VARCHAR]
                                     [byte-size 5])]))
         (field-editor (new field% [position 0]
                            [type (new type% [name 'VARCHAR]
                                       [byte-size 5])]))
         (table (new table% [fields (make-hash `(("NAME" . ,field-name)
                                                 ("EDITOR" . ,field-editor)))]))
         (procedure (new procedure%))
         (schema (make-hash (list)))
         (row1 `(("NAME" . ,(new string% [value "Nathan"]))
                 ("EDITOR" . ,(new string% [value "Visual Studio Code"]))))
         (row2 `(("NAME" . ,(new string% [value "Lemos"]))
                 ("EDITOR" . ,(new string% [value "Emacs"]))))
         (row3 `(("MODEL" . ,(new string% [value "Ford"]))
                 ("YEAR" . ,(new integer32% [value 1999]))))
         (row4 `(("MODEL" . ,(new string% [value "Abc"]))
                 ("YEAR" . ,(new integer32% [value 2013]))))
         (literal1 (new string% [value "potatoes"]))
         (literal2 (new integer32% [value 32]))
         (read-table (read-table-from-disk schema "PROGRAMMER"))
         )
    (println (get-field byte-size (get-field type (hash-ref (get-field fields read-table) "EDITOR"))))
    (hash-set! schema "TEST" procedure)
    (hash-set! schema "PROGRAMMER" table)
    (write-schema-to-disk schema)
    (set! schema (read-schema-from-disk "schema"))
    (println (get-field byte-size (get-field type (hash-ref (get-field fields (hash-ref schema "PROGRAMMER")) "NAME"))))
    (set! schema (write-rows-to-disk schema "PROGRAMMER" (list row1 row2)))
    (println (read-table-values-from-disk schema "PROGRAMMER"))
    ;; (define sample-input (open-input-string "CREATE RELATION CAR (MODEL STRING(5) YEAR INTEGER)"))
    ;; (port-count-lines! sample-input)
    ;; (write-table-to-disk table "PROGRAMMER")
    ;; (let* ((read-table (read-table-from-disk schema "PROGRAMMER"))
    ;;        (thing (parse (lambda () (mappings/tokens sample-input))))
    ;;        (new-table-name (car thing))
    ;;        (new-table (cdr thing)))
    ;;   (write-table-to-disk new-table new-table-name)
    ;;   (hash-set! schema "PROGRAMMER" read-table)
    ;;   (set! schema (write-rows-to-disk schema "PROGRAMMER" (list row1 row2)))
    ;;   (hash-set! schema new-table-name new-table)
    ;;   (set! schema (write-rows-to-disk schema new-table-name (list row3 row4)))
    ;;   (println schema)
      ))

  ;; (exit-handler)
  ;; (server-entrypoint)

  ;; (require racket/cmdline)
  ;; (define who (box "world"))
  ;; (command-line
  ;;   #:program "my-program"
  ;;   #:once-each
  ;;   [("-n" "--name") name "Who to say hello to" (set-box! who name)]
  ;;   #:args ()
  ;;   (printf "hello ~a~n" (unbox who))))
