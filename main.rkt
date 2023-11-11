#lang racket

(module+ test
  (require rackunit)
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
{module+ main
  (require racket/base)
  (require racket/serialize)
  (require racket/class)
  (require (submod RacketowerDB/io writer))
  (require (submod RacketowerDB/io reader))
  ;; (require (submod RacketowerDB/language parser))
  (require (submod RacketowerDB/backend server))
  (require RacketowerDB/util)
  
  (require (except-in RacketowerDB/ast procedure?))
  
  (let* ((field-name (fyeld 0 (type 'VARCHAR 7)))
         (field-editor (fyeld 1 (type 'VARCHAR 5)))
         (field-year (fyeld 1 (type 'INTEGER 4)))
         (programmer-table (table "table" 0 (make-hash `(("NAME" . ,field-name)
                                                         ("EDITOR" . ,field-editor)))))
         (car-table (table "table" 0 (make-hash `(("MODEL" . ,field-name)
                                                  ("YEAR" . ,field-year)))))
         (procedure-test (procedure "procedure"))
         (schema (make-hash (list))))
    (hash-set! schema "CAR" car-table)
    (hash-set! schema "TEST" procedure-test)
    (hash-set! schema "PROGRAMMER" programmer-table)
    (write-schema-to-disk schema)
    (set! schema (read-schema-from-disk "schema"))
    (println schema)
    ;; (set! schema (write-rows-to-disk schema "PROGRAMMER" (list row1 row2)))
    ;; (println (read-table-values-from-disk schema "PROGRAMMER"))
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
      )}

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
