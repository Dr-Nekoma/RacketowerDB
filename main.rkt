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
  (require (rename-in RacketowerDB/ast (procedure? procedura?)))
  (require (submod RacketowerDB/io writer))
  (require (submod RacketowerDB/io reader))
  ;; (require (submod RacketowerDB/language parser))
  (require (submod RacketowerDB/backend server))
  (require RacketowerDB/util)
  
  (dynamic-require 'RacketowerDB/ast 0)

  (let* ((field-name (fyeld 0 (type 'VARCHAR 7)))
         (field-editor (fyeld 1 (type 'VARCHAR 5)))
         (field-year (fyeld 1 (type 'INTEGER 4)))
         (field-age (fyeld 2 (type 'INTEGER 4)))
         (constraint-1 
          #`(lambda (rows)
             (andmap (lambda (row)
                       (andmap (lambda (raw-field)
				 (let ((raw-name (car raw-field))
				       (raw-value (cdr raw-field)))
				   (if (equal? raw-name "AGE")
				       (>= (integer32-value raw-value) 50)
				     #t))) row)) rows)))
         (programmer-table (table 
                              "table" 
                              0 
                              (make-hash `(("NAME" . ,field-name)
                                           ("AGE" . ,field-age)
                                           ("EDITOR" . ,field-editor)))
                              (list constraint-1)))
        ;  (car-table (table "table" 0 (make-hash `(("MODEL" . ,field-name)
        ;                                           ("YEAR" . ,field-year)))))
         (procedure-test (procedure "procedure"))
         (schema (make-hash (list)))
         (row1 `(("NAME" . ,(stringl "Nathan"))
                 ("EDITOR" . ,(stringl "Visual Studio Code"))
                 ("AGE" . ,(integer32 123))))
         (row2 `(("NAME" . ,(stringl "Lemos"))
                 ("EDITOR" . ,(stringl "Emacs"))
                 ("AGE" . ,(integer32 100))))
         (row3 `(("MODEL" . ,(stringl "Ford"))
                 ("YEAR" . ,(integer32 1999))))
         (row4 `(("MODEL" . ,(stringl "Abc"))
                 ("YEAR" . ,(integer32 2013)))))
    (check-local-constraints programmer-table (list row1 row2))
;     (hash-set! schema "CAR" car-table)
;     (hash-set! schema "TEST" procedure-test)
;     (hash-set! schema "PROGRAMMER" programmer-table)
;     (write-schema-to-disk schema)
;     (set! schema (read-schema-from-disk "schema"))
;     (println schema)
;     (set! schema (write-rows-to-disk schema "PROGRAMMER" (list row1 row2)))
;     (println (read-table-values-from-disk schema "PROGRAMMER"))
;     (write-table-to-disk programmer-table "PROGRAMMER")
;     (let ((read-table (read-table-from-disk "PROGRAMMER")))
;       (hash-set! schema "PROGRAMMER" read-table)
;       (set! schema (write-rows-to-disk schema "PROGRAMMER" (list row1 row2)))
;       (println schema))
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
