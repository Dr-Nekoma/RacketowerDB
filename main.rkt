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
  (require RacketowerDB/ast)
 
  (require (submod RacketowerDB/ast entities))
  
  (let* ((field-name (new field% [position 1]
                          [type (new type% [name 'VARCHAR]
                                     [byte-size 30])]))
         (field-editor (new field% [position 0]
                            [type (new type% [name 'VARCHAR]
                                       [byte-size 30])]))
         (table (new table% [fields (make-hash `(("NAME" . ,field-name)
                                                 ("EDITOR" . ,field-editor)))]))
         (schema (make-hash `(("PROGRAMMER" . ,table))))
         (row `(("NAME" . ,(new string% [value "Nathan"]))
                ("EDITOR" . ,(new string% [value "Visual Studio Code"]))))
         (literal1 (new string% [value "potatoes"]))
         (literal2 (new integer32% [value 32])))
    (display (read-table-from-disk schema "PROGRAMMER"))))

  ;; (require racket/cmdline)
  ;; (define who (box "world"))
  ;; (command-line
  ;;   #:program "my-program"
  ;;   #:once-each
  ;;   [("-n" "--name") name "Who to say hello to" (set-box! who name)]
  ;;   #:args ()
  ;;   (printf "hello ~a~n" (unbox who))))
