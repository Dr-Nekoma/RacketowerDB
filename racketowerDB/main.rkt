#lang racket

;; (module MaguetaQL racket

(require racket/base)
(require (submod "io.rkt" writer))
(require "ast.rkt")

(let* ((field-name (new field% [position 1]
                        [type (new type% [name 'VARCHAR]
                                         [size 30])]))
       (field-editor (new field% [position 0]
                          [type (new type% [name 'VARCHAR]
                                           [size 30])]))
       (table (new table% [fields (make-hash `(("NAME" . ,field-name)
                                               ("EDITOR" . ,field-editor)))]))
       (schema (make-hash `(("PROGRAMMER" . ,table))))
       (row `(("NAME" . ,(new string% [value "Nathan"]))
              ("EDITOR" . ,(new string% [value "Visual Studio Code"]))))
       (literal1 (new string% [value "potatoes"]))
       (literal2 (new integer32% [value 32])))
  (println (write-row-to-disk schema "PROGRAMMER" row)))
