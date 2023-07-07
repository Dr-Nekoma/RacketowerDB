#lang racket


;; (module MaguetaQL racket

(require racket/base)
(require racket/class)
(require racket/serialize)
(require threading)

(define serializable<%>
  (interface () serialize deserialize))

(define literal%
  (class object%
    (init-field [value null])
    (super-new)))

(define string%
  (class* literal% (serializable<%>)
    (define/public (serialize)
      (string->bytes/utf-8 (get-field value this)))
    ;; TODO: assign the value to the result of this later
    (define/public (deserialize byte-array)
      (bytes->string/utf-8 byte-array))
    (super-new)))

(define integer32%
  (class* literal% (serializable<%>)
    (define/public (serialize)
      (integer->integer-bytes (get-field value this) 4 #t))
    (define/public (deserialize byte-array)
      (integer-bytes->integer byte-array #t))
    (super-new)))

(define entity%
  (class* object% (serializable<%>)
    (define/public (serialize)
      (serialize this))
    (define/public (deserialize byte-array)
      (deserialize byte-array))
    (super-new)))

(define field%
  (class object%
    (init-field [position null]
                [type null])
    (super-new)))

(define table%
  (class entity%
    (init-field [row-id null]
                [fields (make-hash (list))])
    (super-new)))

(define procedure%
  (class entity%
    (super-new)))

(define (convert-literal table attribute-name literal)
  (let ((attribute (hash-ref (get-field fields table)
                             attribute-name)))
    `(,(get-field position attribute) . ,(send literal serialize))))

(define (convert-row table row)
  (~>
   (foldl (lambda (elem acc)
            (cons (convert-literal table (car elem) (cdr elem))
                  acc))
          '() row)
   (sort _ (lambda (a b) (< (car a) (car b))))
   (map cdr _)))

;;;; Recipe
;; Get the entity and check if it is a table
;; Convert the row
;; Calculate the offset with row-id => row-id*(sum_of_all_types_in_fields_hash)
;; Note: Add padding to strings with less than type size and truncate bigger ones, preferably raising a warning
(define (write-row-to-disk (schema table-name row))
  (let ((entity (hash-ref )))))

(let* ((field-name (new field% [position 1]
                        [type 'VARCHAR30]))
       (field-editor (new field% [position 0]
                          [type 'VARCHAR30]))
       (table (new table% [fields (make-hash `(("NAME" . ,field-name)
                                               ("EDITOR" . ,field-editor)))])))
  ;; (println (convert-literal table "NAME" (new string% [value "I love potatoes"])))
  (println (convert-row table `(("NAME" . ,(new string% [value "Nathan"]))
                                ("EDITOR" . ,(new string% [value "Visual Studio Code"]))))))

;; (deserialize (serialize '('VARCHAR . 123)))

;; (provide Literal<%>
;;          String%)

;; )

(send (new string% [value "ABCDEF"])
      deserialize
      (send (new string% [value "ABCDEFG🦝"])
            serialize))

(module IO racket
  (define (abc a b c)
    (+ a b c)))

