#lang racket


;; (module MaguetaQL racket

(require racket/base)
(require racket/class)
(require racket/serialize)

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
    (field (position null)
           (type null))
    (super-new)))

(define table%
  (class entity%
    (field (row-id null)
           (fields (make-hash (list))))
    (super-new)))

(define procedure%
  (class entity%
    (super-new)))

(define (convert-literal table attribute-name literal)
  (let ((attribute (hash-ref (get-field fields table)
                             attribute-name
                             (lambda () #f))))
    (cond
      ((and attribute
            (is-a? literal string%)) (list (get-field position attribute)
                                           ())))))

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

