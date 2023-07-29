#lang racket

(define serializable<%>
  (interface () serialize deserialize))

(module+ types
  (require racket/base)
  (require racket/serialize)
  (require racket/class)
  (provide type%)
  (provide string%)
  (provide integer32%)
  
  (define type%
    (class object%
      (init-field [name null]
                  [size null])    
      (super-new)))

  (define literal%
    (class object%
      (init-field [value null])
      (super-new)))

  (define string%
    (class* literal% (serializable<%>)
      (define/public (serialize type)
        (let* ((size-of-type (get-field size type))
               (string-value (get-field value this))
               (size-of-string (string-length string-value)))
          (if (< size-of-type size-of-string)
              (string->bytes/utf-8 (substring string-value 0 size-of-type)) ;; Truncate
              (let ((dest-bytes (make-bytes size-of-type 0))
                    (serialized-string (string->bytes/utf-8 string-value)))
                (bytes-copy! dest-bytes 0 serialized-string)
                dest-bytes)))) ;; Padding
      ;; TODO: assign the value to the result of this later
      (define/public (deserialize byte-array)
        (bytes->string/utf-8 byte-array))
      (super-new)))

  (define integer32%
    (class* literal% (serializable<%>)
      (define/public (serialize _)
        (integer->integer-bytes (get-field value this) 4 #t))
      (define/public (deserialize byte-array)
        (integer-bytes->integer byte-array #t))
      (super-new))))

;; (define schema%
;;   (class entity%
;;     (define/public (fields-size)
;;       (let* ((fields (get-field fields this))
;;              (fields-values (hash-values fields)))
;;         (foldl (lambda (elem acc)
;;                  (let ((size (get-field size (get-field type elem))))
;;                    (+ acc size)))
;;                0 fields-values)))
;;     (init-field [row-id 0]
;;                 [fields (make-hash (list))])
;;     (super-new)))

(module+ entities
  (require racket/base)
  (require racket/class)
  (provide table%)
  (provide procedure%)
  (provide field%)

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
      (define/public (fields-size)
        (let* ((fields (get-field fields this))
               (fields-values (hash-values fields)))
          (foldl (lambda (elem acc)
                   (let ((size (get-field size (get-field type elem))))
                     (+ acc size)))
                 0 fields-values)))
      (init-field [row-id 0]
                  [fields (make-hash (list))])
      (super-new)))

  (define procedure%
    (class entity%
      (super-new))))
