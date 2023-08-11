#lang racket

(require racket/class)
(require racket/base)
(require racket/serialize)

(provide type%)
(provide string%)
(provide integer32%)

(define serializable<%>
  (interface () serialize deserialize))

(define bytable<%>
  (interface () from-bytes to-byte-size))

(define type%
  (class* object% (bytable<%> serializable<%>)
    (init-field [name null]
                [byte-size null])
    (define/public (from-bytes byte-stream)
      (let ((received-bytes-size (bytes-length byte-stream)))
        (if (eq? received-bytes-size byte-size)
            (case (list 'quote name)
              [('INTEGER) (new integer32% [value (integer-bytes->integer byte-stream #t)])]
              [('VARCHAR) (new string% [value (bytes->string/utf-8 byte-stream)])]
              [else (raise 'error-with-unknown-type-from-bytes)])
            (raise 'error-with-from-bytes-size-check))))
    (define/public (to-byte-size)
      byte-size)
    (define/public (serialize) this)
    (define/public (deserialize byte-stream) this)
      (super-new)))

(define literal%
  (class* object% (printable<%>)
    (init-field [value null])
    (define/public (custom-print port _)
      (print value port))
    (define/public (custom-write port)
      (write value port))
    (define/public (custom-display port)
      (display value port))
    (super-new)))

(define string%
  (class* literal% (serializable<%>)
    (define/public (serialize type)
      (let* ((size-of-type (get-field byte-size type))
             (size-of-string (string-length value)))
        (if (< size-of-type size-of-string)
            (string->bytes/utf-8 (substring value 0 size-of-type)) ;; Truncate
            (let ((dest-bytes (make-bytes size-of-type 0))
                  (serialyzed-string (string->bytes/utf-8 value)))
              (bytes-copy! dest-bytes 0 serialyzed-string)
              dest-bytes)))) ;; Padding
    (define/public (deserialize byte-array)
      (set-field! value this (string-trim (bytes->string/utf-8 byte-array))))
    (inherit-field value)
    (super-new)))

(define integer32%
  (class* literal% (serializable<%>)
    (define/public (serialize)
      (integer->integer-bytes (get-field value this) 4 #t))
    (define/public (deserialize byte-array)
      (set-field! value this (integer-bytes->integer byte-array #t)))
    (inherit-field value)
    (super-new)))

(module+ entities
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
    (class* object% (serializable<%>)
      (init-field [position null]
                  [type null])
      (define/public (serialize) this)
      (define/public (deserialize byte-array) this)
      (super-new)))

    (define table%
      (class entity%
        (define/public (fields-size)
          (let* ((fields-values (hash-values fields)))
            (foldl (lambda (elem acc)
                     (let ((size (get-field byte-size (get-field type elem))))
                       (+ acc size)))
                   0 fields-values)))
        (init-field [row-id 0]
                    [fields (make-hash (list))])
        (super-new)))

    (define procedure%
      (class entity%
        (super-new))))
