#lang racket

(require racket/class)
(require racket/base)
(require racket/serialize)
(require threading)

(provide table%)
(provide procedure%)
(provide field%)
(provide type%)
(provide string%)
(provide integer32%)
(require RacketowerDB/util)
(require (submod RacketowerDB/util interfaces))
(require (submod RacketowerDB/util classes))

(define-serializable type%
  [class* object% (bytable<%> serializable<%>)
    (init-field [name null]
                [byte-size null])
    (define/public (from-bytes byte-stream)
      (let ((received-bytes-size (bytes-length byte-stream)))
        (if (eq? received-bytes-size byte-size)
            (case (list 'quote (string->symbol name))
              [('INTEGER) (new integer32% [value (integer-bytes->integer byte-stream #t)])]
              [('VARCHAR) (new string% [value (bytes->string/utf-8 byte-stream)])]
              [else (raise 'error-with-unknown-type-from-bytes)])
            (raise 'error-with-from-bytes-size-check))))
    (define/public (to-byte-size)
      byte-size)
    (define/public (serialize)
      (let* ((name-bytes (string->bytes/utf-8 (symbol->string name)))
             (name-length (integer->integer-bytes (bytes-length name-bytes) 1 #t))
             (byte-size-bytes (integer->integer-bytes byte-size 4 #t)))
        (bytes-append name-length name-bytes byte-size-bytes)))
    (define/public (deserialize byte-stream)
      (let* ((name-length (integer-bytes->integer (make-bytes 1 (bytes-ref byte-stream 0)) #t))
             (name-value (bytes->string/utf-8 (subbytes byte-stream 1 (+ 1 name-length))))
             (byte-size-value (integer-bytes->integer (subbytes byte-stream (+ 1 name-length) (+ 3 name-length)) #t)))
        (set-field! name this name-value)
        (set-field! byte-size this byte-size-value)
        (+ 5 name-length)))
      (super-new)])

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
      (set-field! value this (string-trim (bytes->string/utf-8 byte-array)))
      (bytes-length byte-array))
    (inherit-field value)
    (super-new)))

(define integer32%
  (class* literal% (serializable<%>)
    (define/public (serialize _)
      (integer->integer-bytes value 4 #t))
    (define/public (deserialize byte-array)
      (set-field! value this (integer-bytes->integer (subbytes byte-array 0 4) #t))
      4)
    (inherit-field value)
    (super-new)))

(define-serializable field%
  (class hashable%
    (init-field [position null]
                [type null])
    (define/override (serialize)
      (let ((position-bytes (integer->integer-bytes position 1 #t))
            (type-bytes (send type serialize)))
        (bytes-append position-bytes type-bytes)))
    (define/override (deserialize byte-stream)
      (let* ((position-value (integer-bytes->integer (make-bytes 1 (bytes-ref byte-stream 0)) #t))
             (type-bytes (subbytes byte-stream 1))
             (new-type (new type%))
             (type-consumed (send new-type deserialize type-bytes)))
        (set-field! type this new-type)
        (set-field! position this position-value)
        (+ 1 type-consumed)))
    (super-new)))

(define-serializable table%
  (class hashable%
    (define/public (fields-size)
      (let* ((fields-values (hash-values fields)))
        (foldl (lambda (elem acc)
                 (let ((size (get-field byte-size (get-field type elem))))
                   (+ acc size)))
               0 fields-values)))
    (init-field [row-id 0]
                [identifier "table%"]
                [fields (make-hash (list))])
    (define/override (serialize)
      (let* ((row-id-bytes (integer->integer-bytes row-id 4 #t))
             (new-field (make-object field%))
             (fields-list (hash->list fields)))
        (bytes-append row-id-bytes (send new-field serialize-hash-list fields-list))))
    (define/override (deserialize byte-array)
      (let* ((row-id-value (integer-bytes->integer (subbytes byte-array 0 4) #t))
             (new-field (make-object field%))
             (fields-value (make-hash (send new-field deserialize-hash-list (subbytes byte-array 4) '()))))
        (set-field! row-id this row-id-value)
        (set-field! fields this fields-value)
        (bytes-length byte-array)))
    (super-new)))
  
  (define-serializable procedure%
    (class hashable%
      (init-field [identifier "procedure%"])
      (define/override (serialize)
        (string->bytes/utf-8 "procedures' serialization is not yet implemented"))
      (define/override (deserialize byte-array)
        (println "procedures' deserialization is not yet implemented")
        (bytes-length byte-array))
      (super-new)))
