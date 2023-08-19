#lang racket

(require racket/base)
(require threading)

(provide build-ndf-filename)

(define build-ndf-filename
  (lambda (#:data? [data? 'entity] name)
    (let ((path (case (list 'quote data?)
		  [('entity) "ndf/entities/"]
		  [('schema) "ndf/schemas/"]
		  [('data) "ndf/data/"]
		  [else (raise 'error-not-specified-datatype)])))
      (string-append path (string-append name ".ndf")))))

(module interfaces racket
  (provide serializable<%>)
  (provide bytable<%>)
  
  (define serializable<%>
    (interface () serialize deserialize))
  
  (define bytable<%>
    (interface () from-bytes to-byte-size)))

(module+ classes
  (require (submod ".." interfaces))
  
  (provide hashable%)
  
  (define hashable%
    (class* object% (serializable<%>)
      (abstract serialize)
      (abstract deserialize)
      (define/public (deserialize-hash-list byte-stream accumulator)
        (define (deserialize-name more-bytes)
          (let* ((name-size (integer-bytes->integer (subbytes more-bytes 0 4) #t))
                 (name (bytes->string/utf-8 (subbytes more-bytes 4 (+ 4 name-size)))))
            (cons name (+ 4 name-size))))
        (if (equal? byte-stream #"")
            accumulator
            (let* ((name-consumption (deserialize-name byte-stream))
                   (name-consumed (cdr name-consumption))
                   (name (car name-consumption))
                   (field-consumed (send this deserialize (subbytes byte-stream name-consumed))))
              (deserialize-hash-list
               (subbytes byte-stream (+ name-consumed field-consumed) (bytes-length byte-stream))
               (append accumulator (list (cons name this)))))))
      (define/public (serialize-hash-list named-values-list)
        (define (serialize-name name)
          (let* ((name-bytes (string->bytes/utf-8 name))
                 (name-size (integer->integer-bytes (bytes-length name-bytes) 4 #t)))
            (bytes-append name-size name-bytes)))
        (~>
         (map (lambda (named-value)
                (bytes-append
                 (serialize-name (car named-value))
                 (send (cdr named-value) serialize)))
              named-values-list)
         (bytes-join _ #"")))
      (super-new))))
