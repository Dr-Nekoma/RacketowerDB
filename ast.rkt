#lang racket

(provide
  check-local-constraints
  (struct+updaters-out table)
  fields-size
  (struct+updaters-out procedure)
  (struct+updaters-out fyeld)
  (struct+updaters-out integer32)
  (struct+updaters-out type)
  stringl)

(require
  racket/class
  struct-update
  racket/generic
  (except-in racket/serialize serialize deserialize serializable?)
  threading
  (only-in RacketowerDB/util define-serializable entity-structs)
  (submod RacketowerDB/util interfaces)
  (submod RacketowerDB/util hashable))

(define-serializable type [name byte-size] #:transparent
  #:methods gen:byteable
  [(define (from-bytes self byte-stream)
     (let [(received-bytes-size (bytes-length byte-stream))]
       (if (equal? received-bytes-size (type-byte-size self))
         (case (list 'quote (string->symbol (type-name self)))
           [('INTEGER) (integer32 (integer-bytes->integer byte-stream #t))]
           [('VARCHAR) (stringl (bytes->string/utf-8 byte-stream))]
           [else (raise 'error-with-unknown-type-from-bytes)])
         (raise 'error-with-from-bytes-size-check))))
   (define (to-byte-size self)
     (type-byte-size self))]
  #:methods gen:serializable
  [(define (serialize self #:size [_size #f])
     (let* [(name-bytes (string->bytes/utf-8 (symbol->string (type-name self))))
            (name-length (integer->integer-bytes (bytes-length name-bytes) 1 #t))
            (byte-size-bytes (integer->integer-bytes (type-byte-size self) 4 #t))]
       (bytes-append name-length name-bytes byte-size-bytes)))
   (define (deserialize _self byte-stream)
     (let* [(name-length (integer-bytes->integer (make-bytes 1 (bytes-ref byte-stream 0)) #t))
            (name-value (bytes->string/utf-8 (subbytes byte-stream 1 (+ 1 name-length))))
            (byte-size-value (integer-bytes->integer (subbytes byte-stream (+ 1 name-length) (+ 3 name-length)) #t))]
      (values (type name-value byte-size-value) (+ 5 name-length))))])

(define-serializable stringl [value] #:transparent
  #:methods gen:serializable
  [(define (serialize self #:size [size #f])
     (unless size
       (error "size is required"))
     (let* [(value (stringl-value self))
            (size-of-string (string-length value))]
       (if (< size size-of-string)
         (string->bytes/utf-8 (substring value 0 size)) ;; Truncate
         (let [(dest-bytes (make-bytes size 0))
               (serialyzed-string (string->bytes/utf-8 value))]
           (bytes-copy! dest-bytes 0 serialyzed-string)
           dest-bytes))))
   (define (deserialize _self byte-stream)
     (values (stringl (string-trim (bytes->string/utf-8 byte-stream)) (bytes-length byte-stream))))])

(define-serializable integer32 [value] #:transparent
  #:methods gen:serializable
  [(define (serialize self #:size [_size #f])
     (integer->integer-bytes (integer32-value self) 4 #t))
   (define (deserialize _self byte-stream)
     (values (integer32 (integer-bytes->integer (subbytes byte-stream 0 4) #t)) 4))])

(define-serializable fyeld [position type] #:transparent
  #:methods gen:serializable
  [(define/generic super-serialize serialize)
   (define (serialize self #:size [_size #f])
     (let* [(position (fyeld-position self))
            (position-bytes (integer->integer-bytes position 1 #t))
            (type (fyeld-type self))
            (type-bytes (super-serialize type #:size (type-byte-size type)))]
       (bytes-append position-bytes type-bytes)))
   (define/generic super-deserialize deserialize)
   (define (deserialize self byte-stream)
     (let* [(position-value (integer-bytes->integer (make-bytes 1 (bytes-ref byte-stream 0)) #t))
            (type-bytes (subbytes byte-stream 1))]
       (define-values [new-type type-consumed] (super-deserialize struct:type type-bytes))
       (values (fyeld position-value new-type) (+ 1 type-consumed))))])

(define (fields-size fields)
  (let* [(fields-values (hash-values fields))]
    (foldl (lambda [elem acc]
             (let [(size (type-byte-size (fyeld-type elem)))]
               (+ acc size)))
      0 fields-values)))

(define (check-local-constraints table rows)
  (let [(constraints (table-local-constraints table))]
    (andmap (lambda (constraint) ((eval-syntax constraint) rows)) constraints)))

(define-serializable table
  [identifier row-id fields local-constraints] #:transparent
  #:methods gen:identifiable
  [(define (give-identifier self)
     (table-identifier self))]
  #:methods gen:serializable
  [(define (serialize self #:size [_size #f])
     (let* [(row-id (table-row-id self))
            (row-id-bytes (integer->integer-bytes row-id 4 #t))
            (fields-list (hash->list (table-fields self)))]
       (bytes-append row-id-bytes (serialize-hash-list fields-list #:entity? #f))))
   (define (deserialize self byte-stream)
     (let* [(row-id-value (integer-bytes->integer (subbytes byte-stream 0 4) #t))
            (fields-value (make-hash (deserialize-hash-list struct:fyeld (subbytes byte-stream 4) '())))]
       (values (table "table" row-id-value fields-value) (bytes-length byte-stream))))])

(define-serializable procedure [identifier] #:transparent
  #:methods gen:identifiable
  [(define (give-identifier self)
     (procedure-identifier self))]
  #:methods gen:serializable
  [(define (serialize _self #:size [_size #f])
     (bytes-append (string->bytes/utf-8 "procedures' serialization is not yet implemented")))
   (define (deserialize _self byte-stream)
     (println "procedures' deserialization is not yet implemented")
     (values (procedure "procedure") (bytes-length byte-stream)))])
