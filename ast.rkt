#lang racket

(provide
  check-local-constraints
  (struct+updaters-out table)
  fields-size
  (struct+updaters-out procedure)
  (struct+updaters-out field)
  (struct+updaters-out integer32)
  (struct+updaters-out type)
  (struct+updaters-out stringl))

(require
  struct-update
  racket/generic
  threading
  (only-in RacketowerDB/util define-serializable entity-structs checked-guard)
  (submod RacketowerDB/util interfaces)
  (submod RacketowerDB/util hashable))

(define-serializable type [name byte-size] #:transparent
  #:guard
  (checked-guard
    [(name . symbol?)
     (byte-size . exact-nonnegative-integer?)]
    (values name byte-size))
  #:methods gen:byteable
  [(define (from-bytes self byte-stream)
     (let [(received-bytes-size (bytes-length byte-stream))]
       (if (equal? received-bytes-size (type-byte-size self))
         (case (type-name self)
           [[INTEGER] (integer32 (integer-bytes->integer byte-stream #t))]
           [[VARCHAR] (stringl (bytes->string/utf-8 byte-stream))]
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
            (name-value (~> name-length
                          add1
                          (subbytes byte-stream 1 _)
                          bytes->string/utf-8
                          string->symbol))
            (byte-size-value (integer-bytes->integer (subbytes byte-stream (+ 1 name-length) (+ 3 name-length)) #t))]
      (values (type name-value byte-size-value) (+ 5 name-length))))])

(define-serializable stringl [value] #:transparent
  #:guard
  (checked-guard
    [(value . string?)]
    value)
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
  #:guard
  (checked-guard
    [(value . exact-nonnegative-integer?)]
    value)
  #:methods gen:serializable
  [(define (serialize self #:size [_size #f])
     (integer->integer-bytes (integer32-value self) 4 #t))
   (define (deserialize _self byte-stream)
     (values (integer32 (integer-bytes->integer (subbytes byte-stream 0 4) #t)) 4))])

(define-serializable field [position type] #:transparent
  #:guard
  (checked-guard
    [(position . exact-nonnegative-integer?)
     (type . type?)]
    (values position type))
  #:methods gen:serializable
  [(define/generic super-serialize serialize)
   (define (serialize self #:size [_size #f])
     (let* [(position (field-position self))
            (position-bytes (integer->integer-bytes position 1 #t))
            (type (field-type self))
            (type-bytes (super-serialize type #:size (type-byte-size type)))]
       (bytes-append position-bytes type-bytes)))
   (define/generic super-deserialize deserialize)
   (define (deserialize self byte-stream)
     (let* [(position-value (integer-bytes->integer (make-bytes 1 (bytes-ref byte-stream 0)) #t))
            (type-bytes (subbytes byte-stream 1))]
       (define-values [new-type type-consumed] (super-deserialize struct:type type-bytes))
       (values (field position-value new-type) (+ 1 type-consumed))))])

(define (fields-size fields)
  (let* [(fields-values (hash-values fields))]
    (foldl (lambda [elem acc]
             (let [(size (type-byte-size (field-type elem)))]
               (+ acc size)))
      0 fields-values)))

(define (check-local-constraints table rows)
  (let [(constraints (table-local-constraints table))]
    (andmap (lambda [constraint]
              ((eval-syntax constraint) rows))
      constraints)))

(define-serializable table
  [identifier row-id fields local-constraints] #:transparent
  #:guard
  (checked-guard
    [(identifier . (and/c string? immutable?))
     (row-id . exact-nonnegative-integer?)
     (fields . hash?)
     (local-constraints . (listof syntax?))]
    (values identifier row-id fields local-constraints))
  #:methods gen:identifiable
  [(define (give-identifier self)
     (table-identifier self))]
  #:methods gen:serializable
  [(define (serialize self #:size [_size #f])
     (define (serialize-constraints constraint-list)
       (define (serialize-constraint constraint)
         (let* [(serialized-constraint (string->bytes/utf-8 (call-with-output-string
                                                             (lambda (s-port)
                                                               (write (syntax->datum constraint) s-port)))))
                (constraint-size (call-with-output-bytes
                                  (lambda (c-port)
                                    (write-char (integer->char (bytes-length serialized-constraint)) c-port))))]
           (bytes-append constraint-size serialized-constraint)))
       (define constraints-count (length constraint-list))
       (unless (<= constraints-count #xffff)
         (raise 'too-many-constraints))
       (let [(serialized-count (integer->integer-bytes constraints-count 1 #f))
             (serialized-constraints (bytes-join (map serialize-constraint constraint-list) #""))]
         (bytes-append serialized-count serialized-constraints)))
     (let* [(row-id (table-row-id self))
            (row-id-bytes (integer->integer-bytes row-id 4 #t))
            (fields-list (hash->list (table-fields self)))            
            (constraints (serialize-constraints (table-local-constraints self)))]
       (bytes-append row-id-bytes constraints (serialize-hash-list fields-list #:entity? #f))))
   (define (deserialize self byte-stream)
     (define (utf8-character-as-integer byte-array)
       (call-with-input-bytes
        byte-array
        (lambda (c-port)
          (let [(char-read (read-char c-port))]
            (values (char-utf-8-length char-read) (char->integer char-read))))))
     (define (deserialize-constraint byte-array)
       (define-values (constraint-size-consumed constraint-size) (utf8-character-as-integer byte-array))
       (let* [(constraint (datum->syntax
                           #'a
                           (read (open-input-bytes
                                  (subbytes byte-array constraint-size-consumed
                                            (+ constraint-size constraint-size-consumed))))))]
         (values (+ constraint-size constraint-size-consumed) constraint)))
     (define (deserialize-constraints byte-array)
       (let loop [(acc (list))
                  (n (integer-bytes->integer (subbytes byte-array 0 1) 1 #f))
                  (consumed-bytes 1)
                  (streamb (subbytes byte-array 1))]
         (if (zero? n)
             (values consumed-bytes acc)
             (let [] 
               (define-values (constraint-size constraint-value) (deserialize-constraint streamb))
               (loop (cons constraint-value acc) (- n 1) (+ consumed-bytes constraint-size) (subbytes streamb constraint-size))))))
     (define row-id-value (integer-bytes->integer (subbytes byte-stream 0 4) #t))
     (define-values (constraints-length constraints-value) (deserialize-constraints (subbytes byte-stream 4)))
     (println constraints-length)
     (println constraints-value)
     (println (subbytes byte-stream (+ 4 constraints-length)))
     (define fields-value (make-hash (deserialize-hash-list struct:field (subbytes byte-stream (+ 4 constraints-length)) '())))
     (values
      (table "table" row-id-value fields-value constraints-value)
      (bytes-length byte-stream)))])

(define-serializable procedure [identifier] #:transparent
  #:guard
  (checked-guard
    [(identifier . (and/c string? immutable?))]
    identifier)
  #:methods gen:identifiable
  [(define (give-identifier self)
     (procedure-identifier self))]
  #:methods gen:serializable
  [(define (serialize _self #:size [_size #f])
     (bytes-append (string->bytes/utf-8 "procedures' serialization is not yet implemented")))
   (define (deserialize _self byte-stream)
     (println "procedures' deserialization is not yet implemented")
     (values (procedure "procedure") (bytes-length byte-stream)))])
