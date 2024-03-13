#lang racket

(require
  threading
  RacketowerDB/util
  (rename-in RacketowerDB/ast (procedure? procedura?))
  (submod RacketowerDB/util interfaces)
  (submod RacketowerDB/util hashable))

(module+ writer
  (provide
    write-rows-to-disk
    write-table-to-disk
    write-schema-to-disk)

  (define (convert-literal table attribute-name literal)
    (let* [(attribute (hash-ref (table-fields table)
                                attribute-name))
           (type (field-type attribute))
           (type-size (type-byte-size type))
           (position (field-position attribute))]
      (define-values (_literal-size serialized-literal) (serialize literal #:size type-size))
      (cons position serialized-literal)))

  (define (convert-row table row)
    (~>
     (foldl (lambda [elem acc]
              (cons (convert-literal table (car elem) (cdr elem))
                    acc))
       (list) row)
     (sort _ (lambda [a b] (< (car a) (car b))))
     (map cdr _)
     (bytes-join _ #"")))

  (define (write-row-to-disk schema table-name row)
    (let* [(table (lookup-table-in-schema schema table-name))
           (converted-row (convert-row table row))
           (row-id (table-row-id table))
           (total-size (fields-size (table-fields table)))
           (off-set (* row-id total-size))
           (file-name (build-ndf-filename table-name #:data? 'data))]
      (call-with-output-file file-name #:exists 'can-update
        (lambda [out]
          (file-position out off-set)
          (write-bytes converted-row out)))
      (update-row-id-table schema table-name (+ row-id 1))))

  (define (write-rows-to-disk schema table-name rows)
    (if (empty? rows)
      schema
      (let* [(first-row (first rows))
             (new-schema (write-row-to-disk schema table-name first-row))]
        (write-rows-to-disk new-schema table-name (rest rows)))))

  (define (update-row-id-table schema table-name id)
    (hash-set!
     schema
     table-name
     (table-row-id-set
      (lookup-table-in-schema schema table-name)
      id))
    schema)

  (define (write-table-to-disk table table-name)
    (define-values (_table-size serialized-table) (serialize table))
    (define filename (build-ndf-filename table-name))
    (call-with-output-file filename #:exists 'truncate
      (curry write-bytes serialized-table))
    (void))

  (define (write-schema-to-disk schema)
    (define (write-entity-to-disk file-out entities-list)
      (let* [(entity-name (give-identifier (cdr (car entities-list))))
             (entity-name-length (string-length entity-name))
             (list-length (length entities-list))]
        (write-bytes (integer->integer-bytes entity-name-length 1 #t) file-out)
        (write-string entity-name file-out)
        (write-bytes (integer->integer-bytes list-length 2 #t) file-out)
        (write-bytes (serialize-hash-list entities-list) file-out)))
    (let* [(schema-list (hash->list schema))
           (file-name (build-ndf-filename "schema" #:data? 'schema))]
      (call-with-output-file file-name #:exists 'truncate
        (lambda [out]
          (~>> (group-by (compose give-identifier cdr) schema-list)
               (map (curry write-entity-to-disk out)))
          (void))))))

(module+ reader
  (provide
    read-schema-from-disk
    read-table-from-disk
    read-table-values-from-disk)

  (require
   RacketowerDB/ast
   RacketowerDB/util
    racket/hash)
  
  (define (read-schema-from-disk schema-name)
    (define (read-entities-from-disk how-many-entities entity-name byte-stream)
      (define-values (consumed-bytes entities) (deserialize-hash-list
                                                (hash-ref entity-structs entity-name)
                                                how-many-entities
                                                byte-stream))
      (values (make-immutable-hash entities) (subbytes byte-stream consumed-bytes)))
    (define (read-block-from-disk byte-stream)
      (let* [(entity-name-size (integer-bytes->integer (subbytes byte-stream 0 1) #t))
             (entity-name (bytes->string/utf-8 (subbytes byte-stream 1 (+ 1 entity-name-size))))
             (how-many-entities (integer-bytes->integer (subbytes byte-stream (+ 1 entity-name-size) (+ 3 entity-name-size)) #t))]
      (read-entities-from-disk how-many-entities entity-name (subbytes byte-stream (+ 3 entity-name-size)))))
    (let* [(file-name (build-ndf-filename schema-name  #:data? 'schema))
           (in (open-input-file file-name #:mode 'binary))
           (content (port->bytes in #:close? #t))]
      (~> (let loop [(schema (make-immutable-hash (list)))
                     (byte-array content)]
            (if (bytes-empty? byte-array)
                schema
                (let [] 
                  (define-values (sub-schema remaining-byte-stream) (read-block-from-disk byte-array))
                  (loop (hash-union schema sub-schema) remaining-byte-stream))))
        hash->list
        make-hash)))

  (define (read-table-from-disk table-name)
    (let* [(file-name (build-ndf-filename #:data? 'entity table-name))
           (in (open-input-file file-name #:mode 'binary))]
      (deserialize struct:table (port->bytes in #:close? #t))))

  ;; TODO: This function should account for eventual errors LOL
  (define (read-table-values-from-disk schema table-name #:source [source #""])
    (define file-name (build-ndf-filename #:data? 'data table-name))
    (define byte-stream
      (if (bytes-empty? source)
          (port->bytes (open-input-file file-name #:mode 'binary) #:close? #t)
          source))
    ;; TODO: This validation should be done in the caller
    (define entity (lookup-table-in-schema schema table-name))
    (define (create-pair key-field) (cons (car key-field) (field-type (cdr key-field))))
    (define (sort-by-position key-field1 key-field2)
      (let [(p1 (field-position (cdr key-field1)))
            (p2 (field-position (cdr key-field2)))]
        (< p1 p2)))
    (define (reconstruct-literal-data accumulator fields sub-byte-stream)
      (let* [(first-elem (first fields))
             (name (car first-elem))
             (type (cdr first-elem))
             (size (type-byte-size type))
             (new-literal (from-bytes type (subbytes sub-byte-stream 0 size)))
             (return (append (list (cons name new-literal)) accumulator))
             (rest-fields (rest fields))
             (remaining-bytes (subbytes sub-byte-stream size (bytes-length sub-byte-stream)))]
        (if (empty? rest-fields)
            (cons return remaining-bytes)
            (reconstruct-literal-data return rest-fields remaining-bytes))))
    (define (reconstruct-all-literals accumulator fields inner-byte-stream)
      (let* [(one-line (reconstruct-literal-data (list) fields inner-byte-stream))
             (computed-line (list (car one-line)))
             (remaining-bytes (cdr one-line))
             (return (append accumulator computed-line))]
        (if (bytes=? #"" remaining-bytes)
            return
            (reconstruct-all-literals return fields remaining-bytes))))
    (~>
     (hash->list (table-fields entity))
     (sort _ sort-by-position)
     (map create-pair _)
     (reconstruct-all-literals (list) _ byte-stream)
     (map make-hash _))))
