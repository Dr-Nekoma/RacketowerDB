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
      (cons position (serialize literal #:size type-size))))

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
    (let [(entity (hash-ref schema table-name))]
      (cond
        [(table? entity)
         (let* [(converted-row (convert-row entity row))
                (row-id (table-row-id entity))
                (total-size (fields-size (table-fields entity)))
                (off-set (* row-id total-size))
                (file-name (build-ndf-filename table-name #:data? 'data))]
           (call-with-output-file file-name #:exists 'can-update
             (lambda [out]
               (file-position out off-set)
               (write-bytes converted-row out)))
           (set! schema (update-row-id-table schema table-name (+ row-id 1))))]
        [(procedura? entity)
         (println "Don't write procedures yet")])
      schema))

  (define (write-rows-to-disk schema table-name rows)
    (if (empty? rows)
      schema
      (let* [(first-row (first rows))
             (new-schema (write-row-to-disk schema table-name first-row))]
        (write-rows-to-disk new-schema table-name (rest rows)))))

  (define (update-row-id-table schema table-name id)
    (let [(entity (hash-ref schema table-name))]
      (cond
        [(table? entity)
         (hash-set! schema table-name (table-row-id-set entity id))
         schema]
        [(procedura? entity)
         (raise 'tried-update-row-id-with-procedure)])))

  (define (write-table-to-disk table table-name)
    (let* [(serialized-table (serialize table))
           (file-name (build-ndf-filename table-name))]
      (call-with-output-file file-name #:exists 'truncate
        (curry write-bytes serialized-table))
      (void)))

  (define (write-schema-to-disk schema)
    (define (write-entity-to-disk file-out entities-list)
      (let* [(entity-name (give-identifier (cdr (car entities-list))))]
        (write-string entity-name file-out)
        (newline file-out)
        (write-bytes (serialize-hash-list entities-list #:entity? #t) file-out)
        (newline file-out)))
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
    racket/hash)

  (define (read-schema-from-disk schema-name)
    (define (build-hash-from-line struct-instance line-in-bytes)
      (make-immutable-hash
        (deserialize-hash-list
          struct-instance
          line-in-bytes
          '())))
    (define (proceed-reading struct-name current-schema line-in-bytes)
      (let [(line (bytes->string/utf-8 line-in-bytes))]
        (if (hash-has-key? entity-structs line)
          (cons line current-schema)
          (cons struct-name (hash-union current-schema (build-hash-from-line (hash-ref entity-structs struct-name) line-in-bytes))))))
    (let* [(file-name (build-ndf-filename schema-name  #:data? 'schema))
           (in (open-input-file file-name #:mode 'binary))
           (schema (make-immutable-hash (list)))
           (real-lines (port->bytes-lines in #:close? #t))
           (read-lines (fix-empty-read-bytes-lines real-lines))]
      (~> (foldl
            (lambda [line-in-bytes acc]
              (let [(builder-struct (car acc))
                    (current-schema (cdr acc))]
                (proceed-reading builder-struct current-schema line-in-bytes)))
            (cons null schema) read-lines)
        cdr
        hash->list
        make-hash)))

  (define (read-table-from-disk table-name)
    (let* [(file-name (build-ndf-filename #:data? 'entity table-name))
           (in (open-input-file file-name #:mode 'binary))]
      (define-values (table table-consumed) (deserialize struct:table (port->bytes in #:close? #t)))
      table))

  (define (read-table-values-from-disk schema table-name)
    (let* [(file-name (build-ndf-filename #:data? 'data table-name))
           (in (open-input-file file-name #:mode 'binary))
           (byte-stream (port->bytes in #:close? #t))
           (entity (hash-ref schema table-name))]
      (cond
        [(table? entity)
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
           (reconstruct-all-literals (list) _ byte-stream))]
        [(procedura? entity) (raise 'tried-deserialize-procedure-in-table-function)]))))
