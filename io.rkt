#lang racket

(require racket/base)
(require threading)
(require racket/serialize)
(require RacketowerDB/ast)

(module+ writer
  (require RacketowerDB/util)
  (provide write-rows-to-disk)
  (provide write-table-to-disk)
  
  (define (convert-literal table attribute-name literal)
    (let* ((attribute (hash-ref (get-field fields table)
                                attribute-name))
           (type (get-field type attribute))
           (position (get-field position attribute)))
      (cons position (send literal serialize type))))
  
  (define (convert-row table row)
    (~>
     (foldl (lambda (elem acc)
              (cons (convert-literal table (car elem) (cdr elem))
                    acc)) (list) row)
     (sort _ (lambda (a b) (< (car a) (car b))))
     (map cdr _)
     (bytes-join _ #"")))

  (define (update-row-id-table schema table-name id)
    (let ((entity (hash-ref schema table-name)))
      (cond
        [(is-a? entity table%)
         (begin
           (set-field! row-id entity id)
           (hash-set! schema table-name entity)
           schema)]
        [(is-a? entity procedure%)
         (raise 'tried-update-row-id-with-procedure)])))

  (define (write-table-to-disk table table-name)
    (let* ((serialized-table (send table serialize))
           (file-name (build-ndf-filename table-name))
           (out (open-output-file file-name #:exists 'can-update)))
      (write-bytes serialized-table out)
      (close-output-port out)))
  
  (define (write-rows-to-disk schema table-name rows)
    (if (empty? rows)
        schema
        (let* ((first-row (first rows))
               (new-schema (write-row-to-disk schema table-name first-row)))
          (write-rows-to-disk new-schema table-name (rest rows)))))

  (define (write-row-to-disk schema table-name row)
    (let ((entity (hash-ref schema table-name)))
      (cond
        [(is-a? entity table%)
         (let* ((converted-row (convert-row entity row))
                (row-id (get-field row-id entity))
                (total-size (send entity fields-size))
                (off-set (* row-id total-size))
                (file-name (build-ndf-filename table-name #:data? 'data))
                (out (open-output-file file-name #:exists 'can-update)))
           (file-position out off-set)
           (write-bytes converted-row out)
           (close-output-port out)
           (set! schema (update-row-id-table schema table-name (+ row-id 1))))]
        [(is-a? entity procedure%)
         (println "Don't write procedures yet")])
      schema)))

(module+ reader
  (require RacketowerDB/util)
  (require RacketowerDB/ast)
  (provide read-table-from-disk)

  (define (read-table-from-disk schema table-name)
    (let* ((file-name (build-ndf-filename table-name))
           (in (open-input-file file-name #:mode 'binary))
           (table (new table%)))
      (send table deserialize (port->bytes in))
      table))

  (define (deserialize-table-values schema table-name byte-stream)
    (let ((entity (hash-ref schema table-name)))
      (cond
        [(is-a? entity table%)
         (define (create-pair key-field) (cons (car key-field) (get-field type (cdr key-field))))
         (define (sort-by-position key-field1 key-field2)
           (let ((p1 (get-field position (cdr key-field1)))
                 (p2 (get-field position (cdr key-field2))))
             (< p1 p2)))
         (define (reconstruct-columns accumulator columns sub-byte-stream)
           (let* ((first-elem (first columns))
                  (name (car first-elem))
                  (type (cdr first-elem))
                  (size (get-field byte-size type))
                  (new-literal (send type from-bytes (subbytes sub-byte-stream 0 size)))
                  (return (append (list (cons name new-literal)) accumulator))
                  (rest-columns (rest columns)))
             (if (empty? rest-columns)
                 return
                 (reconstruct-columns return rest-columns (subbytes sub-byte-stream size (bytes-length sub-byte-stream))))))
         (~>
          (hash->list (get-field fields entity))
          (sort _ sort-by-position)
          (map create-pair _)
          (reconstruct-columns (list) _ byte-stream)
          (make-hash _))]
        [(is-a? entity procedure%) (raise 'tried-deserialize-procedure-in-table-function)]))))
