#lang racket

(require struct-update)
(require threading)
(require br/cond)

(provide build-ndf-filename)
(provide entity-structs)
(provide define-serializable)
(provide fix-empty-read-bytes-lines)

(require syntax/parse/define
         (for-syntax racket/syntax))

(define (fix-empty-read-bytes-lines lines)
  (define (fix-one-turn inner-lines)
    (let ((newline-flag #f))
      (foldl (lambda (line new-lines)
               [if newline-flag
                   (begin
                     (begin
                       (set! newline-flag #f)
                       new-lines)
                     (append new-lines (list (bytes-append #"\n" line))))
                   (if (bytes=? #"" line)
                       (begin
                         (set! newline-flag #t)
                         new-lines)
                       (append new-lines (list line)))]) (list) inner-lines)))
  (define (stop-condition lines-to-check) (empty? (filter (lambda (line) (bytes=? #"" line)) lines-to-check)))
  (while (not (stop-condition lines))
    (set! lines (fix-one-turn lines)))
  lines)

(define build-ndf-filename
  (lambda (#:data? [data? 'entity] name)
    (let ((path (case (list 'quote data?)
		  [('entity) "ndf/entities/"]
		  [('schema) "ndf/schemas/"]
		  [('data) "ndf/data/"]
		  [else (raise 'error-not-specified-datatype)])))
      (string-append path (string-append name ".ndf")))))

(define entity-structs (make-hash (list)))

(define-syntax (define-serializable stx)
  (syntax-case stx ()
    [(define-serializable name body ...)
     #`(begin
         (struct name body ...)
         (define-struct-updaters name)
         (hash-set!
          entity-structs
          (symbol->string 'name)
          #,(datum->syntax #'name (let ((datum-name (syntax->datum #'name)))
                                  (string->symbol (string-append "struct:" (symbol->string datum-name)))))))]))

(module interfaces racket
  (provide
   (contract-out
    [give-identifier (-> identifiable? string?)]
    [serialize (->* (serializable?) (#:size integer?) bytes?)]
    [deserialize (-> serializable? bytes? (values serializable? natural?))]
    [from-bytes (-> byteable? bytes? serializable?)]
    [to-byte-size (-> byteable? natural?)])
   serializable?
   byteable?
   identifiable?
   gen:serializable
   gen:byteable
   gen:identifiable)
  
  (require racket/generic)
  (require racket/contract)

  (define-generics identifiable #:requires [give-identifier]
    (give-identifier identifiable))
  
  (define-generics serializable #:requires [serialize deserialize]
    (serialize serializable #:size (size))
    (deserialize serializable byte-stream))

  (define-generics byteable #:requires [from-bytes to-byte-size]
    (from-bytes byteable byte-stream)
    (to-byte-size byteable)))

(module+ hashable
  (require (submod ".." interfaces))

  (provide
    (contract-out
      [deserialize-hash-list (-> serializable? bytes? list? list?)]
      [serialize-hash-list (-> (listof (cons/c string? serializable?)) boolean? bytes?)]))

  (define (deserialize-hash-list entity byte-stream accumulator)
    (define (deserialize-name more-bytes)
      (let* ((name-size (integer-bytes->integer (subbytes more-bytes 0 4) #t))
             (name (bytes->string/utf-8 (subbytes more-bytes 4 (+ 4 name-size)))))
        (cons name (+ 4 name-size))))
    (if (equal? byte-stream #"")
        accumulator
        (let* ((name-consumption (deserialize-name byte-stream))
               (name-consumed (cdr name-consumption))
               (name (car name-consumption)))
          (define-values (thing thing-consumed) (deserialize entity (subbytes byte-stream name-consumed)))
          (deserialize-hash-list
           entity
           (subbytes byte-stream (+ name-consumed thing-consumed) (bytes-length byte-stream))
           (append accumulator (list (cons name thing)))))))

  (define (serialize-hash-list named-values-list entity?)
        (define (serialize-name name)
          (let* ((name-bytes (string->bytes/utf-8 name))
                 (name-size (integer->integer-bytes (bytes-length name-bytes) 4 #t)))
            (bytes-append name-size name-bytes)))
        (~>
         (map (lambda (named-value)
                (let ((name (car named-value))
                      (value (cdr named-value)))
                (bytes-append
                 (serialize-name name)
                 (serialize value))))
              named-values-list)
         (bytes-join _ (if entity? #"\n" #"")))))

