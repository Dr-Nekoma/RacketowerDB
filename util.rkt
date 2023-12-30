#lang racket

(provide
  build-ndf-filename
  checked-guard
  entity-structs
  define-serializable
  bytes-empty?)

(require
  (for-syntax threading racket/syntax racket/list)
  struct-update
  threading
  br/cond)

(define (build-ndf-filename #:data? [data? 'entity] name)
  (let [(path (case (list 'quote data?)
                [('entity) "ndf/entities/"]
                [('schema) "ndf/schemas/"]
                [('data) "ndf/data/"]
                [else (raise 'error-not-specified-datatype)]))]
    (string-append path (string-append name ".ndf"))))

(define entity-structs (make-hash (list)))

(define-syntax (define-serializable stx)
  (syntax-case stx ()
    [(define-serializable name body ...)
     #`(begin
         (struct name body ...)
         (define-struct-updaters name)
         (hash-set! entity-structs (symbol->string 'name)
           #,(datum->syntax #'name
               (let [(datum-name (syntax->datum #'name))]
                 (string->symbol (string-append "struct:" (symbol->string datum-name)))))))]))

(define-syntax (checked-guard stx)
  (syntax-case stx []
    [(_ [(args . preds) ...] body ...)
     (let []
       (define/with-syntax [n ...]
         (datum->syntax #'[args ...]
           (~> #'[args ...]
             syntax->list
             length
             range)))
       #`(lambda [args ... name]
           (unless (preds args)
             (raise-argument-error name
               (with-output-to-string
                 (lambda []
                   (write 'preds)))
               n
               args ...)) ...
           body ...))]))

(define (bytes-empty? byte-stream)
  (equal? #"" byte-stream))

(module interfaces racket
  (provide
    serializable?
    byteable?
    identifiable?
    gen:serializable
    gen:byteable
    gen:identifiable
    (contract-out
      [give-identifier (-> identifiable? string?)]
      [serialize (->* (serializable?) (#:size integer?) (values natural? bytes?))]
      [deserialize (-> serializable? bytes? serializable?)]
      [from-bytes (-> byteable? bytes? serializable?)]
      [to-byte-size (-> byteable? natural?)]))

  (require racket/generic racket/contract)

  (define-generics identifiable #:requires [give-identifier]
    (give-identifier identifiable))

  (define-generics serializable #:requires [serialize deserialize]
    (serialize serializable #:size (size))
    (deserialize serializable byte-stream))

  (define-generics byteable #:requires [from-bytes to-byte-size]
    (from-bytes byteable byte-stream)
    (to-byte-size byteable)))

(module+ hashable
  (provide
    (contract-out
      [deserialize-hash-list (-> serializable? natural? bytes? (values natural? list?))]
      [serialize-hash-list (-> (listof (cons/c string? serializable?)) bytes?)]))

  (require (submod ".." interfaces))

  (define (deserialize-hash-list entity how-many byte-stream)
    (inner-deserialize-hash-list entity how-many byte-stream 0 '()))
  
  (define (inner-deserialize-hash-list entity how-many byte-stream consumed-bytes accumulator)
    (define (deserialize-name more-bytes)
      (let* [(name-size (integer-bytes->integer (subbytes more-bytes 0 4) #t))
             (name (bytes->string/utf-8 (subbytes more-bytes 4 (+ 4 name-size))))]
        (values (+ 4 name-size) name)))
    (if (zero? how-many)
        (values consumed-bytes accumulator)
      (let []
        (define-values [name-consumed name] (deserialize-name byte-stream))
        (define entity-size (integer-bytes->integer (subbytes byte-stream name-consumed (+ name-consumed 4)) #t))
        (define thing (deserialize entity (subbytes byte-stream (+ 4 name-consumed) (+ name-consumed entity-size 4))))
        (inner-deserialize-hash-list
         entity
         (- how-many 1)
         (subbytes byte-stream (+ 4 name-consumed entity-size))
         (+ consumed-bytes name-consumed entity-size 4)
         (append accumulator (list (cons name thing)))))))

  (define (serialize-hash-list named-values-list)
    (define (serialize-name name)
      (let* [(name-bytes (string->bytes/utf-8 name))
             (name-size (bytes-length name-bytes))
             (serialized-name-size (integer->integer-bytes name-size 4 #t))]
        (bytes-append serialized-name-size name-bytes)))
    (~>
      (map (lambda [named-value]
             (let* [(name (car named-value))
                    (value (cdr named-value))]
               (define-values [value-size serialized-value] (serialize value))
               (bytes-append
                (serialize-name name)
                (integer->integer-bytes value-size 4 #t)
                serialized-value)))
        named-values-list)
      (bytes-join _ #""))))

