#lang racket

(require racket/base)
(require threading)
(require br/cond)

(provide build-ndf-filename)
(provide entity-classes)
(provide define-serializable)
(provide fix-empty-read-bytes-lines)


(require syntax/parse/define
         (for-syntax racket/syntax))

;; (define-syntax-parse-rule (create name) 
;;   #:with name-string (datum->syntax #f #'name)
;;   (define name-string 2))

(define (fix-empty-read-bytes-lines lines)
  (define (fix-one-turn inner-lines)
    (let ((newline-flag #f))
      (foldl (lambda (line new-lines)
               (if newline-flag
                   (begin
                     (begin
                       (set! newline-flag #f)
                       new-lines)
                     (append new-lines (list (bytes-append #"\n" line))))
                   (if (bytes=? #"" line)
                       (begin
                         (set! newline-flag #t)
                         new-lines)
                       (append new-lines (list line))))) (list) inner-lines)))
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

(define entity-classes (make-hash (list)))

(define-syntax-parse-rule (define-serializable name body ...)
    (begin
        (define name body ...)
        (hash-set! entity-classes (symbol->string 'name) name)))

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

