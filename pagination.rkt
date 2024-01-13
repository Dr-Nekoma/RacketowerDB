#lang racket

(require
 ffi/unsafe
 (only-in RacketowerDB/util checked-guard build-ndf-filename)
 (submod RacketowerDB/io reader)
 RacketowerDB/ast)

(provide build-pages)

(struct page [instances indexes] #:transparent
  #:guard
  (checked-guard
   [(instances . (listof (listof byte?)))
    (indexes . (listof (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)))]
   (values instances indexes)))

(struct pager [pages tree amount-read instancesPerPage maxPagesAtOnce] #:transparent
  #:guard
  (checked-guard
   [(pages . (listof page?))
    (tree .  cpointer?)
    (amount-read . exact-nonnegative-integer?)
    (instancesPerPage . exact-nonnegative-integer?)
    (maxPagesAtOnce . exact-nonnegative-integer?)]
   (values pages tree amount-read)))

(define (split-by lst n)
   (if (not (empty? lst))
       (cons (take lst n) (split-by (drop lst n) n))
       '()))

(define (build-pages
         initial-page-number
         instances-per-page
         page-bound
         instance-size
         amount-already-read
         attribute-to-index
         schema
         entity-name)
  ;; TODO: Leave an error message to only work on the supported types.
  (define (query data attribute-to-index)
    (first (map (lambda [row] (integer32-value (hash-ref row attribute-to-index))) data)))
  (define (create-indexes instances schema entity-name attribute-to-index)
    (map (lambda (index instance)
           (let* [(instance-bytes (list->bytes instance))
                  (read-content (read-table-values-from-disk schema entity-name #:source instance-bytes))
                  (attempt-to-find (query read-content attribute-to-index))]
             ;; TODO: We are missing some error checking in here
             (cons attempt-to-find index)))
         (range (length instances))
         instances))
  (define file-name (build-ndf-filename entity-name #:data? 'data))
  (define content-length (file-size file-name))
  (let loop [(amount-already-read amount-already-read)
             (reader (open-input-file file-name #:mode 'binary))
             (index initial-page-number)
             (pages (list))]
    (if (equal? index page-bound)
        (values pages amount-already-read)
        (begin
          (file-position reader (* index instance-size))
          (let* [(amount-to-read (if (> (* instances-per-page instance-size) (- content-length amount-already-read))
                                     (- content-length amount-already-read)
                                     (* instances-per-page instance-size)))
                 (buffer (bytes->list (read-bytes amount-to-read reader)))
                 (instances (take (split-by buffer instance-size) page-bound))
                 (indexes (create-indexes instances schema entity-name attribute-to-index))]
            (loop
             (+ amount-already-read amount-to-read)
             reader
             (+ instances-per-page index)
             (append pages (list (page instances indexes)))))))))

