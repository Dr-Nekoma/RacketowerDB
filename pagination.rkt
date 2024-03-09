#lang racket

(require
 ffi/unsafe
 struct-update
 threading
 (only-in RacketowerDB/util checked-guard build-ndf-filename split-by take-up-to chunk-by-size foldl-reordered) 
 (submod RacketowerDB/io reader)
 RacketowerDB/bplustree
 RacketowerDB/ast)

(provide build-pages
         build-pagination
         search
         (struct+updaters-out query))

(struct row-id [chunk-number page-number slot-number] #:transparent
  #:guard
  (checked-guard
   [(chunk-number . exact-nonnegative-integer?)
    (page-number . exact-nonnegative-integer?) 
    (slot-number . exact-nonnegative-integer?)]
   (values chunk-number page-number slot-number)))

(struct index [key columns row-id] #:transparent
  #:guard
  (checked-guard
   [(key . exact-nonnegative-integer?) ;; TODO: We are only handling integer keys for now
    (columns . hash?)
    (row-id . row-id?)]
   (values key columns row-id)))

(struct page [indexes] #:transparent
  #:guard
  (checked-guard
   [(indexes . (listof index?))] ;; TODO: Couldn't we use a chunk of rows instead of chunks of pages of rows?
   (values indexes)))

(struct tailor [page-bytesize instances-per-page chunk-size instance-size amount-already-read] #:transparent
  #:guard
  (checked-guard
   [(page-bytesize . exact-nonnegative-integer?)
    (instances-per-page . (or/c #f exact-nonnegative-integer?))
    (chunk-size . exact-nonnegative-integer?)
    (instance-size . (or/c #f exact-nonnegative-integer?))
    (amount-already-read . exact-nonnegative-integer?)]
   (define instances-per-page* (or instances-per-page (and instance-size (quotient page-bytesize instance-size))))
   (when (and instances-per-page (not (equal? instances-per-page instances-per-page*)))
     (raise-argument-error 'tailor "Instances per page and Instance Size are not proportional."
                           "Page Bytesize:" page-bytesize
                           "Instances Per Page:" instances-per-page
                           "Instance Size:" instance-size))
   (define instance-size* (or instance-size (and instances-per-page (quotient page-bytesize instances-per-page))))
   (when (and instance-size (not (equal? instance-size instance-size*)))
     (raise-argument-error 'tailor "Instances per page and Instance Size are not proportional."
                           "Page Bytesize:" page-bytesize
                           "Instances Per Page:" instances-per-page
                           "Instance Size:" instance-size))
   (values
     page-bytesize
     instances-per-page*
     chunk-size
     instance-size*
     amount-already-read)))

(define-struct-updaters tailor)

(define default-page-bytesize 42)
(define default-pages-per-chunk 2)
(define default-chunk-size (* default-pages-per-chunk default-page-bytesize))
(define default-tailor (tailor default-page-bytesize false default-chunk-size false 0))

(struct query [entity-name attribute-to-index attribute-value] #:transparent
  #:guard
  (checked-guard
   [(entity-name . string?)
    (attribute-to-index . string?)
    (attribute-value . exact-nonnegative-integer?)] ;; TODO: We are only handling integer keys for now
   (values entity-name attribute-to-index attribute-value)))
(define-struct-updaters query)

(struct pager [chunks tree tailor] #:transparent
  #:guard
  (checked-guard
   [(chunks . (listof (listof page?)))
    (tree .  cpointer?)
    (tailor . tailor?)]
   (values chunks tree tailor)))

(define (build-pages schema query tailor list-buffer current-chunk-number)
  (let [(entity-name (query-entity-name query))
        (attribute-to-index (query-attribute-to-index query))
        (instances-per-page (tailor-instances-per-page tailor))
        (instance-size (tailor-instance-size tailor))]

    (define (attribute-error-message columns)
      (error (format "Could not find column \"~s\" in columns: ~a" attribute-to-index columns)))
    
    ;; Create an index from the deserialized data alongide row-id
    (define (index-builder chunk-number page-number instance-number columns)
      (~> (hash-ref columns attribute-to-index (lambda () (attribute-error-message columns)))
          ((extract-value schema entity-name attribute-to-index))
          (index _ columns (row-id chunk-number page-number instance-number))))
    
    ;; We create a list of pages, each page with a list of instances
    (define raw-pages
      (~> list-buffer
          (chunk-by-size instance-size _)
          (chunk-by-size instances-per-page _)))

    ;; For each list of instances per page
    (for/list [(page-number (in-naturals))
               (instances raw-pages)]
      
      (define indexing
        ;; For each instance within the list of instances
        (for/list [(instance-number (in-naturals))
                   (instance instances)]
          ;; Read it from the disk
          (~> (read-table-values-from-disk schema entity-name #:source (list->bytes instance))
              car
              (index-builder current-chunk-number page-number instance-number _))))
      (page indexing))))

(define (build-chunks schema query tailor)
  (define entity-name (query-entity-name query))
  (let* [(table (lookup-table-in-schema schema entity-name))
         (instances-per-page (tailor-instances-per-page tailor))
         (chunk-size (tailor-chunk-size tailor))
         (instance-size (tailor-instance-size tailor))
         (amount-already-read (tailor-amount-already-read tailor))
         (entity-size (table-row-size table))]

    (define (derive-to-read-bytes content-length amount-already-read)
      (let* [(pages-bytes (* instances-per-page instance-size default-pages-per-chunk))
             (remaining (- content-length amount-already-read))]
        (min pages-bytes remaining)))
    
    (define file-name (build-ndf-filename entity-name #:data? 'data))
    (define content-length (file-size file-name))
    (define reader (open-input-file file-name #:mode 'binary))

    ;; This loops over the file using a file descriptor and amount already read
    (let loop [(amount-already-read amount-already-read)
               (current-chunk-number 0)
               (chunks (list))]
      ;; This checks if we can continue. What if we have less content than a chunk size?
      (if (< amount-already-read content-length)
          (begin
            ;; Moving file descriptor to position
            (file-position reader amount-already-read)

            ;; Calculates how many more bytes we should read
            (let* [(amount-to-read (derive-to-read-bytes content-length amount-already-read))
                   ;; Read new amount
                   (pages-buffer (bytes->list (read-bytes amount-to-read reader)))
                   (pages (build-pages schema query tailor pages-buffer current-chunk-number))]
              ;; Loops updating the amount already read, the offset, chunk number, and append new created pages
              (loop
               (+ amount-already-read amount-to-read)
               (add1 current-chunk-number)
               (cons pages chunks))))
          ;; We are done with this file bud xD
          chunks))))

(define (fold-chunks chunks initial)
  (foldl-reordered chunks initial
     (lambda (pages tree1)
       (foldl-reordered pages tree1
        (lambda (page tree2)
          (foldl-reordered (page-indexes page) tree2
           (lambda (element tree3)
             (let* [(key (index-key element))
                    (row-id (index-row-id element))
                    (chunk-number (row-id-chunk-number row-id))
                    (page-number (row-id-page-number row-id))
                    (slot-number (row-id-slot-number row-id))]
               (insert tree3 key chunk-number page-number slot-number)))))))))

(define (build-pagination schema query tailor)
  (define (create-b-plus-tree chunks) (fold-chunks chunks false))
  (define entity-name (query-entity-name query))
  (let* [(table (lookup-table-in-schema schema entity-name))
         (instance-size (table-row-size table))
         (new-tailor (tailor-instance-size-set tailor instance-size))]
    (define chunks (build-chunks schema query new-tailor))
    (define tree (create-b-plus-tree chunks))
    (pager chunks tree new-tailor)))

(define (derive-offset schema table-name tailor row-id-ptr)
  (define row-size (table-row-size (hash-ref schema table-name)))
  (define chunk-size (tailor-chunk-size tailor))
  (define page-size (tailor-page-bytesize tailor))
  (define row-id-value (apply row-id (deref-row-id row-id-ptr)))
  (define chunk-number (row-id-chunk-number row-id-value))
  (define page-number (row-id-page-number row-id-value))
  (define slot-number (row-id-slot-number row-id-value))
  (values row-size (+ (* chunk-size chunk-number) (* page-size page-number) (* row-size slot-number))))

(define (offset-lookup schema table-name tailor row-id)
  (define-values (row-size offset) (derive-offset schema table-name tailor row-id))
  (define file-name (build-ndf-filename #:data? 'data table-name))
  (define payload (call-with-input-file file-name #:mode 'binary
                    (lambda (input)
                      (file-position input offset)
                      (read-bytes row-size input))))
  (read-table-values-from-disk schema table-name #:source payload))

(define (update-tailor-page-size schema entity-name tailor)
  (let* [(table (lookup-table-in-schema schema entity-name))
         (page-bytesize (tailor-page-bytesize tailor))
         (entity-size (table-row-size table))
         (page-rem (remainder page-bytesize entity-size))
         (new-page-bytesize (- page-bytesize page-rem))
         (chunk-size (tailor-chunk-size tailor))
         (chunk-rem (remainder chunk-size new-page-bytesize))]
    (~> (tailor-page-bytesize-set tailor new-page-bytesize)
        (tailor-chunk-size-set _ (- chunk-size chunk-rem)))))

(define (search schema query)
  (define entity-name (query-entity-name query))
  (let* [(table (lookup-table-in-schema schema entity-name))
         (attribute-value (query-attribute-value query))
         (new-tailor (update-tailor-page-size schema entity-name default-tailor))
         (pager (build-pagination schema query new-tailor))
         (how-many-leaves-ptr (malloc _int))
         (leaves-ptr (find_and_get_node (pager-tree pager) attribute-value how-many-leaves-ptr))
         (how-many-leaves (ptr-ref how-many-leaves-ptr _int))]
    (foldl (lambda [row-id search-results]
             (append
              (offset-lookup schema entity-name (pager-tailor pager) row-id)
              search-results))
           (list) (vector->list (convert-leaves leaves-ptr how-many-leaves)))))
