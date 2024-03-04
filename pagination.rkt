#lang racket

(require
 ffi/unsafe
 struct-update
 threading
 (only-in RacketowerDB/util checked-guard build-ndf-filename split-by take-up-to) 
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
   [(key . exact-nonnegative-integer?) ; This should be generic later on
    (columns . hash?)
    (row-id . row-id?)]
   (values key columns row-id)))

(struct page [instances indexes] #:transparent
  #:guard
  (checked-guard
   [(instances . (listof hash?))
    (indexes . (listof index?))]
   (values instances indexes)))

(struct tailor [initial-page-number page-bytesize instances-per-page chunk-size instance-size amount-already-read] #:transparent
  #:guard
  (checked-guard
   [(initial-page-number . exact-nonnegative-integer?)
    (page-bytesize . exact-nonnegative-integer?)
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
   (values initial-page-number page-bytesize instances-per-page* chunk-size instance-size* amount-already-read)))
(define-struct-updaters tailor)

(define default-page-bytesize 11)
(define default-max-pages-at-once 64)
(define default-chunk-size (* default-max-pages-at-once default-page-bytesize))
(define initial-chunk-number 0)
(define default-tailor (tailor 0 default-page-bytesize false default-chunk-size false 0))

(struct query [entity-name attribute-to-index attribute-value] #:transparent
  #:guard
  (checked-guard
   [(entity-name . string?)
    (attribute-to-index . string?)
    (attribute-value . exact-nonnegative-integer?)]
   (values entity-name attribute-to-index attribute-value)))
(define-struct-updaters query)

(struct pager [pages tree tailor] #:transparent
  #:guard
  (checked-guard
   [(pages . (listof page?))
    (tree .  cpointer?)
    (tailor . tailor?)]
   (values pages tree tailor)))

(define (chunk-by-size chunk-size elements)
    (let recur [(n chunk-size)
                (chunk '())
                (elements elements)]
      (cond
       ((empty? elements) (if (empty? chunk)
                            '()
                            (list (reverse chunk))))
       ((zero? n) (cons (reverse chunk) (chunk-by-size chunk-size elements)))
       (else (recur (sub1 n) (cons (car elements) chunk) (cdr elements))))))

(define (build-pages schema query tailor)
  (let [(entity-name (query-entity-name query))
        (attribute-to-index (query-attribute-to-index query))
        (initial-page-number (tailor-initial-page-number tailor))
        (instances-per-page (tailor-instances-per-page tailor))
        ;; TODO: Loop over all the remaining chunks
        (chunk-size (tailor-chunk-size tailor))
        (chunk-number initial-chunk-number)
        (instance-size (tailor-instance-size tailor))
        (amount-already-read (tailor-amount-already-read tailor))]

    (define (index-builder chunk-number page-number instance-number columns)
      (~> (hash-ref columns (query-attribute-to-index query) (lambda () #f))
          integer32-value ;; TODO: Leave an error message to only work on the supported types.
          (index _ columns (row-id chunk-number page-number instance-number))))
    (define (create-pages buffer instance-size instances-per-page)
      (define raw-pages
        (~> buffer
            (chunk-by-size instance-size _)
            (chunk-by-size instances-per-page _)))
      (define pages
        (for/list [(page-number (in-naturals))
                   (instances raw-pages)]
          (define indexing
            (for/list [(instance-number (in-naturals))
                       (instance instances)]
              (~> (read-table-values-from-disk schema entity-name #:source (list->bytes instance))
                  car
                  (index-builder chunk-number page-number instance-number _))))
          (page (map (lambda (index) (index-columns index)) indexing)
                indexing)))
      (println pages)
      pages)
    (define file-name (build-ndf-filename entity-name #:data? 'data))
    (define content-length (file-size file-name))
    (define reader (open-input-file file-name #:mode 'binary))
    (let loop [(amount-already-read amount-already-read)
               (page-offset initial-page-number)
               (pages (list))]
      (if (< amount-already-read (min chunk-size content-length))
          (begin
            (file-position reader (* page-offset instance-size))
            (let* [(amount-to-read (if (> (* instances-per-page instance-size) (- content-length amount-already-read))
                                       (- content-length amount-already-read)
                                       (* instances-per-page instance-size)))
                   (buffer (bytes->list (read-bytes amount-to-read reader)))]
              (loop
               (+ amount-already-read amount-to-read)
               (+ instances-per-page page-offset)
               (append pages (create-pages buffer instance-size instances-per-page)))))
          (values pages (tailor-amount-already-read-set tailor amount-already-read))))))

(define (build-pagination schema query tailor)
  (define (create-b-plus-tree pages)
    (foldl (lambda (page tree)
             (foldl (lambda (element tree)
                      (let* [(key (index-key element))
                             (row-id (index-row-id element))
                             (chunk-number (row-id-chunk-number row-id))
                             (page-number (row-id-page-number row-id))
                             (slot-number (row-id-slot-number row-id))]
                      (insert tree key chunk-number page-number slot-number)))
                    tree (page-indexes page)))
           false pages))
  (let* [(entity (hash-ref schema (query-entity-name query)))
         (instance-size (table-row-size entity))]
    (define-values (pages new-tailor)
      (build-pages
       schema
       query
       (tailor-instance-size-set tailor instance-size)))
    (pager
     pages
     (create-b-plus-tree pages)
     new-tailor)))

(define (convert-leaves ptr size)
  (~> (cast ptr _pointer (_array/vector _RECORD-pointer size))
      (vector-map (lambda (pointer) (ptr-ref pointer _RECORD)) _)))

(define (derive-offset schema table-name tailor row-id-ptr)
  (define row-size (table-row-size (hash-ref schema table-name)))
  (define chunk-size (tailor-chunk-size tailor))
  (define page-size (tailor-page-bytesize tailor))
  (define row-id-value (apply row-id (RECORD->list (ptr-ref row-id-ptr _RECORD))))
  (define chunk-number (row-id-chunk-number row-id-value))
  (define page-number (row-id-page-number row-id-value))
  (define slot-number (row-id-slot-number row-id-value))
  (values row-size (+ (* chunk-size chunk-number) (* page-size page-number) (* row-size slot-number))))

(define (offset-lookup schema table-name tailor row-id)
  (define-values (row-size offset) (derive-offset schema table-name tailor row-id))
  (define file-name (build-ndf-filename #:data? 'data table-name))
  (define input (open-input-file file-name #:mode 'binary))
  (define payload (begin
                    (file-position input offset)
                    (read-bytes row-size input)))
  (read-table-values-from-disk schema table-name #:source payload))

(define (search schema query)
  (let* [(table-name (query-entity-name query))
         (entity (hash-ref schema table-name))]
   (cond
      [(table? entity)
       (let* [(entity-name (query-entity-name query))
              (attribute-value (query-attribute-value query))
              (pager (build-pagination schema query default-tailor))
              (how-many-leaves-ptr (malloc _int))
              (leaves-ptr (find_and_get_node (pager-tree pager) attribute-value how-many-leaves-ptr))
              (how-many-leaves (ptr-ref how-many-leaves-ptr _int))
              (leaves (convert-leaves leaves-ptr how-many-leaves))]
         (println pager)
         (println leaves)
         (unless (= 0 how-many-leaves)
           (println how-many-leaves)
           (foldl (lambda [row-id search-results]
                    (println row-id)
                    (println search-results)
                    (append
                     (offset-lookup schema table-name (pager-tailor pager) row-id)
                     search-results))
                  (list) (vector->list leaves))))]
      [else (println "Did not find the entity name")])))
