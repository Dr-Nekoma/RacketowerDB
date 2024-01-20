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

(struct page [instances indexes] #:transparent
  #:guard
  (checked-guard
   [(instances . (listof (listof byte?)))
    (indexes . (listof (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)))]
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

(define default-page-bytesize 8000)
(define default-max-pages-at-once 64)
(define default-chunk-size (* default-max-pages-at-once default-page-bytesize))
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

(define (build-pages schema query tailor)
  (let [(entity-name (query-entity-name query))
        (attribute-to-index (query-attribute-to-index query))
        (initial-page-number (tailor-initial-page-number tailor))
        (instances-per-page (tailor-instances-per-page tailor))
        (chunk-size (tailor-chunk-size tailor))
        (instance-size (tailor-instance-size tailor))
        (amount-already-read (tailor-amount-already-read tailor))]
    ;; TODO: Leave an error message to only work on the supported types.
    (define (query data attribute-to-index)
      (first (map (lambda [row] (integer32-value (hash-ref row attribute-to-index))) data)))
    (define (create-indexes instances schema entity-name attribute-to-index)
      (map (lambda (page-offset instance)
             (let* [(instance-bytes (list->bytes instance))
                    (read-content (read-table-values-from-disk schema entity-name #:source instance-bytes))
                    (attempt-to-find (query read-content attribute-to-index))]
               ;; TODO: We are missing some error checking in here
               (cons attempt-to-find page-offset)))
           (range (length instances))
           instances))
    (define file-name (build-ndf-filename entity-name #:data? 'data))
    (define content-length (file-size file-name))
    (define reader (open-input-file file-name #:mode 'binary))
    (let loop [(amount-already-read amount-already-read)
               (page-offset initial-page-number)
               (pages (list))]
      (if (< page-offset chunk-size)
          (begin
            (file-position reader (* page-offset instance-size))
            (let* [(amount-to-read (if (> (* instances-per-page instance-size) (- content-length amount-already-read))
                                       (- content-length amount-already-read)
                                       (* instances-per-page instance-size)))
                   (buffer (bytes->list (read-bytes amount-to-read reader)))
                   (instances (take-up-to (split-by buffer instance-size) chunk-size))
                   (indexes (create-indexes instances schema entity-name attribute-to-index))]
              (loop
               (+ amount-already-read amount-to-read)
               (+ instances-per-page page-offset)
               (append pages (list (page instances indexes))))))
          (values pages (tailor-amount-already-read-set tailor amount-already-read))))))

(define (build-pagination schema query tailor)
  (define (create-b-plus-tree pages)
    (foldl (lambda (page tree)
             (foldl (lambda (element tree)
                      (insert tree (car element) (cdr element)))
                    tree (page-indexes page)))
           false pages))
  (let [(entity (hash-ref schema (query-entity-name query)))]
    (cond
      [(table? entity)
       (let* [(instance-size (table-row-size entity))]
         (define-values (pages new-tailor)
           (build-pages
            schema
            query
            (tailor-instance-size-set tailor instance-size)))
         (pager
          pages
          (create-b-plus-tree pages)
          new-tailor))]
      [else (println "Did not find the entity name")])))

(define (search schema query)
  (let [(entity (hash-ref schema (query-entity-name query)))]
   (cond
      [(table? entity)
       (let* [(entity-name (query-entity-name query))
              (attribute-value (query-attribute-value query))
              (pager (build-pagination schema query default-tailor))
              (index (find_and_get_value (pager-tree pager) attribute-value false))]
         (if (= -1 index)
             (println "Did not find your thing buddy xD")
             ;; TODO: We should generalize this to not just use the first page
             (let* [(pages (pager-pages pager))
                    (first-page (car pages))
                    (instances (page-instances first-page))
                    (payload (list->bytes (list-ref instances index)))]
               (read-table-values-from-disk schema entity-name #:source payload))))]
      [else (println "Did not find the entity name")])))
