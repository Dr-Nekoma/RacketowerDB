#lang racket

(require
 ffi/unsafe
 (only-in RacketowerDB/util checked-guard))

(struct page [instances indexes] #:transparent
  #:guard
  (checked-guard
   [(instances . (vectorof bytes?))
    (indexes . (vectorof (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)))]
   (values instances indexes)))

(struct pager [pages tree amount-read] #:transparent
  #:guard
  (checked-guard
   [(pages . (vectorof page?))
    (tree .  cpointer?)
    (amount-read . exact-nonnegative-integer?)]
   (values pages tree amount-read)))
