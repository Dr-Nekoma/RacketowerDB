#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(provide insert
	 find_and_get_value
	 find_and_get_node
	 _RECORD
	 _RECORD-pointer
	 RECORD->list)

(define-cstruct _RECORD ([chunkNumber _int]
                         [pageNumber _int]
                         [slotNumber _int]))

(define-ffi-definer define-bplustree (ffi-lib "libbplustree"))

(define-bplustree insert (_fun _pointer _int _int _int _int -> _pointer))
(define-bplustree find_and_get_value (_fun _pointer _int _bool -> _pointer))
(define-bplustree find_and_get_node (_fun _pointer _int _pointer -> _pointer))
