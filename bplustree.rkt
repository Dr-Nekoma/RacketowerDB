#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(provide insert
	 find_and_get_value
	 find_and_get_node
	 _NODE
	 _RECORD
	 _RECORD-pointer
	 RECORD->list)

(define-cstruct _RECORD ([chunkNumber _int]
                         [pageNumber _int]
                         [slotNumber _int]))

(define-cstruct _NODE ([pointers _pointer]
                       [keys _pointer]
                       [parent _pointer]
		       [is_leaf _bool]
		       [num_keys _int]
		       [next _pointer]))

(define-ffi-definer define-bplustree (ffi-lib "libbplustree"))

(define-bplustree insert (_fun _pointer _int _int _int _int -> _pointer))
(define-bplustree find_and_get_value (_fun _pointer _int _bool -> _pointer))
(define-bplustree find_and_get_node (_fun _pointer _int _pointer -> _pointer))
