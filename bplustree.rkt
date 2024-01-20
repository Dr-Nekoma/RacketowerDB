#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(provide insert
	 find_and_get_value)
 
(define-ffi-definer define-bplustree (ffi-lib "libbplustree"))

(define-bplustree print_leaves (_fun _pointer -> _void))

(define-bplustree insert (_fun _pointer _int _int -> _pointer))
(define-bplustree find_and_print (_fun _pointer _int _bool -> _void))
(define-bplustree find_and_get_value (_fun _pointer _int _bool -> _int))

(define (tree-test)
  (let* [(tree (insert false 1 10))
         (tree2 (insert tree 2 11))
         (tree3 (insert tree2 3 12))]
    (print_leaves tree3)))
