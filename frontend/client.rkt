#lang racket

(require racket/tcp)

(define (client-socket)
  (define-values (in out) (tcp-connect "localhost" 8891))
  (write "RacketowerDB test drive" out)
  (close-output-port out)
  (display (read in)))