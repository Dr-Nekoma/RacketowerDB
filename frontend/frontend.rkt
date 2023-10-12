#lang racket

(require racket/tcp)

(module+ client
  (require (submod RacketowerDB/backend server))
  (define (client-socket)
    (define-values (in out) (tcp-connect "localhost" default-port))
    (write "RacketowerDB test drive" out)
    (close-ouput-port out)
    (display (read in))))
