#lang racket

(require racket/tcp)

(module+ server
  (provide
    server-socket
    default-port)

  (define default-port 8891)

  (define (server)
    (tcp-listen default-port))

  (define (server-socket)
    (define listener (server))
    (let loop []
      (define-values (in out) (tcp-accept listener))
      (thread
        (lambda []
          (let [(file-out (open-output-file "backend.log" #:exists 'can-update))]
            (write (read in) file-out)
            (close-output-port file-out)
            (write "I saved your amazing message!" out)
            (close-output-port out))))
      (loop))))
