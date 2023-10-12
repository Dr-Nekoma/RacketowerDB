#lang racket

(require racket/tcp)

(module+ server
  (define default-port 8891)
  
  (define (server)
    (tcp-listen default-port))

  (define (server-entrypoint)
    (define listener (server))
    (define (loop)
        (define-values (in out) (tcp-accept listener))
        (thread (lambda ()
          (let ((file-out (open-output-file "backend.log" #:exists 'can-update)))
            (write (read in) file-out)
            (close-output-port file-out)
            (write "I saved your amazing message!" out)
            (close-output-port out))))
        (loop))
    (loop))
  
  (provide server-entrypoint)
  (provide default-port))