#!read-macro=sagittarius/bv-string
(import (rnrs)
	(net socket)
	(only (sagittarius socket) make-server-socket)
	(srfi :18)
	(srfi :64))

(test-begin "(net socket): Supposed to be modern socket library")

;; default
(let ((so (socket-options)))
  (test-assert "socket-options?" (socket-options? so))
  (test-assert "non-blocking?" (not (socket-options-non-blocking? so)))
  (test-equal #f (socket-options-connection-timeout so))
  (test-equal #f (socket-options-read-timeout so)))

(let ((so (socket-options (non-blocking? #t)
			  (connection-timeout 10)
			  (read-timeout 100))))
  (test-assert "socket-options?" (socket-options? so))
  (test-assert "non-blocking?" (socket-options-non-blocking? so))
  (test-equal 10 (socket-options-connection-timeout so))
  (test-equal 100 (socket-options-read-timeout so))
  (test-equal AF_INET (socket-options-ai-family so)))

(let ((so (tls-socket-options)))
  (test-assert "socket-options?" (socket-options? so))
  (test-assert "tls-socket-options?" (tls-socket-options? so))
  (test-assert "non-blocking?" (not (socket-options-non-blocking? so)))
  (test-equal #f (socket-options-connection-timeout so))
  (test-equal #f (socket-options-read-timeout so))
  (test-assert (tls-socket-options-handshake so)))

(let ((so (tls-socket-options (non-blocking? #t)
			      (certificates '(foo)))))
  (test-assert "socket-options?" (socket-options? so))
  (test-assert "tls-socket-options?" (tls-socket-options? so))
  (test-assert "non-blocking?" (socket-options-non-blocking? so))
  (test-equal #f (socket-options-connection-timeout so))
  (test-equal #f (socket-options-read-timeout so))
  (test-assert (tls-socket-options-handshake so))
  (test-equal '(foo) (tls-socket-options-certificates so)))

(define (shutdown&close s)
  (socket-shutdown s SHUT_RDWR)
  (socket-close s))

(define echo-server-socket (make-server-socket "0"))
;; addr is client socket
(define (server-run wait)
  (lambda ()
    (guard (e (else #t))
      (let ((addr (socket-accept echo-server-socket)))
	(thread-sleep! wait)
	(call-with-socket addr
	  (lambda (sock)
	    (let ((p (socket-port sock)))
	      (call-with-port p
		(lambda (p)
		  (put-bytevector p #*"hello"))))))))))
(define port
  (number->string (socket-info-port (socket-info echo-server-socket))))


(let ()
  (define server-thread (make-thread (server-run 1)))
  (thread-start! server-thread)

  (let ((client-socket (make-client-socket "localhost" port
					   (socket-options (read-timeout 10)))))
    (test-error socket-read-timeout-error? (socket-recv client-socket 5))
    (test-equal EWOULDBLOCK (socket-last-error client-socket))
    (socket-shutdown client-socket SHUT_RDWR)
    (socket-close client-socket)
    ))
;; nonblocking
(let ()
  (define server-thread (make-thread (server-run 1)))
  (thread-start! server-thread)

  (let ((client-socket (make-client-socket "localhost" port
					   (socket-options
					    (non-blocking? #t)))))
    (test-equal #f (socket-recv client-socket 5))
    (test-equal EWOULDBLOCK (socket-last-error client-socket))
    (socket-shutdown client-socket SHUT_RDWR)
    (socket-close client-socket)
    ))

;; normal
(let ()
  (define server-thread (make-thread (server-run 0)))
  (thread-start! server-thread)

  (let ((client-socket (make-client-socket "localhost" port
					   (socket-options
					    (read-timeout 1000)))))
    (test-equal #*"hello" (socket-recv client-socket 5))
    (socket-shutdown client-socket SHUT_RDWR)
    (socket-close client-socket)
    ))

(test-end)

