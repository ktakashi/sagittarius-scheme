#!read-macro=sagittarius/bv-string
(import (rnrs)
	(net socket)
	(crypto)
	(rfc x.509)
	(srfi :18)
	(srfi :19)
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

(let ()
  (define server-socket (make-server-socket "0"))
  ;; addr is client socket
  (define (server-run wait)
    (lambda ()
      (guard (e (else #t))
	(let ((addr (socket-accept server-socket)))
	  (thread-sleep! wait)
	  (call-with-socket addr
	    (lambda (sock)
	      (let ((p (socket-port sock)))
		(call-with-port p
		  (lambda (p)
		    (put-bytevector p #*"hello"))))))))))
  (define port
    (number->string (socket-info-port (socket-info server-socket))))

  (let ()
    (define server-thread (make-thread (server-run 1)))
    (thread-start! server-thread)

    (let ((client-socket (make-client-socket "localhost" port
					     (socket-options (read-timeout 100)))))
      (test-error socket-read-timeout-error? (socket-recv client-socket 5))
      ;; On windows, this is ETIMEDOUT, so don't rely on it
      ;; (test-equal EWOULDBLOCK (socket-last-error client-socket))
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
      ;; (test-equal EWOULDBLOCK (socket-last-error client-socket))
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

  (socket-shutdown server-socket SHUT_RDWR)
  (socket-close server-socket)
  )

;; server read timeout
(let ()
  (define server-socket (make-server-socket "0"))
  (define (server-run)
    (guard (e (else #t))
      (let ((addr (socket-accept server-socket)))
	(socket-set-read-timeout! addr 100)
	(call-with-socket addr
	  (lambda (sock)
	    (guard (e (else e))
	      (socket-send sock (socket-recv sock 5))))))))
  (define port
    (number->string (socket-info-port (socket-info server-socket))))
  (let ()
    (define server-thread (make-thread server-run))
    (thread-start! server-thread)
    
    (let ((client-socket (make-client-socket "localhost" port
					     (socket-options
					      (read-timeout 1000)))))
      (thread-sleep! 0.2)
      (let ((r (thread-join! server-thread)))
	(test-assert (socket-read-timeout-error? r)))
      (socket-shutdown client-socket SHUT_RDWR)
      (socket-close client-socket)
      ))
  (socket-shutdown server-socket SHUT_RDWR)
  (socket-close server-socket)
  )

;; TLS
(define keypair (generate-key-pair RSA :size 1024))
(define 1year (make-time time-duration 0 (* 1 60 60 24 365)))
;; NB timezone must be set (probably with Z), so specifying zone offset 0.
(define cert (make-x509-basic-certificate keypair 1
              (make-x509-issuer '((C . "NL")))
              (make-validity (current-date 0)
			     (time-utc->date
			      (add-duration! (current-time) 1year) 0))
              (make-x509-issuer '((C . "NL")))))

(define client-keypair (generate-key-pair RSA :size 1024))
(define client-cert (make-x509-basic-certificate client-keypair 2
		     (make-x509-issuer '((C . "NL")))
		     (make-validity (current-date 0)
				    (time-utc->date
				     (add-duration! (current-time) 1year) 0))
		     (make-x509-issuer '((C . "NL")))))

(let ()
  (define (shutdown&close s)
    (tls-socket-shutdown s SHUT_RDWR)
    (tls-socket-close s))

  (define options (server-tls-socket-options
		   (certificates (list cert))
		   (trusted-certificates (list client-cert))
		   (private-key (keypair-private keypair))
		   (certificate-verifier #t)))
  (define server-socket (make-server-tls-socket "0" options))
  (define port
    (number->string (socket-info-port (socket-info server-socket))))
  (define (server-run wait)
    (lambda ()
      (guard (e (else #t))
	(let ((addr (socket-accept server-socket)))
	  (thread-sleep! wait)
	  (call-with-socket addr
	    (lambda (sock)
	      (let ((p (socket-port sock)))
		(call-with-port p
		  (lambda (p)
		    (put-bytevector p #*"hello"))))))))))

  (let ()
    (define server-thread (make-thread (server-run 1)))
    (thread-start! server-thread)

    ;; it's okey to use socket-options ;)
    (let ((client-socket (make-client-tls-socket "localhost" port
			   (socket-options (read-timeout 100)))))
      (test-error socket-read-timeout-error? (tls-socket-recv client-socket 5))
      (shutdown&close client-socket))))
  
(test-end)

