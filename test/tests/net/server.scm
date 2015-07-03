(import (rnrs)
	(net server)
	(sagittarius)
	(sagittarius socket)
	(rfc tls)
	(rfc x.509)
	(crypto)
	(srfi :18)
	(srfi :19)
	(srfi :64))

(test-begin "Simple server framework")

(define-constant +shutdown-port+ "7500")

;; use default config
;; no IPv6, no shutdown port and signel thread
(let ()
  (define (handler server socket)
    (let ((bv (socket-recv socket 255)))
      (socket-send socket bv)))
  (define server (make-simple-server "5000" handler))
  (test-assert "server?" (server? server))
  (server-start! server :background #t)
  ;; wait until it's started
  (thread-sleep! 0.1)

  (let ((sock (make-client-socket "localhost" "5000")))
    (socket-send sock (string->utf8 "hello"))
    (test-equal "echo back" (string->utf8 "hello") (socket-recv sock 255))
    (socket-close sock))

  (test-assert "stop server" (server-stop! server))
)

;; multi threading server
(let ()
  (define config (make-server-config :shutdown-port +shutdown-port+
				     :exception-handler 
				     (lambda (sr s e) (print e))
				     :max-thread 5
				     :use-ipv6? #t))
  (define (handler server socket)
    (let ((bv (socket-recv socket 255)))
      (socket-send socket bv)))
  (define server (make-simple-server "5000" handler :config config))
  (define (test ai-family)
    (let ((t* (map (lambda (_)
		     (make-thread
		      (lambda ()
			;; IPv6 may not be supported
			(guard (e (else "hello"))
			  (define sock (make-client-socket "localhost" "5000"
							   ai-family))
			  (thread-sleep! 0.2)
			  (socket-send sock (string->utf8 "hello"))
			  (let ((r (utf8->string (socket-recv sock 255))))
			    (socket-close sock)
			    r)))))
		   ;; more than max thread
		   '(1 2 3 4 5 6 7 8 9 10))))
      (test-equal "multi threaded server"
		  '("hello" "hello" "hello" "hello" "hello"
		    "hello" "hello" "hello" "hello" "hello")
		  (map thread-join! (map thread-start! t*)))))
  (test-assert "config?" (server-config? config))
  (test-assert "server-config" (eq? config (server-config server)))

  (server-start! server :background #t)
  (thread-sleep! 0.1)
  ;; test both sockets
  (test AF_INET6)
  (test AF_INET)

  (test-assert "stop server" (server-stop! server))
  (test-assert "server-stopped?" (server-stopped? server))
)

(let ()
  (define keypair (generate-key-pair RSA :size 1024))
  (define cert (make-x509-basic-certificate keypair 1
					    (make-x509-issuer '((C . "NL")))
					    (make-validity (current-date)
							   (current-date))
					    (make-x509-issuer '((C . "NL")))))

  (define config (make-server-config :shutdown-port +shutdown-port+
				     :secure? #t
				     :use-ipv6? #t
				     :certificates (list cert)))
  (define (handler server socket)
    (let ((bv (socket-recv socket 255)))
      (socket-send socket bv)))
  (define server (make-simple-server "5000" handler :config config))
  (define (test ai-family)
    ;; IPv6 may not be supported
    (guard (e (else #t))
      (let ((sock (make-client-tls-socket "localhost" "5000" ai-family)))
	(socket-send sock (string->utf8 "hello"))
	(test-equal "TLS echo back" 
		    (string->utf8 "hello") (socket-recv sock 255))
	(socket-close sock))))
  (server-start! server :background #t)
  (thread-sleep! 0.1)
  ;; test both socket
  (test AF_INET)
  (test AF_INET6)

  ;; stop server by accessing shutdown port
  (make-client-socket "localhost" +shutdown-port+)
  (test-assert "finish simple server (2)" (wait-server-stop! server))
  (test-assert "finish simple server (3)" (wait-server-stop! server))
  )

;; call #135
(let ()
  (define server (make-simple-server "12345" (lambda (s sock) #t)))

  (test-assert "socket not created"
	       (let ((s (make-server-socket "12345")))
		 (socket-close s))))

(test-end)
