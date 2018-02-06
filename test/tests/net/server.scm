(import (rnrs)
	(net server)
	(sagittarius)
	(sagittarius socket)
	(util concurrent)
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
				     :certificates (list cert)
				     :private-key (keypair-private keypair)))
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

(let ((server (make-simple-server "12345" (lambda (s sock) #t)
				  :context 'context)))
  (test-equal 'context (server-context server))
  (test-error (server-status server)))

(let ()
  ;; the thread management is done outside of our threads
  ;; thus there's no way to guarantee. let's hope...
  (define (hope-it-works)
    (thread-yield!)
    (thread-sleep! 1))
  (define detached-actor
    (make-shared-queue-channel-actor
     (lambda (input-receiver output-sender)
       (define socket (input-receiver))
       (output-sender 'ready)
       (hope-it-works)
       (let ((msg (input-receiver)))
	 (socket-send socket msg))
       (output-sender 'done)
       (input-receiver)
       (socket-shutdown socket SHUT_RDWR)
       (socket-close socket))))
  (define config (make-server-config
		  :non-blocking? #t :max-thread 5
		  :exception-handler print))
  (define server (make-simple-server
		  "12345" (lambda (s sock)
			    (server-detach-socket! s sock)
			    (actor-send-message! detached-actor sock))
		  :config config))
  (define (check-status server)
    (let ((status (server-status server)))
    (test-assert (server-status? status))
    (test-equal 5 (server-status-thread-count status))
    (test-equal server (server-status-target-server status))
    (test-equal 5 (length (server-status-thread-statuses status)))
    (for-each (lambda (ts)
		(test-assert (number? (thread-status-thread-id ts)))
		(test-assert (string? (thread-status-thread-info ts)))
		(test-equal 0 (thread-status-active-socket-count ts)))
	      (server-status-thread-statuses status))
    (test-assert
     (call-with-string-output-port
      (lambda (out) (report-server-status status out))))))
  
  (server-start! server :background #t)
  (test-assert (server-status server))
  (check-status server)

  (let ((sock (make-client-socket "localhost" "12345")))
    (socket-send sock #vu8(0))
    (test-equal 'ready (actor-receive-message! detached-actor))
    (actor-send-message! detached-actor #vu8(1 2 3 4 5))
    (test-equal 'done (actor-receive-message! detached-actor))
    (hope-it-works)
    ;; it should have 0 active socket on the server, it's detached
    ;; and server socket is not closed
    (check-status server)
    (actor-send-message! detached-actor 'finish)
    (let ((bv (socket-recv sock 5)))
      (test-equal #vu8(1 2 3 4 5) bv))
    (socket-shutdown sock SHUT_RDWR)
    (socket-close sock))
  
  (server-stop! server))

(test-end)
