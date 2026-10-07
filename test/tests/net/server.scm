#!read-macro=sagittarius/bv-string
(import (rnrs)
	(net server)
	(net socket)
	(sagittarius)
	(sagittarius crypto keys)
	(util concurrent)
	(rfc x509)
	(srfi :18)
	(srfi :19)
	(srfi :64))

(define (print . args)
  (lock-port! (current-output-port))
  (for-each display args) (newline)
  (unlock-port! (current-output-port)))
(test-begin "Simple server framework")

(define-constant +shutdown-port+ "7500")

;; use default config
;; no IPv6, no shutdown port and signel thread

(print "basic test")
(let ()
  (define (handler server socket)
    (let ((bv (socket-recv socket 255)))
      (socket-send socket bv)))
  (define server (make-simple-server "0" handler))
  (test-assert "server?" (server? server))
  (server-start! server :background #t)
  ;; wait until it's started
  (thread-sleep! 0.1)

  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock (string->utf8 "hello"))
    (test-equal "echo back" (string->utf8 "hello") (socket-recv sock 255))
    (socket-close sock))

  (test-assert "stop server" (server-stop! server))
)

(print "thread limitation")
(let ()
  (define config (make-server-config :shutdown-port +shutdown-port+
				     :exception-handler
				     (lambda (sr s e) (print e))
				     :max-thread 5
				     :use-ipv6? #t))
  (define (handler server socket)
    (let ((bv (socket-recv socket 255)))
      (socket-send socket bv)))
  (define server (make-simple-server "0" handler :config config))
  (define (test ai-family)
    (let ((t* (map (lambda (_)
		     (make-thread
		      (lambda ()
			;; IPv6 may not be supported
			(guard (e (else "hello"))
			  (define sock
			    (make-client-socket "localhost" (server-port server)
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

  (test-assert (not (server-stopping? server)))
  (test-assert (server-stopped? server))
  
  (server-start! server :background #t)
  (thread-sleep! 0.1)
  ;; test both sockets
  (test AF_INET6)
  (test AF_INET)
  (test-assert "stop server" (server-stop! server))
  (test-assert "server-stopped?" (server-stopped? server))
)

(print "shutdown handler and alpn")
(let ()
  (define (shutdown-handler server socket)
    ;; some heavy authentication process here
    (let ((bv (socket-recv socket 5)))
      (test-equal #vu8(1 2 3 4 5) bv)
      (thread-sleep! 1)
      #t))
  (define keypair (generate-key-pair *key:rsa*))
  (define cert (make-x509-basic-certificate keypair 1
					    (make-x509-issuer '((C . "NL")))
					    (make-validity (current-date)
							   (current-date))
					    (make-x509-issuer '((C . "NL")))))

  (define config (make-server-config :shutdown-port +shutdown-port+
				     :secure? #t
				     :use-ipv6? #t
				     :certificates (list cert)
				     :private-key (key-pair-private keypair)
				     :shutdown-handler shutdown-handler
				     :alpn '("s0" "s1")))
  (define (handler server socket)
    (let ((alpn (tls-socket-selected-alpn socket))
	  (bv (socket-recv socket 255)))
      (let-values (((out e) (open-bytevector-output-port)))
	(put-bytevector out (string->utf8 alpn))
	(put-bytevector out #*":")
	(put-bytevector out bv)
	(socket-send socket (e)))))
  (define server (make-simple-server "0" handler :config config))
  (define (test ai-family)
    (define option (tls-socket-options (ai-family ai-family) (alpn* '("s1"))))
    ;; IPv6 may not be supported
    (guard (e (else #t))
      (let ((sock (make-client-tls-socket "localhost" (server-port server)
					  option)))
	(let ((alpn (tls-socket-selected-alpn sock)))
	  (test-equal "s1" alpn)
	  (socket-send sock #*"hello")
	  (thread-sleep! 0.1)
	  (test-equal "TLS echo back" #*"s1:hello" (socket-recv sock 255))
	  (socket-close sock)))))
  (server-start! server :background #t)
  (thread-sleep! 0.1)
  ;; test both socket
  (test AF_INET)
  (test AF_INET6)

  ;; stop server by accessing shutdown port
  (let ((s (make-client-socket "localhost" +shutdown-port+)))
    (socket-send s #vu8(1 2 3 4 5))
    (socket-close s))
  (test-assert "finish simple server (2)" (wait-server-stop! server))
  ;; a bit weird location, the flag is always true after the server is
  ;; stopped ...
  (test-assert "server stopping?" (server-stopping? server))
  (test-assert "finish simple server (3)" (wait-server-stop! server))
  )

;; call #135
(print "lazy socket creation")
(let ()
  (define server (make-simple-server "0" (lambda (s sock) #t)))

  (test-assert "socket not created"
	       (let ((s (make-server-socket (server-port server))))
		 (socket-close s))))

(print "server context")
(let ((server (make-simple-server "0" (lambda (s sock) #t)
				  :context 'context)))
  (test-equal 'context (server-context server))
  #;(test-error (server-status server)))

;; Test for socket detachment functionality in the simple server framework.
;;
;; This test verifies that a server can detach sockets and hand them off to
;; external actors for processing, while maintaining proper thread pool status.
;; The test creates a non-blocking server that detaches incoming connections
;; to a shared-queue-channel-actor which handles the actual socket
;; communication.
(print "socket detachment")
(let ()
  ;; the thread management is done outside of our threads
  ;; thus there's no way to guarantee. let's hope...
  (define (hope-it-works)
    (thread-yield!)
    (thread-sleep! 1))
  ;; Actor that receives detached sockets and handles them independently.
  (define detached-actor
    (make-shared-queue-channel-actor
     (lambda (input-receiver output-sender)
       (define socket (input-receiver))
       (output-sender 'ready)
       (hope-it-works)
       (let ((msg (input-receiver)))
	   (socket-send socket msg))
       (output-sender 'done)
       ;; Wait for finish signal.
       (input-receiver)
       (socket-shutdown socket SHUT_RDWR)
       (socket-close socket))))
  (define config (make-server-config
		  :non-blocking? #t :max-thread 5
		  :exception-handler print))
  (define server (make-simple-server
		  "0" (lambda (s sock)
			;; Remove socket from server's management.
			(server-detach-socket! s sock)
			;; Hand it over to external actor.
			(actor-send-message! detached-actor sock))
		  :config config))
  (define (check-status server)
    (let ((status (server-status server)))
      (test-assert (server-status? status))
      (test-assert "Max 5 threads" (<= (server-status-thread-count status) 5))
      (test-equal server (server-status-target-server status))
      ;; always 0
      (test-equal 0 (length (server-status-thread-statuses status)))
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

  (let ((sock (make-client-socket "localhost" (server-port server))))
    ;; Trigger socket detachment by connecting.
    (socket-send sock #vu8(0))
    (test-equal 'ready (actor-receive-message! detached-actor))
    ;; Send actual data through the detached socket.
    (actor-send-message! detached-actor #vu8(1 2 3 4 5))
    (test-equal 'done (actor-receive-message! detached-actor))
    (hope-it-works)
    ;; it should have 0 active socket on the server, it's detached
    ;; and server socket is not closed
    (check-status server)
    ;; Signal actor to finish and close socket.
    (actor-send-message! detached-actor 'finish)
    ;; Handle potential race condition where socket closes before read.
    (guard (e ((socket-error? e) (test-assert "server socket closed" #t))
              (else (test-assert (condition-message e) #f)))
      (let ((bv (socket-recv sock 5)))
        (test-equal #vu8(1 2 3 4 5) bv)))
    (socket-shutdown sock SHUT_RDWR)
    (socket-close sock))

  (server-stop! server))

;; Client certificate
(print "client certificate")
(let ()
  (define kp1 (generate-key-pair *key:ecdsa*))
  (define kp2 (generate-key-pair *key:ecdsa*))
  (define one-year (make-time time-duration 0 (* 3600 24 365)))
  (define now (add-duration (current-time) (make-time time-duration 0 -300)))
  (define cert1 (make-x509-basic-certificate kp1 1
      (make-x509-issuer '((CN . "sagittarius-client-1")))
      (make-validity (time-utc->date now)
         (time-utc->date (add-duration now one-year)))
      (make-x509-issuer '((CN . "sagittarius-client-1")))))
  (define cert2 (make-x509-basic-certificate kp2 2
      (make-x509-issuer '((CN . "sagittarius-client-2")))
      (make-validity (time-utc->date now)
         (time-utc->date (add-duration now one-year)))
      (make-x509-issuer '((CN . "sagittarius-client-2")))))
  (define cert1-bv (x509-certificate->bytevector cert1))
  (define cert2-bv (x509-certificate->bytevector cert2))
  (define tls-config (make-server-tls-config
		      :trusted-certificates (list cert1 cert2)
		      :client-certificate-required? #t
		      :certificate-verifier #t))
  (define config (make-server-config :secure? #t
				     :tls-config tls-config
				     :exception-handler print
				     :close-socket? #t))
  (define lock (make-mutex))
  (define (app server sock)
    (mutex-lock! lock)
    (guard (e (else (mutex-unlock! lock)))
      (print "    = server: " sock)
      (socket-recv sock 255) ;; discard
      (let ((cert (tls-socket-peer-certificate sock)))
	(print "    = cert: " (x509-certificate? cert))
	(test-assert "client certificate" (x509-certificate? cert))
	(socket-send sock (x509-certificate->bytevector cert))
	(socket-shutdown sock SHUT_RDWR)
	(socket-close sock)
	)
      (mutex-unlock! lock)))
  (define server (make-simple-server "0" app :config config))
  (define option1
    (tls-socket-options
     (private-key (key-pair-private kp1))
     (certificates (list cert1))))
  (define option2
    (tls-socket-options
     (private-key (key-pair-private kp2))
     (certificates (list cert2))))

  (print "  - start server")
  (server-start! server :background #t)

  (print "  - ckient with cert 1")
  (mutex-lock! lock)
  (let ((sock (make-client-tls-socket "localhost" (server-port server) option1)))
    (socket-send sock #*"hello")
    (mutex-unlock! lock)
    (let ((cert (socket-recv sock 2048)))
      (test-assert "client cert #1" (bytevector->x509-certificate cert))
      (test-equal "client cert #1 bytes" cert1-bv cert))
    (socket-shutdown sock SHUT_RDWR)
    (socket-close sock))

  (print "  - ckient with cert 2")
  (mutex-lock! lock)
  (let ((sock (make-client-tls-socket "localhost" (server-port server) option2)))
    (socket-send sock #*"hello")
    (mutex-unlock! lock)
    (let ((cert (socket-recv sock 2048)))
      (test-assert "client cert #2" (bytevector->x509-certificate cert))
      (test-equal "client cert #2 bytes" cert2-bv cert)
      (test-assert "server cert result is refreshed"
                   (not (bytevector=? cert cert1-bv))))
    (socket-shutdown sock SHUT_RDWR)
    (socket-close sock))

  (print "  - ckient without certificate")
  (test-assert "no auth"
               (let ((sock #f))
		 (define (close sock)
		   (when sock
		     (socket-shutdown sock SHUT_RDWR)
                     (socket-close sock)))
		 (guard (e ((socket-error? e) (close sock) #t)
                           (else (close sock) #f))
		   (print "    - making socket")
		   (mutex-lock! lock)
		   (set! sock (make-client-tls-socket
                               "localhost" (server-port server)))
		   (print "    - sock: " sock)
		   (socket-send sock #*"hello")
		   (print "    - send socket done")
		   (mutex-unlock! lock)
                   (socket-recv sock 1)
		   (print "    - recv socket done")
		   (close sock)
		   #f)))

  (server-stop! server))

(test-end)
