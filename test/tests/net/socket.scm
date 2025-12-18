#!read-macro=sagittarius/bv-string
(import (rnrs)
	(net socket)
	(rfc x509)
	(srfi :1)
	(srfi :13)
	(srfi :18)
	(srfi :19)
	(srfi :64)
	(sagittarius atomic)
	(sagittarius threads)
	(util concurrent)
	(util concurrent atomic)
	(sagittarius crypto keys))

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

(let ((s* (make-server-socket* "0")))
  ;; should be 2, but we don't check the length in case IPv6 is not enabled
  (test-assert "make-server-socket*" (list? s*))
  (for-each socket-close s*))

(define (shutdown&close s)
  (socket-shutdown s SHUT_RDWR)
  (socket-close s))

(print "Testing basic socket operations")
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
			     (socket-options (read-timeout 100))))
	  (buffer (make-bytevector 5)))
      (test-error socket-read-timeout-error? (socket-recv client-socket 5))
      (test-error socket-read-timeout-error?
		  (socket-recv! client-socket buffer 0 5))
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
					      (read-timeout 1000000)))))
      (test-equal #*"hello" (socket-recv client-socket 5))
      (socket-shutdown client-socket SHUT_RDWR)
      (socket-close client-socket)
      ))

  (socket-shutdown server-socket SHUT_RDWR)
  (socket-close server-socket)
  )

;; server read timeout
(print "Testing server read timeout")
(let ()
  (define server-socket (make-server-socket "0"))
  (define (server-run)
    (guard (e (else #t))
      (let ((addr (socket-accept server-socket)))
	(socket-set-read-timeout! addr 1)
	(let ((r (guard (e (else e))
		   (socket-send addr (socket-recv addr 5)))))
	  (socket-shutdown addr SHUT_RDWR)
	  (socket-close addr)
	  r))))
  (define port
    (number->string (socket-info-port (socket-info server-socket))))
  (let ()
    (define server-thread (thread-start! (make-thread server-run)))
    (let ((client-socket (make-client-socket "localhost" port)))
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
(define keypair (generate-key-pair *key:rsa*))
(define 1year (make-time time-duration 0 (* 1 60 60 24 365)))
;; NB timezone must be set (probably with Z), so specifying zone offset 0.
(define cert (make-x509-basic-certificate keypair 1
              (make-x509-issuer '((C . "NL")))
              (make-validity (current-date 0)
			     (time-utc->date
			      (add-duration! (current-time) 1year) 0))
              (make-x509-issuer '((C . "NL")))))

(define client-keypair (generate-key-pair *key:rsa*))
(define client-cert (make-x509-basic-certificate client-keypair 2
		     (make-x509-issuer '((C . "NL")))
		     (make-validity (current-date 0)
				    (time-utc->date
				     (add-duration! (current-time) 1year) 0))
		     (make-x509-issuer '((C . "NL")))))

(print "Testing TLS sockets")
(let ()
  (define (shutdown&close s)
    (tls-socket-shutdown s SHUT_RDWR)
    (tls-socket-close s))

  (define options (server-tls-socket-options
		   (certificates (list cert))
		   (trusted-certificates (list client-cert))
		   (private-key (key-pair-private keypair))
		   (certificate-verifier #t)))
  (define server-socket (make-server-tls-socket "0" options))
  (define port
    (number->string (socket-info-port (socket-info server-socket))))
  (define (server-run wait)
    (lambda ()
      (guard (e (else #t))
	(let ((addr (socket-accept server-socket)))
	  (thread-sleep! wait)
	  (let ((p (socket-port addr)))
	    (call-with-port p
	      (lambda (p)
		(put-bytevector p #*"hello"))))))))
  
  (let ()
    (define server-thread (make-thread (server-run 1)))
    (thread-start! server-thread)

    ;; it's okey to use socket-options ;)
    (let ((client-socket (make-client-tls-socket "localhost" port
			   (socket-options (read-timeout 100))))
	  (buffer (make-bytevector 5)))
      (test-error socket-read-timeout-error? (tls-socket-recv client-socket 5))
      (test-error socket-read-timeout-error?
		  (tls-socket-recv! client-socket buffer 0 5))
      (shutdown&close client-socket))
    (thread-join! server-thread)))

(define (get-socket-count default)
  (or (and (cond ((getenv "FILE_LIMIT") =>
		  (lambda (v)
		    (min (div (- (string->number (string-trim-both v)) 20) 2) default)))
		 (else #f)))
      default))

(print "Testing server socket selector")
(let ()
  (define count (get-socket-count 100))
  (define (run-socket-selector hard-timeout soft-timeout)
    (define ready-sockets (make-atomic-fixnum 0))
    (define server-sock (make-server-socket "0"))
    (define mark #vu8(0))
    (define (echo sock e reuse-invoker)
      (define (close-socket s)
	(socket-shutdown sock SHUT_RDWR)
	(socket-close sock))
      (if e
	  (begin (socket-send sock mark) (reuse-invoker #f))
	  (let ((v (socket-recv sock 255)))
	    (if (bytevector=? v mark)
		(close-socket sock)
		(begin (socket-send sock v)
		       (reuse-invoker))))))
    (define server-thread
      (thread-start!
       (make-thread
	(lambda ()
	  (let-values (((socket-selector terminater)
			(make-socket-selector hard-timeout)))
	    (guard (e (else (print e) (terminater)))
	      (let loop ((i 0))
		(let ((sock (socket-accept server-sock)))
		  (unless sock (print "Server socket closed"))
		  (when sock
		    (atomic-fixnum-inc! ready-sockets)
		    (socket-send sock #vu8(1)) ;; let it wake up
		    (socket-selector sock echo soft-timeout)
		    (loop (+ i 1)))))
	      (terminater)))))))
    (define result (make-lock-free-queue))
    (define server-port
      (and server-sock
	   (number->string (socket-info-port (socket-info server-sock)))))

    (define (caller i)
      (thread-start!
       (make-thread
	(lambda ()
	  (guard (e (else (cons #f e)))
	    (let ((s (make-client-socket "localhost" server-port))
		  (msg (string->utf8
			(string-append "Hello world " (number->string i)))))
	      (socket-recv s 1) ;; wait for wake up :)
	      (socket-set-read-timeout! s 1000)
	      (guard (e (else (socket-shutdown s SHUT_RDWR)
			      (cons s e)))
		(when (even? i) (thread-yield!) (thread-sleep! 0.2));; 200ms
		(socket-send s msg)
		(let ((v (socket-recv s 255)))
		  (cond ((zero? (bytevector-u8-ref v 0)) ;; mark
			 (lock-free-queue-push! result #f))
			(else (lock-free-queue-push! result (utf8->string v)))))
		(socket-send s mark)
		(socket-shutdown s SHUT_RDWR)
		(cons s #f)))))
	(string-append "client-thread-" (number->string i)))))
    (define (safe-join! t)
      (guard (e ((uncaught-exception? e)
		 (uncaught-exception-reason e))
		(else #f))
	(thread-join! t 0.1)))
    (define (safe-close v)
      (when v
	(let ((s (car v))
	      (e (cdr v)))
	  (when (socket? s) (socket-close s))
	  (when (condition? e) (print (condition-message e))))))

    (let ((t* (map caller (iota count))))
      (do ((i 0 (+ i 1)))
	  ((or (= count (atomic-fixnum-load ready-sockets)) (= i 50)))
	(thread-sleep! 0.01))
      (print "Socket counts: " (atomic-fixnum-load ready-sockets))
      (for-each safe-close (map safe-join! t*)))
    ;; when all the threads are done, the socket must be the same
    ;; as count
    (test-equal "Expected client sockets count" count
		(atomic-fixnum-load ready-sockets))
    (socket-shutdown server-sock SHUT_RDWR)
    (socket-close server-sock)
    (guard (e (else #t)) (thread-join! server-thread 0.1))
    (lock-free-queue->list result))
  (let ((r (run-socket-selector 1000 #f)))
    (cond-expand (openbsd (test-expect-fail 1)) (else #t))
    (test-equal "no soft, hard = 1000ms" count (length (filter string? r))))
  (let ((r (run-socket-selector 50 #f)))
    ;; for some reason, Sagittarius is extremely slow on OpenBSD and 50ms of
    ;; waiting time might be too soon to be executed.
    (cond-expand (openbsd (test-expect-fail 1)) (else #t))
    (test-assert "no soft, hard = 50ms"
		 (<= (length (filter string? r)) (div count 2))))
  (let ((r (run-socket-selector 50 1000)))
    (cond-expand (openbsd (test-expect-fail 1)) (else #t))
    (test-equal "soft = 1000ms, hard = 50ms" count (length (filter string? r))))
  )

(print "Testing client socket selector")
(let ()
  (define server-sock (make-server-socket "0"))
  (define (close-socket! sock)
    (when sock
      (socket-shutdown sock SHUT_RDWR)
      (socket-close sock)))
  (define (echo sock)
    (lambda ()
      (guard (e (else (print e)))
	(let ((v (socket-recv sock 255)))
	  (thread-yield!)
	  (thread-sleep! 0.3) ;; wait 300ms
	  (socket-send sock v)
	  (thread-sleep! 0.5) ;; wait 500ms to let client finish
	  ))
      (close-socket! sock)))
  (define server-thread
    (thread-start!
     (make-thread
      (lambda ()
	(guard (e (else (close-socket! server-sock)
			(set! server-sock #f)
			(print e)))
	  (let loop ()
	    (let ((sock (socket-accept server-sock)))
	      (when sock
		(socket-send sock #vu8(1)) ;; send wake up
		(guard (e (else (report-error e) (close-socket! sock)))
		  (socket-set-read-timeout! sock 2000) ;; 2s
		  (thread-start! (make-thread (echo sock))))
		(loop)))))))))
  (define count (get-socket-count 100))
  (define (run-socket-selector hard-timeout soft-timeout)
    (define result (make-shared-queue))
    (define counter (make-atomic-fixnum 0))
    (define server-port
      (number->string (socket-info-port (socket-info server-sock))))
    (define ((caller selector) i)
      (define (push-result sock e reuse)
	(atomic-fixnum-inc! counter)
	(shared-queue-put! result
	 (thread-start!
	  (make-thread
	   (lambda ()	      
	     (guard (ex (else (close-socket! sock) #;(print ex) ex))
	       (let ((v (or e (utf8->string (socket-recv sock 255)))))
		 (close-socket! sock)
		 (if (string? v)
		     (and (not (zero? (string-length v))) v)
		     e))))))))
      (guard (e (else (shared-queue-put! result
		       (thread-start! (make-thread (lambda () (raise e)))))))
	(let ((s (make-client-socket "localhost" server-port
				     (socket-options (read-timeout 2000)))))
	  (socket-recv s 1) ;; wait for wake up
	  (socket-set-read-timeout! s 1000)
	  (socket-send s (string->utf8
			  (string-append "Hello world " (number->string i))))
	  (selector s push-result soft-timeout))))
    (define (collect-thread)
      (format #t "Collecting threads...~!")
      ;; all the sockets must be in the selector,
      ;; so waiting for 1.5s here should conver all the sockets
      ;; execution (incl. timeout)
      (thread-yield!)
      (thread-sleep! 1.5)
      (do ((i 0 (+ i 1)) (r '() (cons (shared-queue-get! result 0.1) r)))
	  ((= i count) (format #t "done!~%") r)))
    (define (safe-join! t)
      (guard (e ((uncaught-exception? e)
		 (uncaught-exception-reason e))
		(else #f))
	(thread-join! t 0.1)))

    (let-values (((selector terminator) (make-socket-selector hard-timeout)))
      (format #t "Start client selector test - ~a:~a~%" hard-timeout soft-timeout)
      (for-each (caller selector) (iota count))
      (let ((r (map (lambda (t?) (and t? (safe-join! t?))) (collect-thread))))
	(format #t "Terminating socket selector...~!")
	(terminator)
	(format #t "done!~%")
	(values (atomic-fixnum-load counter) r))))

  (let-values (((c r) (run-socket-selector 1000 #f)))
    (cond-expand (openbsd (test-expect-fail 2)) (else #t))
    (test-equal "counter (1)" count c)
    (test-equal "hard 1000ms soft #f" count (length (filter string? r))))
  (let-values (((c r) (run-socket-selector 50 #f)))
    (cond-expand (openbsd (test-expect-fail 2)) (else #t))
    (test-equal "counter (2)" count c)
    (test-equal "hard 50ms soft #f" 0 (length (filter string? r))))
  (let-values (((c r) (run-socket-selector 50 1000)))
    (cond-expand (openbsd (test-expect-fail 2)) (else #t))
    (test-equal "counter (3)" count c)
    (test-equal "hard 50ms soft 1000ms" count (length (filter string? r))))
  (close-socket! server-sock)
  (guard (e (else #t)) (thread-join! server-thread 0.1)))

(test-end)

