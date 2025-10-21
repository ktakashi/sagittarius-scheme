;; -*- scheme -*-
(add-load-path "./socket")
(add-load-path "./threads")
(add-load-path "./time")

#!read-macro=sagittarius/bv-string
(import (rnrs)
	(clos user)
	(sagittarius socket)
	(sagittarius) ;; for format
	;; use thread for testing
	(sagittarius threads)
	(sagittarius time)
	(sagittarius atomic)
	(prefix (binary io) b:)
	(util bytevector)
	(util concurrent)
	(util duration)
	(srfi :1 lists)
	(srfi :13 strings)
	(srfi :64 testing)
	(srfi :106))

(define (shutdown&close s)
  (socket-shutdown s SHUT_RDWR)
  (socket-close s))

(define echo-server-socket (make-server-socket "0"))
(define echo-server-queue (make-shared-queue))
;; addr is client socket
(define (server-run)
  (define stop? #f)
  (let loop ()
    (guard (e (else #t))
      (let ((addr (socket-accept echo-server-socket)))
	(call-with-socket addr
	  (lambda (sock)
	    (let ((p (socket-port sock)))
	      (call-with-port p
		(lambda (p)
		  (let lp2 ((l (b:get-line p)))
		    (let ((r (bytevector-trim-right l)))
		      (cond
		       ((or (eof-object? r) (bytevector=? r #*"end"))
			(set! stop? #t))
		       ((bytevector=? r #*"test-end")
			(put-bytevector p #vu8()))
		       (else
			(let ((res (bytevector-append r #*"\r\n")))
			  ;; wait one sec
			  (when (bytevector=? r #*"wait") (thread-sleep! 1))
			  (put-bytevector p res 0 (bytevector-length res))
			  (when (bytevector=? r #*"push")
			    (shared-queue-put! echo-server-queue #t))
			  (lp2 (b:get-line p))))))))))))))
    (unless stop? (loop))))
(define server-thread (make-thread server-run))

(define server-port
  (number->string (socket-info-port (socket-info echo-server-socket))))

(test-begin "Sagittarius socket")

;; start echo server
(thread-start! server-thread)

(let ((s (make-client-socket "localhost" server-port)))
  ;; it returns 0, but should we check?
  (let ((to (socket-get-read-timeout s)))
    (test-assert "socket timeout (not set)" (time? to))
    (test-equal time-duration (time-type to))
    (test-equal 0 (time-second to))
    (test-equal 0 (time-nanosecond to)))
  
  (socket-set-read-timeout! s 1000) ;; 1000ms
  (let ((to (socket-get-read-timeout s)))
    (test-assert "socket timeout" (time? to))
    (test-equal time-duration (time-type to))
    (test-equal 1 (time-second to))
    (test-equal 0 (time-nanosecond to)))
  (socket-shutdown s SHUT_RDWR)
  (socket-close s))

(test-error "ai-passive" assertion-violation?
	    (make-client-socket #f server-port 0 0 AI_PASSIVE))

(let ((client-socket (make-client-socket "localhost" server-port)))
  (test-assert "socket?"(socket? client-socket))
  (test-equal "raw socket-send"
	      (+ (string-length "push") 2) ;; for \r\n
	      (socket-send client-socket (string->utf8 "push\r\n") 0))
  (shared-queue-get! echo-server-queue)
  ;; these are not documented
  (test-assert "socket-read-select"
	       (not (null? (socket-read-select #f client-socket))))
  (test-assert "socket-write-select"
	       (not (null? (socket-write-select #f client-socket))))
  ;; does socket even have error fd?
  ;; (test-assert "socket-error-select"
  ;;               (null? (socket-error-select 100 client-socket)))

  ;; fdset
  ;; `socket-select!`, `socket-select` and fdset related procedures
  ;; are not documented so may change especially socket doesn't have
  ;; error side FD so I think it's useless.
  (let ((fdset (make-fdset)))
    (test-assert "fdset?" (fdset? fdset))
    (test-assert "fdset-set! (1)" (fdset-set! fdset client-socket #t))
    (test-assert "fdset-ref  (1)" (fdset-ref fdset client-socket))
    (test-assert "fdset-set! (2)" (fdset-set! fdset client-socket #f))
    (test-assert "fdset-ref  (2)" (not (fdset-ref fdset client-socket)))
    (test-assert "fdset-set! (3)" (fdset-set! fdset client-socket #t))
    ;; it's ready already
    ;; what's good for specifying error fds?
    (let-values (((n r w e) (socket-select! fdset #f #f #f)))
      (test-equal "socket-select!" 1 n)
      (test-assert "socket-select!" (fdset? r))
      (test-assert "socket-select!" (not w))
      (test-assert "socket-select!" (not e)))

    (let-values (((n r w e) (socket-select fdset #f #f #f)))
      (test-equal "socket-select" 1 n)
      (test-assert "socket-select" (fdset? r))
      (test-assert "socket-select" (not w))
      (test-assert "socket-select" (not e)))
    
    (test-assert "fdset-ref  (3)" (fdset-ref fdset client-socket))
    (let ((l (list client-socket)))
      (test-equal "collect-sockets" l (collect-sockets fdset))
      (test-assert "sockets->fdset" (fdset? (sockets->fdset l)))))

  (test-equal "raw socket-recv"
	      (string->utf8 "push\r\n")
	      (socket-recv client-socket (+ (string-length "push") 2) 0))

  (test-equal "raw socket-send (2)"
	      (+ (string-length "hello") 2) ;; for \r\n
	      (socket-send client-socket (string->utf8 "hello\r\n") 0))
  (test-equal "raw socket-recv!"
	      (+ (string-length "hello") 2)
	      (let ((bv (make-bytevector (+ (string-length "hello") 2))))
		(socket-recv! client-socket bv 0 
			      (+ (string-length "hello") 2) 0)))

  ;; make port
  (let ((port (socket-port client-socket)))
    (test-assert "port?" (port? port))
    (test-assert "binary-port?" (binary-port? port))
    (test-assert "input-port?" (input-port? port))
    (test-assert "output-port?" (output-port? port))

    (put-bytevector port (string->utf8 "put from port\r\n"))
    (test-equal "get-bytevector-n"
		(string->utf8 "put from port\r\n")
		(get-bytevector-n port
				  (string-length "put from port\r\n")))
    ;; textual
    (let ((text-port (transcoded-port port
				      (make-transcoder (utf-8-codec)
						       'crlf))))
      (put-string text-port "put from text port\r\n")
      (test-equal "get-line" "put from text port" (get-line text-port))
      ;; end test
      (put-string text-port "test-end\r\n")
      )))

(let ((client-socket (make-client-socket "localhost" server-port)))
  (socket-nonblocking! client-socket)
  (test-equal "raw nonblocking socket-send"
	      (+ (string-length "wait") 2)
	      (socket-send client-socket (string->utf8 "wait\r\n") 0))
  (test-equal "raw nonblocking socket-recv"
	      #f
	      (socket-recv client-socket (+ (string-length "hello\r\n") 2) 0))
  (socket-send client-socket (string->utf8 "test-end\r\n") 0)
  (test-assert "socket-close" (socket-close client-socket))
  (test-assert "socket-closed? (1)" (socket-closed? client-socket))
  )


;; call #125
(let* ((client-socket (make-client-socket "localhost" server-port))
       (in/out (socket-port client-socket))
       (msg "hello\n"))
  (define (ensure-n in n)
    (let loop ((n n) (r '()))
      (let ((bv (get-bytevector-n in n)))
	(if (= (bytevector-length bv) n)
	    (bytevector-concatenate (reverse! (cons bv r)))
	    (loop (- n (bytevector-length bv)) (cons bv r))))))
  (test-assert "with display" (display msg in/out))
  (test-assert "with format" (format in/out msg))
  ;; response contains \r
  (let ((r (ensure-n in/out (+ (* (string-length msg) 2) 2))))
    ;;(write (utf8->string r)) (newline)
    (test-equal "result" #*"hello\r\nhello\r\n" r))

  (put-bytevector in/out #*"test-end\r\n")
  (close-port in/out)
  ;; socket-port without optional argument closes given socket
  (test-assert "socket-closed? (2)" (socket-closed? client-socket)))

;; now socket-port creates bidirectional port
(let ((port (socket-port (make-client-socket "localhost" server-port)))
      (text "bidirectional port\r\n"))
  (define recv-thread
    (make-thread
     (lambda ()
       (get-bytevector-n port (string-length text)))))
  (define send-thread
    (make-thread
     (lambda ()
       (put-bytevector port (string->utf8 text)))))
  ;; it's not a good test case because it's assuming it works properly.
  (thread-start! recv-thread)
  (thread-sleep! 0.1)
  (thread-start! send-thread)
  (test-equal "bidirectional port" text 
	      (utf8->string (thread-join! recv-thread)))
  (put-bytevector port (string->utf8 "end\r\n"))
  )

(test-assert "wait server ends" (thread-join! server-thread))
(shutdown&close echo-server-socket)

;; addr info slots
(let ((info (get-addrinfo "localhost" server-port
			  (make-hint-addrinfo
			   :family AF_INET
			   :socktype SOCK_DGRAM))))
  (test-assert "sockaddr?" (sockaddr? (addrinfo-sockaddr info)))
  (let ((s (make-socket AF_INET SOCK_DGRAM)))
    (test-equal "seocket-sendto" 4 
		(socket-sendto s #vu8(1 2 3 4) (addrinfo-sockaddr info)))))

;; TODO test for socket-recvfrom and socket-recvfrom!

(test-equal "msg-peek"    MSG_PEEK *msg-peek*)
(test-equal "msg-oob"     MSG_OOB *msg-oob*)
(test-equal "msg-waitall" MSG_WAITALL *msg-waitall*)

;; blocking retry of get-bytevector-n
(define (server-service sock)
  (number->string (socket-info-port (socket-info sock))))

(let ()
  (define server (make-server-socket "0"))
  
  (define t (make-thread
	     (lambda ()
	       (let ((s (socket-accept server)))
		 (socket-send s #vu8(0 1 2 3 4))))))
  (thread-start! t)
  (let ()
    (define client (make-client-socket "localhost" (server-service server)))
    (define in (socket-input-port client))
    (define buf (make-bytevector 10))
    (test-equal "get-bytevector-n shouldn't block" #vu8(0 1 2 3 4)
		(get-bytevector-n in 10))
    (shutdown&close client)
    (shutdown&close server)))

;; thread-interrupt!
;; cancelling blocking socket operation in other threads
(let ()
  (define server (make-server-socket "0"))
  (define t (make-thread
	      (lambda ()
		(socket-read-select #f server))))
  (test-error "not blocked" condition? (thread-interrupt! t))
  (thread-start! t)
  (thread-sleep! 1) ;; wait a bit
  (test-assert "thread-interrupt!" (thread-interrupt! t))
  (test-equal "result" '() (thread-join! t))
  (socket-close server))

;; signal related
;; this test case is only relevant on multi core Linux
(let ()
  (define server (make-server-socket "0"))
  (define interrupted? #f)
  (define started? #f)
  (define t (thread-start!
	     (make-thread
	      (lambda ()
		(set! started? #t)
		(socket-read-select #f server)
		(set! interrupted? #t)))))
  (unless started?
    (thread-yield!)
    (thread-sleep! 1))
  (gc) ;; this uses signal on Linux
  (test-assert "not interrupted" (not interrupted?))
  (test-assert "thread-interrupt!" (thread-interrupt! t))
  (thread-yield!)
  (thread-sleep! 1)
  (test-assert "interrupted" interrupted?)
  (shutdown&close server))

;; ditto
(let ()
  (define server (make-server-socket "0"))
  (define interrupting? #f)
  (define accepted #f)
  (define recieved #f)
  (define (yield!)
    (thread-yield!)
    (thread-sleep! 1))
  (define (invoke-gc)
    (gc) ;; this uses signal on Linux
    (set! interrupting? #t)
    (thread-interrupt! t) ;; interrupted flag on
    (yield!))
  (define t (thread-start!
	     (make-thread
	      (lambda ()
		(let loop ()
		  (let ((client (socket-accept server)))
		    (cond (client
			   (set! accepted #t)
			   (set! recieved (socket-recv client 1))
			   (socket-close client)
			   recieved)
			  (else
			   (if interrupting?
			       (test-assert "socket-accept (interrupt)" #t)
			       (test-assert "socket-accept (interrupt)" #f))
			   (loop)))))))))
  (invoke-gc)
  (test-assert "not accepted" (not accepted))
  (let ((client (make-client-socket "localhost" (server-service server))))
    (yield!)
    (test-assert "accepted" accepted)
    (test-assert "not recieved" (not recieved))
    (socket-send client #vu8(1))
    (test-equal "received" #vu8(1) (thread-join! t))
    (shutdown&close client))
  (shutdown&close server))

;; call #134, socket-select returns incorrect socket
(let ()
  (define server (make-server-socket "0"))
  (define vec (make-vector 5))
  (define server-lock (make-mutex))
  (define client-lock (make-mutex))
  (define t 
    (thread-start!
     (make-thread
      (lambda ()
	(mutex-lock! server-lock)
	(let loop ((i 0))
	  (unless (= i 5)
	    (let ((s (socket-accept server)))
	      (vector-set! vec i s)
	      (loop (+ i 1)))))
	;; FIXME we need something nicer
	(mutex-unlock! server-lock)
	(mutex-lock! client-lock)
	(mutex-unlock! client-lock)

	(apply socket-read-select #f (vector->list vec))))))
  ;; lock it
  (mutex-lock! client-lock)
  (let ((s* (map (lambda (i)
		   (make-client-socket "localhost" (server-service server)))
		 ;; whatever is fine
		 '(1 2 3 4 5))))
    ;; wait until server is done
    (mutex-lock! server-lock)
    (mutex-unlock! server-lock)
    ;; send it (server is waiting for you!)
    (socket-send (car s*) #vu8(1))
    (socket-send (car (last-pair s*)) #vu8(1))
    ;; let server do select
    (mutex-unlock! client-lock)

    (let ((r (thread-join! t)))
      (test-equal "socket-select (size)" 2 (length r))
      (test-equal "socket-select (1)" (vector-ref vec 0) (car r))
      (test-equal "socket-select (2)" (vector-ref vec 4) (cadr r))
      (for-each shutdown&close s*)
      (vector-for-each shutdown&close vec)
      (shutdown&close server))))

;; condition
(test-error "&host-not-found" host-not-found-error?
	    (make-client-socket "localhost" "123456789"))
(guard (e ((host-not-found-error? e)
	   (test-assert "localhost" (host-not-found-error-node e))
	   (test-assert "123456789" (host-not-found-error-service e)))
	  (else (test-assert "unexpected condition" #f)))
  (make-client-socket "localhost" "123456789"))

(let ()
  (define server (make-server-socket "0"))
  (define t (thread-start!
	     (make-thread
	      (lambda ()
		(let ((s (socket-accept server)))
		  (thread-sleep! 1) ;; 1s
		  (socket-send s #*"ok")
		  (thread-sleep! 1) ;; 1s
		  (socket-send s #*"ok2")
		  (socket-shutdown s SHUT_RDWR)
		  (socket-close s))))))
  (define selector (make-socket-selector))
  (define (make-selector-thread)
    (make-thread (lambda () (socket-selector-wait! selector))))
  (define t2 (make-selector-thread))
  (define s (make-client-socket "localhost" (server-service server)))

  (test-assert (socket-selector? (socket-selector-add! selector s)))
  (thread-start! t2)

  ;; This test may cause race condition, and a bit problematic to
  ;; implement a test to avoid, so disable it.
  ;; (test-error (socket-selector-wait! selector))
  (let ((s* (thread-join! t2)))
    (test-equal 1 (length s*))
    (test-equal '(#*"ok") (map (lambda (s) (socket-recv s 2))
			       (map car s*))))
  ;; nothing to wait and specifying timeout
  (test-equal '() (socket-selector-wait! selector 0))

  (socket-selector-add! selector s)
  ;; 1000 => 1ms (timeout = usec)
  (test-equal "selector (timeout)" '() (socket-selector-wait! selector 1000))
  (let-values (((s* t*) (socket-selector-wait! selector)))
    (test-equal '() t*)
    (test-equal 1 (length s*))
    (test-equal '(#*"ok2") (map (lambda (s) (socket-recv s 3))
				(map car s*))))
  (close-socket-selector! selector)
  (socket-close server))

(define (selector-test count :optional (timeout #f))
  (define delay 0.01) ;; 10ms
  (define server (make-server-socket "0"))
  (define selector (make-socket-selector))
  (define timeouts (make-atomic-fixnum 0))
  (define ready-counts (make-atomic-fixnum 0))
  (define client-counts (make-atomic-fixnum 0))
  (define selected (make-atomic-fixnum 0))
  (define (echo-back s)
    (atomic-fixnum-inc! ready-counts)
    (socket-send s (socket-recv s 255)))
  (define end? #f)
  (define t (thread-start!
	     (make-thread
	      (lambda ()
		(let loop ()
		  (let ((s (socket-accept server)))
		    (when (and s (not end?))
		      (atomic-fixnum-inc! client-counts)
		      (socket-selector-add! selector s timeout)
		      (loop))))))))
  (define t2
    (thread-start!
     (make-thread
      (lambda ()
	(guard (e (else #;(print e) #f))
	(let loop ()
	  (unless (socket-selector-closed? selector)
	    (let-values (((socks timed-out) (socket-selector-wait! selector)))
	      (atomic-fixnum-add! selected (+ (length socks) (length timed-out)))
	      (for-each echo-back (map car socks))
	      (for-each (lambda (s)
			  (atomic-fixnum-inc! result-to)
			  (socket-send s #vu8(0))
			  (socket-shutdown s SHUT_RDWR)
			  (socket-close s))
			(map car timed-out))
	      (unless end? (loop))))))))))
  (define result (make-atomic-fixnum 0))
  (define result-to (make-atomic-fixnum 0))
  (define (do-test i)
    (define s (make-client-socket "localhost" (server-service server)))
    (socket-set-read-timeout! s 500) ;; 500ms
    (make-thread
      (lambda ()
	(let ((msg (string->utf8 (string-append "hello " (number->string i)))))
	  (guard (e (else (atomic-fixnum-inc! timeouts) #t))
	    (socket-send s msg)
	    (let ((r (socket-recv s 255)))
	      (when (bytevector=? r msg)
		(atomic-fixnum-inc! result))))
	  (socket-shutdown s SHUT_RDWR)
	  (socket-close s)
	  s))))

  (let ((t* (map do-test (iota count))))
    ;; First wait until all the sockets are created
    (let loop ()
      (unless (= count (atomic-fixnum-load client-counts))
	(thread-sleep! delay)
	(loop)))
    ;; then send 
    (for-each thread-start! t*)
    (test-assert (for-all socket? (map thread-join! t*))))

  (set! end? #t)
  (let ((s (make-client-socket "localhost" (server-service server))))
    (socket-shutdown s SHUT_RDWR)
    (socket-close s))
  ;; this shouldn't be needed...
  (thread-sleep! (* delay 10)) ;; let's wait 10 times more than delay here

  (socket-selector-interrupt! selector)
  (close-socket-selector! selector)

  (thread-join! t)
  (thread-join! t2)

  (socket-shutdown server SHUT_RDWR)
  (socket-close server)

  (unless timeout
    (test-equal (format "timeouts (count = ~a, timeout = ~a): ~a"
			count timeout (atomic-fixnum-load timeouts))
		0 (atomic-fixnum-load timeouts)))
  (values (atomic-fixnum-load ready-counts)
	  (atomic-fixnum-load result) (atomic-fixnum-load result-to)))

;; compute count, we need to think that client and server are running the same here
(define count (or (and (cond ((getenv "FILE_LIMIT") =>
			      (lambda (v)
				(min (div (- (string->number (string-trim-both v)) 50) 2) 500)))
			     (else #f)))
		  500))

(let-values (((c r rt) (selector-test count)))
  (test-equal "no timeout (socket ready)" count c)
  (test-equal "no timeout (received)" count r)
  (test-equal "no timeout (timedout)" 0 rt))

(let-values (((c r rt) (selector-test count (duration:of-nanos 1))))
  ;; for some reason, some sockets don't timeout.
  (test-assert "with timeout (socket ready)" (= r c))
  (test-assert "with timeout (received)" (< r count))
  (test-assert "with timeout (timedout)" (< 0 rt))
  (test-equal "with timeout (total)" count (+ r rt)))

(let ((s* (make-server-socket* "0")))
  (test-assert "make-server-socket*" (list? s*))
  (for-each socket-close s*))

(let ((s* (make-server-socket* "0"))
      (selector (make-socket-selector)))
  (define (accept srv)
    (let ((s (socket-accept srv)))
      (socket-send s (socket-recv s 255))
      (socket-shutdown s SHUT_RDWR)
      (socket-close s)))
  (define thread
    (thread-start!
     (make-thread
      (lambda ()
	(let loop ()
	  (let-values (((s* t*) (socket-selector-wait! selector)))
	    (for-each accept (map car s*))
	    (unless (zero? (socket-selector-size selector))
	      (loop))))))))
  (define (check service ai-family)
    (let ((s (make-client-socket "localhost" service ai-family)))
      (socket-send s (string->utf8 "hello"))
      (test-equal (list service ai-family) "hello"
		  (utf8->string (socket-recv s 255)))))
  (for-each (lambda (s) (socket-selector-add! selector s)) s*)
  (for-each check (map server-service s*) (list AF_INET AF_INET6)))

(test-end)
