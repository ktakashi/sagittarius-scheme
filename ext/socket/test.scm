;; -*- scheme -*-
(add-load-path "./socket")
(add-load-path "./threads")

#!read-macro=sagittarius/bv-string
(import (srfi :64 testing)
	(srfi :13 strings)
	(rnrs)
	(srfi :0 cond-expand)
	(clos user)
	(sagittarius socket)
	(sagittarius) ;; for format
	;; use thread for testing
	(sagittarius threads))

(define (shutdown&close s)
  (socket-shutdown s SHUT_RDWR)
  (socket-close s))

(define echo-server-socket (make-server-socket "5000"))

;; addr is client socket
(define (server-run)
  (let loop ()
    (guard (e (else ;; shouldn't happen but happens so put like this
	       (loop)))
      (let ((addr (socket-accept echo-server-socket)))
	(call-with-socket addr
	  (lambda (sock)
	    (let ((p (transcoded-port (socket-port sock) (native-transcoder))))
	      (call-with-port p
		(lambda (p)
		  (let lp2 ((r (get-line p)))
		    (cond ((or (not (string? r)) (string=? r "end")))
			  ((or (not (string? r)) (string=? r "test-end"))
			   (put-string p "") (loop))
			  (else
			   (let ((res (string->utf8 (string-append r "\r\n"))))
			     ;; wait one sec
			     (when (string=? r "wait") (thread-sleep! 1))
			     (put-bytevector p res 0 (bytevector-length res) #t)
			     (lp2 (get-line p)))))))))))))))
(define server-thread (make-thread server-run))

(test-begin "(run-socket-test)")
;; start echo server
(thread-start! server-thread)

(test-error "ai-passive" assertion-violation?
	    (make-client-socket #f "5000" 0 0 AI_PASSIVE))

(let ((client-socket (make-client-socket "localhost" "5000")))
  (test-assert "socket?"(socket? client-socket))
  (test-equal "raw socket-send"
	      (+ (string-length "hello") 2) ;; for \r\n
	      (socket-send client-socket (string->utf8 "hello\r\n") 0))

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
    
    (test-assert "fdset-ref  (3)" (fdset-ref fdset client-socket))
    (let ((l (list client-socket)))
      (test-equal "collect-sockets" l (collect-sockets fdset))
      (test-assert "sockets->fdset" (fdset? (sockets->fdset l)))))

  (test-equal "raw socket-recv"
	      (string->utf8 "hello\r\n")
	      (socket-recv client-socket (+ (string-length "hello") 2) 0))
  
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

(let ((client-socket (make-client-socket "localhost" "5000")))
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
(let* ((client-socket (make-client-socket "localhost" "5000"))
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
(let ((port (socket-port (make-client-socket "localhost" "5000")))
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
(let ((info (get-addrinfo "localhost" "5000" (make-hint-addrinfo
					      :family AF_INET
					      :socktype SOCK_DGRAM))))
  (test-assert "sockaddr?" (sockaddr? (addrinfo-sockaddr info)))
  (let ((s (make-socket AF_INET SOCK_DGRAM)))
    (test-equal "seocket-sendto" 4 
		(socket-sendto s #vu8(1 2 3 4) (addrinfo-sockaddr info)))))

;; TODO test for socket-recvfrom and socket-recvfrom!

;; srfi 106
(import (srfi :106))

(test-equal "msg-peek"    MSG_PEEK *msg-peek*)
(test-equal "msg-oob"     MSG_OOB *msg-oob*)
(test-equal "msg-waitall" MSG_WAITALL *msg-waitall*)

;; blocking retry of get-bytevector-n
(let ()
  (define server (make-server-socket "5001"))
  
  (define t (make-thread
	     (lambda ()
	       (let ((s (socket-accept server)))
		 (socket-send s #vu8(0 1 2 3 4))))))
  (thread-start! t)
  (let ()
    (define client (make-client-socket "localhost" "5001"))
    (define in (socket-input-port client))
    (define buf (make-bytevector 10))
    (test-equal "get-bytevector-n shouldn't block" #vu8(0 1 2 3 4)
		(get-bytevector-n in 10))
    (shutdown&close client)
    (shutdown&close server)))

;; thread-interrupt!
;; cancelling blocking socket operation in other threads
(let ()
  (define server (make-server-socket "5001"))
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
  (define server (make-server-socket "5001"))
  (define interrupted? #f)
  (define t (thread-start!
	     (make-thread
	      (lambda ()
		(socket-read-select #f server)
		(set! interrupted? #t)))))
  (gc) ;; this uses signal on Linux
  (thread-yield!)
  (thread-sleep! 1)
  (test-assert "not interrupted" (not interrupted?))
  (test-assert "thread-interrupt!" (thread-interrupt! t))
  (thread-yield!)
  (thread-sleep! 1)
  (test-assert "interrupted" interrupted?)
  (shutdown&close server))

;; ditto
(let ()
  (define server (make-server-socket "5001"))
  (define interrupted? #f)
  (define accepted #f)
  (define recieved #f)
  (define (yield!)
    (thread-yield!)
    (thread-sleep! 1))
  (define (invoke-gc)
    (gc) ;; this uses signal on Linux
    (thread-interrupt! t) ;; interrupted flag on
    (yield!))
  (define t (thread-start!
	     (make-thread
	      (lambda ()
		(let ((client (socket-accept server)))
		  (set! accepted #t)
		  (set! recieved (socket-recv client 1))
		  (socket-close client)
		  recieved)))))
  (invoke-gc)
  (test-assert "not accepted" (not accepted))
  (let ((client (make-client-socket "localhost" "5001")))
    (yield!)
    (test-assert "accepted" accepted)
    (test-assert "not recieved" (not recieved))
    (socket-send client #vu8(1))
    (test-equal "received" #vu8(1) (thread-join! t))
    (shutdown&close client))
  (shutdown&close server))

;; call #134, socket-select returns incorrect socket
(let ()
  (define server (make-server-socket "5001"))
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
  (let ((s* (map (lambda (i) (make-client-socket "localhost" "5001"))
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

(test-end)
