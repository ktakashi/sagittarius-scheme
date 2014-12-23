;; -*- scheme -*-
(add-load-path "./socket")
(add-load-path "./threads")

(import (srfi :64 testing)
	(srfi :13 strings)
	(rnrs)
	(srfi :0 cond-expand)
	(clos user)
	(sagittarius socket)
	;; use thread for testing
	(sagittarius threads))

(define echo-server-socket (make-server-socket "5000"))

;; addr is client socket
(define (server-run)
  (let loop ()
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
			   (lp2 (get-line p))))))))))))))

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
      (test-equal "collect-sockets" l (collect-sockets fdset l))
      (test-assert "sockets->fdset" (fdset? (sockets->fdset l)))))
		
  (test-equal "raw socket-recv"
	      (string->utf8 "hello\r\n")
	      (socket-recv client-socket (+ (string-length "hello") 2) 0))

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
  )

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

;; addr info slots
(let ((info (get-addrinfo "localhost" "5000" (make-hint-addrinfo
					      :family AF_INET
					      :socktype SOCK_DGRAM))))
  (test-assert "sockaddr?" (sockaddr? (addrinfo-sockaddr info)))
  (let ((s (make-socket AF_INET SOCK_DGRAM)))
    (test-equal "seocket-sendto" 4 
		(socket-sendto s #vu8(1 2 3 4) (addrinfo-sockaddr info)))))


;; srfi 106
(import (srfi :106))

(test-equal "msg-peek"    MSG_PEEK *msg-peek*)
(test-equal "msg-oob"     MSG_OOB *msg-oob*)
(test-equal "msg-waitall" MSG_WAITALL *msg-waitall*)

(test-end)
