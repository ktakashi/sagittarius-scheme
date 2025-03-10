#!read-macro=sagittarius/bv-string
(import (rnrs)
	(rfc websocket)
	(rfc websocket connection) ;; for websocket-connection-port
	(rfc websocket messages)
	(sagittarius control)
	(sagittarius socket)
	(sagittarius io)
	(clos user)
	(net server)
	(binary io)
	(srfi :18)
	(srfi :39 parameters)
	(srfi :64)
	(rfc :5322)
	(rfc base64)
	(sagittarius crypto digests)
	(util concurrent shared-queue))

;; for testing, we need this
(define websocket-connection-socket-port-set!
  (with-library (rfc websocket connection)
		websocket-connection-port-set!))

(test-begin "RFC Websocket")

(define-class <one-shot-binary-port>
  (<custom-binary-input-port> <read-once-port>)
  ())

(define (make-lazy-binary-input-port data)
  (define pos 0)
  (define len (bytevector-length data))
  (define (read! bv start count)
    (cond ((= pos len) 0)
	  ((zero? count) 0)
	  (else
	   (bytevector-u8-set! bv start (bytevector-u8-ref data pos))
	   (set! pos (+ pos 1))
	   1)))
  (define (close))
  (make <one-shot-binary-port> :read read!))

(test-equal "websocket-recv-frame"
	    '(#t 1 #*"Hello")
	    (let ((in (make-lazy-binary-input-port #vu8(#x81 #x05 #x48 #x65 #x6c #x6c #x6f))))
	      (let-values ((r (websocket-recv-frame in)))
		r)))

(define (test-message conn populate expect)
  (websocket-connection-socket-port-set! conn (open-output-bytevector))
  (populate conn)
  (test-equal "message test" expect
	      (get-output-bytevector (websocket-connection-port conn))))

(define (test-binary-message conn binary expect)
  (test-message conn (lambda (conn) (websocket-send-binary conn binary))
		expect))
(define (test-text-message conn binary expect)
  (test-message conn (lambda (conn) (websocket-send-text conn binary)) expect))

;; it won't connect until handshake (depending on the engine of course
;; but default is http)
(let ((conn (make-websocket-connection "wss://echo.websocket.org")))
  (test-assert (websocket-connection? conn))
  (parameterize ((*websocket-mask-data?* #f))
    (test-text-message conn "Hello" #vu8(#x81 #x05 #x48 #x65 #x6c #x6c #x6f))
    (test-message conn (lambda (conn) (websocket-send-text conn "Hello" 0 3))
		  #vu8(#x01 #x03 #x48 #x65 #x6c #x80 #x02 #x6c #x6f))

    (test-binary-message conn #*"Hello"
			 #vu8(#x82 #x05 #x48 #x65 #x6c #x6c #x6f))
    (let ((bv126 (make-bytevector 126 1))
	  (bvFFFF (make-bytevector #xFFFF 2)))
      (test-binary-message conn bv126
			   (bytevector-append #vu8(#x82 #x7E #x00 #x7E) bv126))
      (test-binary-message conn bvFFFF
			   (bytevector-append
			    #vu8(#x82 #x7F #x00 #x00 #x00 #x00 #x00 #x00 #xFF #xFF)
			    bvFFFF)))))

;; TODO add more tests for low level APIs here
(test-error "Non supported handshake engine"
	    websocket-engine-not-found-error?
	    (make-websocket-connection "ws://localhost" 'not-found))
(test-error "Invalid scheme"
	    websocket-scheme-error?
	    (make-websocket-connection "http://localhost"))
(test-error "Invalid scheme (2)"
	    websocket-scheme-error?
	    (make-websocket-connection "http://localhost:8080"))
(test-error "Connection failed"
	    websocket-connection-error?
	    (websocket-connection-handshake!
	     (make-websocket-connection "ws://this.should.not.exist")))

;; high level APIs test
;; condition tests
(test-error "Non supported handshake engine (high)"
	    websocket-engine-not-found-error?
	    (make-websocket "ws://localhost" :engine 'not-found))
(test-error "Invalid scheme (high)"
	    websocket-scheme-error?
	    (make-websocket "http://localhost"))
(test-error "Connection failed (high)"
	    websocket-connection-error?
	    (websocket-open
	     (websocket-on-error
	      (make-websocket "ws://this.should.not.exist")
	      (lambda (ws e)
		(test-assert "&websocket-connection"
			     (websocket-connection-error? e))))))

(test-assert "reconnectable?" (websocket-reconnectable?
			       (make-websocket "ws://localhost")))

(define (make-test-websocket-server count)
  (define (put-bytevector* out bv . bvs)
    (put-bytevector out bv)
    (for-each (lambda (bv) (put-bytevector out bv)) bvs))

  (define *uuid* #*"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

  (define (calculate-key headers)
    (let ((key (rfc5322-header-ref headers "Sec-WebSocket-Key"))
	  (md (make-message-digest *digest:sha-1*)))
      (base64-encode
       (digest-message md (bytevector-append (string->utf8 key) *uuid*)))))

  (define (keep-sending out)
    (lambda ()
      (let loop ((i 0))
	(unless (= i count)
	  (websocket-send-frame! out +websocket-text-frame+ #f #*"Hello" #t)
	  (loop (+ i 1))))))

  (define (send-close out)
    (websocket-send-frame! out +websocket-close-frame+ #t #vu8() #t))
  
  (define (send-pong out data)
    (websocket-send-frame! out +websocket-pong-frame+ #f data #t))
  ;; don't mask for my sake
  (define (send-binary out data op fin?)
    (websocket-send-frame! out op #t data fin?))

  (define (send-ping out data)
    (websocket-send-frame! out +websocket-ping-frame+ #f data #t))
  
  (define (websocket-handler server socket)
    ;; for some reason closing this wouldn't remove
    ;; from buffered port lists. i don't know why.
    ;;(define in/out (buffered-port (socket-port socket #f) (buffer-mode block)))
    (define in/out (socket-port socket #f))
    (unwind-protect
     (let ((thread (make-thread (keep-sending in/out))))
       (get-line in/out) ;; discard
       (let* ((headers (rfc5322-read-headers in/out))
	      (key (calculate-key headers)))
	 (put-bytevector* in/out #*"HTTP/1.1 101 Switch protocol\r\n")
	 (put-bytevector* in/out #*"Upgrade: websocket\r\n")
	 (put-bytevector* in/out #*"Connection: Upgrade\r\n")
	 (put-bytevector* in/out #*"Sec-WebSocket-Accept: " key #*"\r\n")
	 (when (rfc5322-header-ref headers "Sec-WebSocket-Protocol")
	   (put-bytevector* in/out #*"Sec-WebSocket-Protocol: chat\r\n"))
	 (put-bytevector* in/out #*"\r\n")
	 (flush-output-port in/out))
       (thread-start! thread)
       (let loop ()
	 (let-values (((fin? op data) (websocket-recv-frame in/out)))
	   (cond ((= op +websocket-close-frame+) (send-close in/out))
		 ((= op +websocket-ping-frame+)
		  (if (bytevector=? #*"invalid" data)
		      (send-pong in/out #*"fail")
		      (send-pong in/out data))
		  (loop))
		 (else
		  (send-binary in/out data op fin?)
		  (unless fin?
		    (let lp ()
		      (let-values (((fin? op data)
				    (websocket-recv-frame in/out)))
			(send-binary in/out data op fin?)
			(unless fin? (lp)))))
		  ;; if it's a single thread or very slow multi threads
		  ;; environment, the background thread may not finish
		  ;; by the expected time. the purpose of the background
		  ;; thread is that client can handle sending and receiving
		  ;; simultaneously so if one frame is received then
		  ;; we can wait (I guess)
		  (thread-join! thread)
		  (loop))))))
     (close-port in/out)))
  (define config (make-server-config :use-ipv6? #t))
  (make-simple-server "0" websocket-handler :config config))

(define (test-websocket uri count)
  (let ((tsq (make-shared-queue))
	(sq (make-shared-queue))
	(websocket (make-websocket uri))
	(on-open #f)
	(on-close #f)
	(bv126 (make-bytevector 126 1))
	(bvFFFF (make-bytevector #xFFFF 2))
	)
    (test-assert (websocket? websocket))
    (test-assert (websocket? (websocket-on-text-message websocket
			      (lambda (ws text) 
				(shared-queue-put! tsq text)))))
    (test-assert (websocket? (websocket-on-binary-message websocket
			      (lambda (ws bin) (shared-queue-put! sq bin)))))
    (test-assert (websocket? (websocket-on-open websocket
			      (lambda (ws) (set! on-open #t)))))
    (test-assert (websocket? (websocket-on-close websocket
			      (lambda (ws) (set! on-close #t)))))
    (test-assert (websocket? (websocket-on-error websocket
			      (lambda (ws e)
				(test-assert (websocket-error? e))
				(raise e)))))
    ;; not yet opened
    (test-assert (not on-open))
    (test-assert (not on-close))
    (test-assert (websocket? (websocket-open websocket)))
    (test-assert on-open)
    (test-assert (websocket? (websocket-send websocket #*"binary")))
    (test-assert (websocket? (websocket-send websocket bv126)))
    ;; CI fails...
    ;;(test-assert (websocket? (websocket-send websocket bvFFFF)))
    ;; using splitter
    (test-assert (websocket? (websocket-send websocket bvFFFF 0 1024)))

    (do ((i 0 (+ i 1))) ((= i count))
      (thread-yield!)
      (test-equal (format "Hello (~a)" i) "Hello" (shared-queue-get! tsq 1)))
    (test-equal #*"binary" (shared-queue-get! sq 1))
    (test-equal bv126 (shared-queue-get! sq 1))
    #;(test-equal (bytevector-length bvFFFF)
		(bytevector-length (shared-queue-get! sq 1)))
    (test-equal (bytevector-length bvFFFF)
		(bytevector-length (shared-queue-get! sq 1)))
    (test-assert (websocket? (websocket-ping websocket #*"data")))
    (test-error "websocket ping" websocket-pong-error?
		(websocket-ping websocket #*"invalid"))

    (test-assert (websocket? (websocket-close websocket :timeout 1)))
    ;; double close doesn't effect anything
    (test-assert (websocket? (websocket-close websocket :timeout 1)))
    (test-assert on-close)))

(let ()
  (define count 5)
  (define server (make-test-websocket-server count))
  (server-start! server :background #t)
  (thread-sleep! 0.1)

  (let ((uri (format "ws://localhost:~a" (server-port server))))
    ;; normal test
    (test-websocket uri count)
    
    ;; protocol
    (let ((websocket (make-websocket uri :protocols '("chat"))))
      (test-assert (websocket? (websocket-open websocket)))
      (test-assert (websocket-close websocket :timeout 1))
      ;; re-open
      (test-assert (websocket? (websocket-open websocket)))
      (test-assert (websocket-close websocket :timeout 1))
      )
    (let ((websocket (make-websocket uri :protocols '("not-exist"))))
      (test-error websocket-engine-error? (websocket-open websocket)))
    
    (server-stop! server)))

;; TODO server socket conversion tests.

(test-end)
