#!read-macro=sagittarius/bv-string
(import (rnrs)
	(rfc websocket)
	(rfc websocket connection) ;; for websocket-connection-port
	(rfc websocket messages)
	(sagittarius control)
	(sagittarius socket)
	(net server)
	(binary io)
	(srfi :18)
	(srfi :39 parameters)
	(srfi :64)
	(rfc :5322)
	(rfc base64)
	(math hash)
	(util concurrent shared-queue))

;; for testing, we need this
(define websocket-connection-socket-port-set!
  (with-library (rfc websocket connection)
		websocket-connection-socket-port-set!))

(test-begin "RFC Websocket")

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
			 #vu8(#x82 #x05 #x48 #x65 #x6c #x6c #x6f))))

;; TODO add more tests for low level APIs here


;; high level APIs test
(define (make-test-websocket-server count)
  (define (put-bytevector* out bv . bvs)
    (put-bytevector out bv)
    (for-each (lambda (bv) (put-bytevector out bv)) bvs))

  (define *uuid* #*"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

  (define (calculate-key headers)
    (let ((key (rfc5322-header-ref headers "Sec-WebSocket-Key")))
      (base64-encode
       (hash SHA-1 (bytevector-append (string->utf8 key) *uuid*)))))

  (define (keep-sending out)
    (lambda ()
      (let loop ((i 0))
	(unless (= i count)
	  (put-bytevector out #vu8(#x81 #x05 #x48 #x65 #x6c #x6c #x6f))
	  (flush-output-port out)
	  (loop (+ i 1))))))

  (define (send-close out)
    (put-bytevector out #vu8(#x88 #x00))
    (flush-output-port out)
    (close-port out))
  
  (define (send-pong out data)
    (put-u8 out #x8A)
    (put-u8 out (bytevector-length data))
    (put-bytevector out data)
    (flush-output-port out))
  ;; don't mask for my sake
  (define (send-binary out data)
    (put-u8 out #x82)
    (put-u8 out (bytevector-length data))
    (put-bytevector out data)
    (flush-output-port out))
  
  (define (websocket-handler server socket)
    (define in/out (buffered-port (socket-port socket) (buffer-mode block)))
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
    (thread-start! (make-thread (keep-sending in/out)))
    (let loop ()
      (let-values (((fin? op data) (websocket-recv-frame in/out)))
	(cond ((= op +websocket-close-frame+) (send-close in/out))
	      ((= op +websocket-ping-frame+)
	       (if (bytevector=? #*"invalid" data)
		   (send-pong in/out #*"fail")
		   (send-pong in/out data))
	       (loop))
	      (else
	       (send-binary in/out data)
	       (loop))))))

  (define config (make-server-config :use-ipv6? #t))
  (make-simple-server "9000" websocket-handler :config config))

(define (test-websocket uri count)
  (let ((tsq (make-shared-queue))
	(sq (make-shared-queue))
	(websocket (make-websocket uri))
	(on-open #f)
	(on-close #f))
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

    ;; wait until all text messages are sent
    (do ((i 0 (+ i 1))) ((= i count))
      (test-equal "Hello" (shared-queue-get! tsq)))
    (test-equal #*"binary" (shared-queue-get! sq))
    (test-assert (websocket? (websocket-ping websocket #*"data")))
    (test-error "websocket ping" websocket-pong-error?
		(websocket-ping websocket #*"invalid"))
    
    (test-assert (websocket? (websocket-close websocket)))
    (test-assert on-close)))

(let ()
  (define count 5)
  (define server (make-test-websocket-server count))
  (define uri "ws://localhost:9000")
  (server-start! server :background #t)
  ;; normal test
  (test-websocket uri count)

  ;; protocol
  (let ((websocket (make-websocket uri :protocols '("chat"))))
    (test-assert (websocket? (websocket-open websocket)))
    (test-assert (websocket-close websocket)))
  (let ((websocket (make-websocket uri :protocols '("not-exist"))))
    (test-error websocket-engine-error? (websocket-open websocket)))
  
  (server-stop! server))


(test-end)
