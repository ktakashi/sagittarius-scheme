#!read-macro=sagittarius/bv-string
(import (rnrs)
	(rfc websocket)
	(rfc websocket connection) ;; for websocket-connection-port
	(sagittarius control)
	(srfi :64)
	(srfi :39 parameters))

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

;; TODO add some more tests.

(test-end)
