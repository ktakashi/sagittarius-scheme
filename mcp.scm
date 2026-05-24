(import (rnrs)
	(net http-client)
	(text json)
	(rfc uuid)
	(text json object-builder)
	(util logging)
	(util port)
	(record builder))

(define-record-type mcp-context
  (fields url
	  (mutable session-id))
  (protocol (lambda (p)
	      (lambda (url)
		(p url #f)))))


(define (json->string j)
  (call-with-string-output-port (lambda (out) (json-write j out))))

(define-record-type json-rpc-request
  (fields jsonrpc method params id)
  (protocol
   (lambda (p)
     (lambda (method :key (params '()) (id (uuid->string (make-v4-uuid))))
       (p "2.0" method params id)))))
(define-record-type json-rpc-response
  (fields jsonrpc id)
  (protocol (lambda (p) (lambda (id) (p "2.0" id)))))
(define-record-type json-rpc-success-response
  (parent json-rpc-response)
  (fields result)
  (protocol (lambda (n) (lambda (id result) ((n result) id)))))
(define-record-type json-rpc-error-response
  (parent json-rpc-response)
  (fields error)
  (protocol (lambda (n) (lambda (id error) ((n error) id)))))

(define json-rpc-request-serializer
  (json-object-serializer
   (("jsonrpc" json-rpc-request-jsonrpc)
    ("method" json-rpc-request-method)
    (? "id" #f json-rpc-request-id)
    (? "params" #f json-rpc-request-params))))
    

(define (make-jsonrpc-request-message method params)
  (object->json-string (make-json-rpc-request method :params params)
		       json-rpc-request-serializer))

(define-record-type mpc-request
  (fields method))

(define msg
  `#(("jsonrpc" . "2.0")
     ("id" . 1)
     ("method" . "initialize")
     ("params" .
      #(("protocolVersion" . "2025-06-18")
	("capabilities" .
	 #(("roots" . #(("listChanged" . #t)))))
	("clientInfo" .
	 #(("name" . "sagittarius-mcp-client")
	   ("title" . "Sagittarius MCP Client")
	   ("version" . "0.0.1")))))))

(define url "https://time.mcp.inevitable.fyi/mcp")
;;(define url "https://echo.mcp.inevitable.fyi/mcp")

(define (make-request context :optional (init? #f))
  (http:request-builder
   (uri (mcp-context-url context))
   (method 'POST)
   (content-type "application/json")
   (headers `(("Accept" . ,(if init?
			       "application/json,text/event-stream"
			       "application/json"))
	      ,@(cond ((mcp-context-session-id context) =>
		       (lambda (id)
			 `(("MCP-Session-ID" . ,id))))
		      (else '()))))
   (body (string->utf8 (json->string msg)))))

(define (bytevector-formatter bv)
  (map (lambda (u8) (string-append "0x" (number->string u8 16)))
       (bytevector->uint-list bv (endianness little) 1)))

(define http-client
  (http:client-builder
   ;; (version (http:version http/1.1))
   #;(connection-manager
    (make-http-logging-connection-manager
     (http-connection-config-builder)
     (http-client-logger-builder
      (connection-logger
       (http-connection-logger-builder
	(logger (make-logger +debug-level+ (make-appender "~m ~a[0]")))))
      #;(wire-logger
       (http-wire-logger-builder
	(logger (make-logger +debug-level+ (make-appender "~m")))
	(data-formatter bytevector-formatter))))))))

(define context (make-mcp-context url))

(define (stdout-data-handler)
  (values (standard-error-port) (lambda (#:_ #:_) #vu8())))

(let ((r (http:client-send http-client (make-request context #t))))
  (mcp-context-session-id-set! context
    (http:headers-ref (http:response-headers r) "mcp-session-id"))
  
  (print (http:headers->alist (http:response-headers r)))
  (print (http:stream-response? r))
  (port-for-each (lambda (v) (print v))
		 (let ((in (transcoded-port (http:response-body r)
					    (native-transcoder))))
		   (lambda () (get-line in))))
  #;(http:stream-response-close! r))
