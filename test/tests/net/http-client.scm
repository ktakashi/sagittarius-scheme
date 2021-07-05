(import (rnrs)
	(srfi :13)
	(rfc base64)
	(rfc pem)
	(rsa pkcs :8)
	(rsa pkcs :12)
	(rfc x.509)
	(crypto)
	(net http-client)
	(util concurrent)
	;; (util logging)
	(security keystore)
	(srfi :64))

(test-begin "HTTP client")

#;(define (bytevector-formatter bv)
  (map (lambda (u8) (string-append "0x" (number->string u8 16)))
       (bytevector->uint-list bv (endianness little) 1)))
(define pooling-config
  (http-connection-pooling-config-builder
   (connection-request-timeout 100)
   (time-to-live 3)
   #;(delegate-provider
    (make-logging-delegate-connection-provider
     (http-client-logger-builder
      (connection-logger
       (http-connection-logger-builder
	(logger (make-logger +debug-level+ (make-appender "~m ~a[0]")))))
      (wire-logger
       (http-wire-logger-builder
	(logger (make-logger +debug-level+ (make-appender "~m")))
	(data-formatter bytevector-formatter))))))))

(define (idrix-eu p)
  (define node (socket-parameter-socket-node p))
  (cond ((string=? node "prod.idrix.eu") "eckey.pem")
	(else #f)))
(define (badssl-com p)
  (define node (socket-parameter-socket-node p))
  (and (string-suffix? ".badssl.com" node)
       "1"))
(define keystores
  ;; keystore file,  store pass, key pass, alias selector
  `(("test/data/keystores/keystore0.b64" "password" "password" ,idrix-eu)
    ("test/data/keystores/badssl-client.b64" "badssl.com" "badssl.com" ,badssl-com)))
    

(define (test-key-manager)
  (define (->keystore-key-provider keystore-info)
    (let ((file (car keystore-info))
	  (storepass (cadr keystore-info))
	  (keypass (caddr keystore-info))
	  (strategy (cadddr keystore-info)))
      (make-keystore-key-provider
       (call-with-input-file file
	 (lambda (in)
	   (let ((bin (open-base64-decode-input-port in)))
	     (load-keystore 'pkcs12 bin storepass)))
	 :transcoder #f)
       keypass
       strategy)))
  (make-key-manager (map ->keystore-key-provider keystores)))

(let ()
  (define (test-future f)
    (let ((res (future-get f)))
      (test-equal "200" (http:response-status res))))
  (define (run-test url)
    (define request (http:request-builder (uri url) (method 'GET)))
    (test-future (http:client-send-async client request)))
  
  (define client (http:client-builder
		  (cookie-handler (http:make-default-cookie-handler))
		  (key-manager (test-key-manager))
		  (connection-manager
		   (build-http-pooling-connection-manager pooling-config))
		  (follow-redirects (http:redirect normal))))

  (test-assert (http:client? client))
  (run-test "https://prod.idrix.eu/secure/")
  (run-test "https://client.badssl.com/")
  )

(test-end)
