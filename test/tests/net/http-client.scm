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
	(util logging)
	(security keystore)
	(srfi :64))

(test-begin "HTTP client")

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

(define (bytevector-formatter bv)
  (map (lambda (u8) (string-append "0x" (number->string u8 16)))
       (bytevector->uint-list bv (endianness little) 1)))

(test-error "Invalid format of route-max-connections"
	    (http-pooling-connection-config-builder
	     (route-max-connections '(("httpbin.org" . 10)))))
(test-error "Invalid value of route-max-connections"
	    (http-pooling-connection-config-builder
	     (route-max-connections '(("httpbin.org" a)))))

(define pooling-config
  (http-pooling-connection-config-builder
   (connection-request-timeout 100)
   (time-to-live 3)
   (key-manager (test-key-manager))
   (route-max-connections '(("httpbin.org" 10)))
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


(let ()
  (define (test-future f status)
    (let ((res (future-get f)))
      (test-equal status (http:response-status res))))
  (define (run-test url status)
    (define request (http:request-builder (uri url) (method 'GET)))
    (test-future (http:client-send-async client request) status))
  
  (define client (http:client-builder
		  (cookie-handler (http:make-default-cookie-handler))
		  (connection-manager
		   (make-http-pooling-connection-manager pooling-config))
		  (follow-redirects (http:redirect normal))))

  (test-assert (http:client? client))
  (run-test "https://prod.idrix.eu/secure/" "200")
  ;; The certificate is expired as of 26 Nov 2021...
  (run-test "https://client.badssl.com/" "400")
  )

(let ()
  (define basic-api "https://httpbin.org/basic-auth/foo/bar")
  (define bearer-api "https://httpbin.org/bearer")
  (define (run url auth)
    (define request (http:request-builder (uri url) (auth auth)))
    (http:client-send client request))

  (define (test-status status res)
    (test-equal "Auth" status (http:response-status res)))
  
  (define client (http:client-builder
		  (cookie-handler (http:make-default-cookie-handler))
		  (connection-manager
		   (make-http-pooling-connection-manager pooling-config))
		  (follow-redirects (http:redirect normal))))
  (test-status "200" (run basic-api (http:request-basic-auth "foo" "bar")))
  (test-status "401" (run basic-api (http:request-basic-auth "foo" "baz")))
  (test-status "200" (run bearer-api (http:request-bearer-auth "foo")))

  (http:client-shutdown! client)
  )

(let ()
  (define (test-http-client version)
    (define client (http:client-builder
		    (version version)
		    (follow-redirects (http:redirect never))))
    (define methods '(GET POST PUT DELETE PATCH))
    (define (test-200s client)
      (define (test-200 method)
	(define request (http:request-builder
			 (method method)
			 (uri "https://httpbin.org/status/200")))
	(let ((resp (future-get (http:client-send-async client request))))
	  (test-equal "200" (http:response-status resp))))
      (for-each test-200 methods))

    (http:client-shutdown! client))
  
  (test-http-client (http:version http/1.1))
  (test-http-client (http:version http/2)))
  

(test-end)
