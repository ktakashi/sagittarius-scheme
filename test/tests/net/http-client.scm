(import (rnrs)
	(srfi :13)
	(rfc base64)
	(rsa pkcs :8)
	(rsa pkcs :12)
	(rfc x509)
	(net http-client)
	(net server)
	(net http-server)
	(net socket)
	(text json)
	(text json pointer)
	(srfi :18)
	(util concurrent)
	(util logging)
	(security keystore)
	(srfi :64))

(test-begin "HTTP client")

(define (idrix-eu p)
  (define node (socket-parameter-socket-node p))
  (cond ((string-suffix? ".certauth.dev" node) "eckey.pem")
	(else #f)))
(define (badssl-com p)
  (define node (socket-parameter-socket-node p))
  (and (string-suffix? ".badssl.com" node) "1"))
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
   (selector-error-handler (lambda args (for-each display args) (newline)))
   #;(delegate-provider
    (make-logging-delegate-connection-provider
     (http-client-logger-builder
      (connection-logger
       (http-connection-logger-builder
	(logger (make-logger +debug-level+ (make-appender "~m")))))
      (loggers
       `((connection-manager
	  ,(make-logger +debug-level+ (make-appender "~m")))))
      (wire-logger
       (http-wire-logger-builder
	(logger (make-logger +debug-level+ (make-appender "~m")))
	(data-formatter bytevector-formatter))))))))

(let ()
  (define (test-future f status body-checks)
    (define (check-body check body)
      (case (car check)
	((json) (let ((json (json-read (open-string-input-port body)))
		      (pointer (json-pointer (cadr check)))
		      (expected (cddr check)))
		  (test-equal expected (pointer json))))))
    (let ((res (future-get f)))
      (test-equal (list status) status (http:response-status res))
      (let ((body (utf8->string (http:response-body res))))
	(for-each (lambda (check) (check-body check body)) body-checks))))
  (define (run-test url status . body-checks)
    (define request (http:request-builder (uri url) (method 'GET)))
    (test-future (http:client-send-async client request) status body-checks))
  
  (define client (http:client-builder
		  (cookie-handler (http:make-default-cookie-handler))
		  (version (http:version http/1.1))
		  (connection-manager
		   (make-http-pooling-connection-manager pooling-config))
		  (follow-redirects (http:redirect normal))))

  (test-assert (http:client? client))
  ;; NOTE certautth.dev TLS certificate is expired
  (cond-expand
   ((not openbsd)
    (run-test "https://mtls.certauth.dev/" "403" '(json "/ssl" . #t)))
   (else
    ;; seems OpenBSD doesn't send if the certificate is expired
    ;; LibreSSL thing?
    (run-test "https://mtls.certauth.dev/" "401" '(json "/ssl" . #f))))
  (run-test "https://client.badssl.com/" "200")
  (http:client-shutdown! client)
  )

#;(let ()
  (define basic-api "https://httpbin.org/basic-auth/foo/bar")
  (define bearer-api "https://httpbin.org/bearer")
  (define (run url auth)
    (define request (http:request-builder (uri url) (auth auth) (timeout 3000)))
    (guard (e ((socket-read-timeout-error? e)
	       (test-expect-fail 1)
	       (test-assert "Read timeout" #f)
	       #f)
	      (else (test-assert (condition-message e) #f)))
      (http:client-send client request)))

  (define (test-status status res)
    (test-equal (string-append "Auth " status)
		status (http:response-status res)))
  
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

#;(let ()
  (define (test-http-client version)
    (define client (http:client-builder
		    (version version)
		    (follow-redirects (http:redirect never))))
    (define client2 (http:client-builder
		     (version version)
		     (follow-redirects (http:redirect normal))))

    (define methods '(GET POST PUT DELETE PATCH))
    (define (run thunk)
      (guard (e ((socket-read-timeout-error? e)
		 ;; ok, ignore as we're using external sevice
		 (test-expect-fail 1)
		 (test-assert "Read timeout" #f))
		(else (test-assert (condition-message e) #f)))
	(thunk)))
    (define (test-200s client)
      (define (test-200 method)
	(define request (http:request-builder
			 (method method)
			 (timeout 3000) ;; 3s
			 (uri "https://httpbin.org/status/200")))
	(run (lambda ()
	       (let ((resp (http:client-send client request)))
		 (test-equal "200" (http:response-status resp))))))
      (print "Testing 200 responses")
      (for-each test-200 methods))

    (define (test-303s client)
      (define (test-303 method)
	(define request (http:request-builder
			 (method method)
			 (timeout 3000) ;; 3s
			 (uri "https://httpbin.org/status/303")))
	(run (lambda ()
	       (let ((resp (http:client-send client request)))
		 (test-equal "303" (http:response-status resp))))))
      (print "Testing 303 responses")
      (for-each test-303 methods))

    (define (test-redirect client)
      (define (test-302 method)
	(define request (http:request-builder
			 (method method)
			 (timeout 3000) ;; 3s
			 (uri "https://httpbin.org/status/302")))
	(run (lambda ()
	       (let ((resp (http:client-send client request)))
		 (test-equal "200" (http:response-status resp))))))
      (print "Testing redirect responses")
      (for-each test-302 methods))
    
    (print "HTTP client for " version)
    (test-200s client)
    (test-303s client)
    (test-redirect client2)
    (print "Done!")

    (http:client-shutdown! client)
    (http:client-shutdown! client2))
  
  (test-http-client (http:version http/1.1))
  (test-http-client (http:version http/2)))

(let ()
  (define client (http:client-builder (follow-redirects (http:redirect always))))
  (define request (http:request-builder (uri "https://google.com")))
  (test-assert "wrong constructer arguments on HTTP2"
	       (http:client-send client request)))

(let ()
  (define (start-http-client-test-server)
    (define (app req res)
      (http-server:response-status-set! res 200)
      (http-server:response-header-set! res "Content-Type" "text/plain")
      (http-server:response-text! res "operation-body")
      res)
    (define server (make-http-server "0" app))
    (server-start! server :background #t)
    (thread-sleep! 0.2)
    (do ()
        ((server-running? server))
      (thread-sleep! 0.05))
    server)

  (define (make-url server)
    (define port (server-port server))
    (string-append "http://localhost:"
                   (if (string? port) port (number->string port))
                   "/operation"))

  (define make-base-response-context
    (record-constructor
     (make-record-constructor-descriptor
      (record-type-descriptor <http:response-context>)
      #f
      #f)))

  (define make-http-response
    (record-constructor
     (make-record-constructor-descriptor
      (record-type-descriptor <http:response>)
      #f
      #f)))

  (define server (start-http-client-test-server))
  (define client (http:client-builder
                  (version (http:version http/1.1))
                  (follow-redirects (http:redirect never))))
  (define request (http:request-builder (method 'GET) (uri (make-url server))))

  (let-values (((f success failure) (make-piped-future)))
    (define saw-headers? #f)
    (define saw-data? #f)
    (define response-status #f)
    (define response-headers '())
    (define response-body-parts '())
    (define (on-init request header-handler data-handler)
      (make-base-response-context request header-handler data-handler))
    (define on-finalize 
      (lambda (ctx)
        (define headers (http:make-headers))
        (for-each (lambda (kv)
                    (hashtable-set! headers (car kv) (cdr kv)))
                  response-headers)
        (make-http-response
         response-status
         headers
         '()
         (if (null? response-body-parts)
             #vu8()
             (apply bytevector-append (reverse response-body-parts)))
         #f)))
    (define operation
      (http:client-start client request
        :on-init on-init
	:on-finalize on-finalize
        :on-headers (lambda (op ctx status headers has-data?)
                      (set! saw-headers? #t)
                      (set! response-status status)
                      (set! response-headers headers))
        :on-data (lambda (op ctx data end?)
                   (set! saw-data? #t)
                   (set! response-body-parts (cons data response-body-parts)))
        :on-complete (lambda (op response)
                       (success response))
        :on-error (lambda (op e)
                    (failure e))))
    (test-assert "http:client-start returns operation"
                 (http:operation? operation))
    (let ((response (future-get f 5 #f)))
      (test-assert "http:client-start completes response"
                   (http:response? response))
      (test-equal "http:client-start status" "200" (http:response-status response))
      (test-equal "http:client-start final state"
                  'completed
                  (http:operation-state operation))
      (test-assert "http:client-start on-headers callback"
                   saw-headers?)
      (test-assert "http:client-start on-data callback"
                   saw-data?)))

  (let-values (((f success failure) (make-piped-future)))
    (define response-status #f)
    (define response-headers '())
    (define response-body-parts '())
    (define (on-init request header-handler data-handler)
      (make-base-response-context request header-handler data-handler))
    (define on-finalize
      (lambda (ctx)
        (define headers (http:make-headers))
        (for-each (lambda (kv)
                    (hashtable-set! headers (car kv) (cdr kv)))
                  response-headers)
        (make-http-response
         response-status
         headers
         '()
         (if (null? response-body-parts)
             #vu8()
             (apply bytevector-append (reverse response-body-parts)))
         #f)))
    (define operation
      (http:client-start client request
        :on-init on-init
	:on-finalize on-finalize
        :on-headers (lambda (op ctx status headers has-data?)
                      (set! response-status status)
                      (set! response-headers headers))
        :on-data (lambda (op ctx data end?)
                   (set! response-body-parts (cons data response-body-parts)))
        :on-complete (lambda (op response)
                       (success response))
        :on-error (lambda (op e)
                    (failure e))))
    (let ((response (future-get f 5 #f)))
      (test-assert "http:client-start custom handler response"
                   (http:response? response))
      (test-equal "http:client-start custom handler body"
		  (string->utf8 "operation-body")
                  (http:response-body response))
      (test-equal "http:client-start custom handler state"
                  'completed
                  (http:operation-state operation))))

  (http:client-shutdown! client)
  (server-stop! server))

(test-end)
