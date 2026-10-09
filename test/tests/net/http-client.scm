(import (rnrs)
	(srfi :13)
	(srfi :18)
	(srfi :19)
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
	(util concurrent)
	(util logging)
	(sagittarius crypto keys)
	(security keystore)
	(srfi :64))

(test-begin "HTTP client")

(let ()
  (define kp1 (generate-key-pair *key:ecdsa*))
  (define kp2 (generate-key-pair *key:ecdsa*))
  (define one-year (make-time time-duration 0 (* 3600 24 365)))
  (define now (add-duration (current-time) (make-time time-duration 0 -300)))
  (define cert1 (make-x509-basic-certificate kp1 1
		  (make-x509-issuer '((CN . "sagittarius-client-1")))
		  (make-validity (time-utc->date now)
				 (time-utc->date (add-duration now one-year)))
		  (make-x509-issuer '((CN . "sagittarius-client-1")))))
  (define cert2 (make-x509-basic-certificate kp2 2
		  (make-x509-issuer '((CN . "sagittarius-client-2")))
		  (make-validity (time-utc->date now)
				 (time-utc->date (add-duration now one-year)))
		  (make-x509-issuer '((CN . "sagittarius-client-2")))))
  (define keystore
    (let ((ks (make-keystore 'pkcs12)))
      (keystore-set-key! ks "client-1"
			 (key-pair-private kp1) "pass1" (list cert1))
      (keystore-set-key! ks "client-2"
			 (key-pair-private kp2) "pass2" (list cert2))
      ks))
  (define (client-1 parameter) "client-1")
  (define (client-2 parameter) "client-2")
  (define strategy
    `(("pass1" ,client-1)
      ("pass2" ,client-2)))
  (define tls-config (make-server-tls-config 
		      :trusted-certificates (list cert1 cert2)
		      :certificate-verifier #t))
  (define config (make-http-server-config :secure? #t :tls-config tls-config))
  
  (define (app req res)
    (http-server:response-header-set! res "Content-Type" "application/json")
    (cond ((http-server:request-peer-certificate req) =>
	   (lambda (cert)
	     (http-server:response-status-set! res 200)
	     (http-server:response-bytes! res
	      (x509-certificate->bytevector cert))))
	  (else (http-server:response-status-set! res 400)))
    res)
  (define server (make-http-server "0" app :config config))

  (define (test-key-manager)
    (define ((->keystore-key-provider ks) keystore-info)
      (let ((keypass (car keystore-info))
	    (strategy (cadr keystore-info)))
	(make-keystore-key-provider ks keypass strategy)))
    (make-key-manager (map (->keystore-key-provider keystore) strategy)))

  (define (bytevector-formatter bv)
    (map (lambda (u8) (string-append "0x" (number->string u8 16)))
	 (bytevector->uint-list bv (endianness little) 1)))

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

  (server-start! server :background #t)
  (test-error "Invalid format of route-max-connections"
	      (http-pooling-connection-config-builder
	       (route-max-connections '(("httpbin.org" . 10)))))
  (test-error "Invalid value of route-max-connections"
	      (http-pooling-connection-config-builder
	       (route-max-connections '(("httpbin.org" a)))))

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
    (define uri (format "https://localhost:~a/" (server-port server)))
    (test-assert (http:client? client))
    (run-test uri "200")
    (http:client-shutdown! client))

  (let ()
    (define uri (format "https://localhost:~a/" (server-port server)))
    (define client (http:client-builder
		    (follow-redirects (http:redirect always))))
    (define request (http:request-builder (uri uri)))
    (test-assert "wrong constructer arguments on HTTP2"
		 (http:client-send client request))
    (http:client-shutdown! client))

  (server-stop! server)
)

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
        :on-init #f
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
        :on-init #f
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
