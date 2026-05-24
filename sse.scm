(import (rnrs)
	(net http-client)
	(util logging)
	(util port))

(define (bytevector-formatter bv)
  (map (lambda (u8) (string-append "0x" (number->string u8 16)))
       (bytevector->uint-list bv (endianness little) 1)))

(define http-client 
  (http:client-builder
   (connection-manager
    (make-http-logging-connection-manager
     (http-connection-config-builder)
     (http-client-logger-builder
      (connection-logger
       (http-connection-logger-builder
	(logger (make-logger +debug-level+ (make-appender "~m")))))
      #;(wire-logger
       (http-wire-logger-builder
	(logger (make-logger +debug-level+ (make-appender "~m")))
	(data-formatter bytevector-formatter))))))))

(define request
  (http:request-builder
   (uri "https://sse.dev/test")
   ;;(uri "https://www.ing.com")
   (method 'GET)))

(let ((r (http:client-send http-client request)))
  (print (http:headers->alist (http:response-headers r)))
  (port-for-each (lambda (v) (print v))
		 (let ((in (transcoded-port (http:response-body r)
					    (native-transcoder))))
		   (lambda () (get-line in)))))

