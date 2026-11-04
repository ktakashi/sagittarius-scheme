#!read-macro=sagittarius/bv-string
(import (rnrs)
	(net http-client)
	(net http-server)
	(net server)
	(srfi :18)
	(srfi :19)
	(srfi :64)
	(rfc x509)
	(sagittarius crypto keys))

(test-begin "net/http-compliance")

(define (response-header response name)
  (http:headers-ref (http:response-headers response) name))

(define (response-body-string response)
  (let ((body (http:response-body response)))
    (if (bytevector? body)
        (utf8->string body)
        "")))

(define (assert-empty-body label response)
  (test-equal label #vu8() (http:response-body response)))

(define (assert-header-absent label response name)
  (test-eqv label #f (response-header response name)))

(define (assert-header-zero-or-absent label response name)
  (let ((v (response-header response name)))
    (test-assert label (or (not v) (string=? v "0")))))

(define (make-httpbin-router)
  (define router (make-http-server:router))

  (define (status->exact-integer req)
    (let ((v (http-server:request-attribute-ref req 'code #f)))
      (and v (string->number v))))

  (define (status-handler req res)
    (let ((code (status->exact-integer req)))
      (cond ((not (and code (integer? code)))
             (http-server:response-status-set! res 400)
             (http-server:response-text! res "bad status code")
             res)
            ((= code 204)
             (http-server:response-status-set! res 204)
             res)
            ((= code 205)
             (http-server:response-status-set! res 205)
             (http-server:response-bytes! res #*"reset-body")
             res)
            ((= code 304)
             (http-server:response-status-set! res 304)
             (http-server:response-header-set! res "content-length" "7")
             (http-server:response-bytes! res #*"ignored")
             res)
            ((= code 103)
             (http-server:response-status-set! res 103)
             (http-server:response-text! res "early hints")
             res)
            ((= code 407)
             (http-server:response-status-set! res 407)
             (http-server:response-header-set! res "proxy-authenticate"
                                              "Basic realm=\"proxy\"")
             (http-server:response-text! res "proxy auth required")
             res)
            (else
             (http-server:response-status-set! res code)
             (http-server:response-text! res "status")
             res))))

  (define (status-204-headers-handler req res)
    (http-server:response-status-set! res 204)
    (http-server:response-header-set! res "content-length" "3")
    (http-server:response-header-set! res "transfer-encoding" "chunked")
    (http-server:response-bytes! res #*"abc")
    res)

  (define (status-304-empty-handler req res)
    (http-server:response-status-set! res 304)
    res)

  (define (head-body-handler req res)
    (http-server:response-bytes! res #*"abcdef")
    res)

  (define (head-port-handler req res)
    (http-server:response-port! res (open-bytevector-input-port #*"abcdef"))
    res)

  (define (bad-content-length-handler req res)
    (http-server:response-header-set! res "content-length" "999")
    (http-server:response-text! res "hello")
    res)

  (define (bad-transfer-encoding-handler req res)
    (http-server:response-header-set! res "transfer-encoding" "chunked")
    (http-server:response-bytes! res #*"hello")
    res)

  (define (stream-handler req res)
    (http-server:response-port! res (open-bytevector-input-port #*"hello-stream"))
    res)

  (define (forbidden-headers-handler req res)
    (http-server:response-header-set! res "connection" "keep-alive, x-remove")
    (http-server:response-header-set! res "keep-alive" "timeout=5")
    (http-server:response-header-set! res "proxy-connection" "close")
    (http-server:response-header-set! res "transfer-encoding" "chunked")
    (http-server:response-header-set! res "upgrade" "h2c")
    (http-server:response-header-set! res "te" "trailers")
    (http-server:response-header-set! res "x-remove" "to-be-removed")
    (http-server:response-header-set! res "x-keep" "ok")
    (http-server:response-text! res "ok")
    res)

  (define (connect-handler req res)
    (http-server:response-status-set! res 200)
    (http-server:response-text! res "connect")
    res)

  (http-server:router-add-route! router 'GET "/status/204-headers" status-204-headers-handler)
  (http-server:router-add-route! router 'GET "/status/304-empty" status-304-empty-handler)
  (http-server:router-add-route! router 'GET "/head-body" head-body-handler)
  (http-server:router-add-route! router 'HEAD "/head-body" head-body-handler)
  (http-server:router-add-route! router 'GET "/head-port" head-port-handler)
  (http-server:router-add-route! router 'HEAD "/head-port" head-port-handler)
  (http-server:router-add-route! router 'GET "/bad-content-length" bad-content-length-handler)
  (http-server:router-add-route! router 'GET "/bad-transfer-encoding" bad-transfer-encoding-handler)
  (http-server:router-add-route! router 'GET "/stream" stream-handler)
  (http-server:router-add-route! router 'GET "/forbidden-headers" forbidden-headers-handler)
  (http-server:router-add-route! router 'CONNECT "/connect" connect-handler)
  (http-server:router-add-route! router 'GET "/status/:code" status-handler)
  router)

(define (start-http-server config)
  (let* ((router (make-httpbin-router))
         (app (http-server:make-router-handler router))
         (server (make-http-server "0" app :config config)))
    (server-start! server :background #t)
    (thread-sleep! 0.2)
    server))

(define (make-self-signed-certificate)
  (let* ((keypair (generate-key-pair *key:rsa*))
         (now (current-date))
         (cert (make-x509-basic-certificate
		keypair
                1
                (make-x509-issuer '((CN . "localhost")))
                (make-validity now now)
                (make-x509-issuer '((CN . "localhost"))))))
    (values keypair cert)))

(define (make-http1-client)
  (http:client-builder
   (version (http:version http/1.1))
   (follow-redirects (http:redirect never))))

(define (make-http2-client)
  (http:client-builder
   (version (http:version http/2))
   (follow-redirects (http:redirect never))))

(define (run-common-compliance-tests label client base-uri)
  (define (request method path)
    (http:request-builder
     (method method)
     (uri (string-append base-uri path))))
  (define (send method path)
    (http:client-send client (request method path)))

  (let ((r (send 'GET "/status/204")))
    (test-equal (string-append label " status 204") "204" (http:response-status r))
    (assert-empty-body (string-append label " 204 body") r)
    (assert-header-absent (string-append label " 204 content-length") r "content-length")
    (assert-header-absent (string-append label " 204 transfer-encoding") r "transfer-encoding"))

  (let ((r (send 'GET "/status/204-headers")))
    (test-equal (string-append label " status 204 headers") "204" (http:response-status r))
    (assert-empty-body (string-append label " 204 headers body") r)
    (assert-header-absent (string-append label " 204 headers content-length")
                         r "content-length")
    (assert-header-absent (string-append label " 204 headers transfer-encoding")
                         r "transfer-encoding"))

  (let ((r (send 'GET "/status/205")))
    (test-equal (string-append label " status 205") "205" (http:response-status r))
    (assert-empty-body (string-append label " 205 body") r)
    (assert-header-zero-or-absent (string-append label " 205 content-length")
                                 r "content-length")
    (assert-header-absent (string-append label " 205 transfer-encoding")
                         r "transfer-encoding"))

  (let ((r (send 'GET "/status/304")))
    (test-equal (string-append label " status 304") "304" (http:response-status r))
    (assert-empty-body (string-append label " 304 body") r)
    (test-equal (string-append label " 304 content-length")
                "7"
                (response-header r "content-length")))

  (let ((r (send 'GET "/status/304-empty")))
    (test-equal (string-append label " status 304 empty") "304" (http:response-status r))
    (assert-empty-body (string-append label " 304 empty body") r)
    (assert-header-absent (string-append label " 304 empty content-length")
                         r "content-length"))

  (let ((r (send 'HEAD "/head-body")))
    (test-equal (string-append label " HEAD status") "200" (http:response-status r))
    (assert-empty-body (string-append label " HEAD body") r)
    (test-equal (string-append label " HEAD representation length")
                "6"
                (response-header r "content-length")))

  (let ((r (send 'HEAD "/head-port")))
    (test-equal (string-append label " HEAD port status") "200" (http:response-status r))
    (assert-empty-body (string-append label " HEAD port body") r)
    (assert-header-absent (string-append label " HEAD port content-length")
                         r "content-length"))

  (let ((r (send 'GET "/bad-content-length")))
    (test-equal (string-append label " bad content-length status")
                "200"
                (http:response-status r))
    (test-equal (string-append label " bad content-length normalized")
                "5"
                (response-header r "content-length"))
    (assert-header-absent (string-append label " bad content-length no transfer-encoding")
                         r "transfer-encoding")
    (test-equal (string-append label " bad content-length body")
                "hello"
                (response-body-string r)))

  (let ((r (send 'GET "/bad-transfer-encoding")))
    (test-equal (string-append label " bad transfer-encoding status")
                "200"
                (http:response-status r))
    (test-equal (string-append label " bad transfer-encoding normalized")
                "5"
                (response-header r "content-length"))
    (assert-header-absent (string-append label " bad transfer-encoding stripped")
                         r "transfer-encoding")
    (test-equal (string-append label " bad transfer-encoding body")
                "hello"
                (response-body-string r)))

  (let ((r (send 'GET "/stream")))
    (test-equal (string-append label " stream status") "200" (http:response-status r))
    (test-equal (string-append label " stream body")
                "hello-stream"
                (response-body-string r))
    (assert-header-absent (string-append label " stream content-length")
                         r "content-length"))

  (let ((r (send 'GET "/status/103")))
    (test-equal (string-append label " final informational response is rejected")
                "500"
                (http:response-status r)))

  (let ((r (send 'GET "/status/407")))
    (test-equal (string-append label " 407 status") "407" (http:response-status r))
    (test-equal (string-append label " proxy-authenticate preserved")
                "Basic realm=\"proxy\""
                (response-header r "proxy-authenticate"))))

(define (run-http1-only-tests client base-uri)
  (define r
    (http:client-send client
                      (http:request-builder
                       (method 'CONNECT)
                       (uri (string-append base-uri "/connect")))))
  (test-equal "http/1.1 CONNECT 2xx is rejected" "501" (http:response-status r)))

(define (run-http2-only-tests client base-uri)
  (define r
    (http:client-send client
                      (http:request-builder
                       (method 'GET)
                       (uri (string-append base-uri "/forbidden-headers")))))
  (test-equal "http/2 forbidden headers status" "200" (http:response-status r))
  (assert-header-absent "http/2 strips connection" r "connection")
  (assert-header-absent "http/2 strips keep-alive" r "keep-alive")
  (assert-header-absent "http/2 strips proxy-connection" r "proxy-connection")
  (assert-header-absent "http/2 strips transfer-encoding" r "transfer-encoding")
  (assert-header-absent "http/2 strips upgrade" r "upgrade")
  (assert-header-absent "http/2 strips te" r "te")
  (assert-header-absent "http/2 strips connection-nominated custom header" r "x-remove")
  (test-equal "http/2 keeps non-connection header" "ok" (response-header r "x-keep")))

(let* ((server (start-http-server (make-http-server-config)))
       (base-uri (format "http://localhost:~a" (server-port server)))
       (client (make-http1-client)))
  (dynamic-wind
      (lambda () #f)
      (lambda ()
        (run-common-compliance-tests "http/1.1" client base-uri)
        (run-http1-only-tests client base-uri))
      (lambda ()
        (http:client-shutdown! client)
        (server-stop! server))))

(let-values (((keypair cert) (make-self-signed-certificate)))
  (let* ((config (make-http-server-config
		  :secure? #t
		  :certificates (list cert)
		  :private-key (key-pair-private keypair)))
	 (server (start-http-server config))
	 (base-uri (format "https://localhost:~a" (server-port server)))
	 (client (make-http2-client)))
    (dynamic-wind
        (lambda () #f)
        (lambda ()
          (run-common-compliance-tests "http/2" client base-uri)
          (run-http2-only-tests client base-uri))
        (lambda ()
          (http:client-shutdown! client)
          (server-stop! server)))))

(test-end)
