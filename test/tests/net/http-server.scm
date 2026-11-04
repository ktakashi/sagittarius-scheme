#!read-macro=sagittarius/bv-string
(import (rnrs)
	(net socket)
	(net server)
	(net http-server)
	(net http-server upgrade)
	(net http-server protocol)
	(net http-server http1)
	(net http-server h2c) ;; for upgrade 
	(sagittarius crypto keys)
	(rfc tls)
	(rfc x509)
	(rfc http2 frame)
	(rfc http2 hpack)
	(srfi :1)
	(srfi :18)
	(srfi :19)
	(srfi :64))

(test-begin "net/http-server")

(define default-config
  (make-http-server-config
   :exception-handler (lambda (s sock e) (report-error e))))

(define (recv-text sock)
  (socket-set-read-timeout! sock 1000)
  (let loop ((acc '()))
    (guard (e ((socket-read-timeout-error? e)
               (apply string-append (reverse acc)))
              ((socket-closed-error? e)
               (apply string-append (reverse acc))))
      (let ((bv (socket-recv sock 8192)))
        (if (and bv (bytevector? bv) (> (bytevector-length bv) 0))
            (loop (cons (utf8->string bv) acc))
            (apply string-append (reverse acc)))))))

(define (contains? s part)
  (let ((n (string-length s))
        (m (string-length part)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) part) #t)
            (else (loop (+ i 1)))))))

(define (flag-set? flags mask)
  (not (zero? (bitwise-and flags mask))))

(define (encode-frames frames)
  (define ctx (make-hpack-context 4096))
  (define buffer (make-frame-buffer))
  (call-with-bytevector-output-port
   (lambda (out)
     (for-each (lambda (entry)
                 (write-http2-frame out buffer (car entry) (cdr entry) ctx))
               frames))))

(define (decode-frames bv)
  (let ((len (bytevector-length bv))
        (ctx (make-hpack-context 4096))
        (buffer (make-frame-buffer)))
    (let loop ((offset 0) (out '()))
      (if (= offset len)
          (reverse out)
          (let ((end (http2-frame-block-end bv offset +http2-max-frame-buffer-size+)))
            (if (not end)
                (reverse out)
                (let ((frame
                       (read-http2-frame
                        (open-bytevector-input-port (bytevector-copy bv offset end))
                        buffer
                        ctx)))
                  (loop end (cons frame out)))))))))

(define (recv-bytes sock)
  (socket-set-read-timeout! sock 200)
  (let loop ((chunks '()))
    (guard (e ((socket-read-timeout-error? e)
               (bytevector-concatenate (reverse chunks)))
              ((socket-closed-error? e)
               (bytevector-concatenate (reverse chunks))))
      (let ((bv (socket-recv sock 8192)))
        (if (and bv (bytevector? bv) (> (bytevector-length bv) 0))
            (loop (cons bv chunks))
            (bytevector-concatenate (reverse chunks)))))))

(define (header-value headers name)
  (let ((key (string->utf8 name)))
    (let loop ((rest headers))
      (and (pair? rest)
           (let* ((e (car rest))
                  (n (and (pair? e) (car e)))
                  (v (and (pair? e) (pair? (cdr e)) (cadr e))))
             (if (and (bytevector? n) (bytevector? v) (bytevector=? n key))
                 (utf8->string v)
                 (loop (cdr rest))))))))

(define (bytevector-find-subsequence bv sub)
  (let ((n (bytevector-length bv))
        (m (bytevector-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((let loop2 ((j 0))
               (if (= j m)
                   #t
                   (and (= (bytevector-u8-ref bv (+ i j))
                           (bytevector-u8-ref sub j))
                        (loop2 (+ j 1)))))
             i)
            (else (loop (+ i 1)))))))

(define (split-http-response bv)
  (let* ((sep #*"\r\n\r\n")
         (idx (bytevector-find-subsequence bv sep)))
    (if idx
        (values (bytevector-copy bv 0 (+ idx 4))
                (bytevector-copy bv (+ idx 4) (bytevector-length bv)))
        (values bv #vu8()))))

(let ()
  (define ci (make-http-server:connection-info '(remote . "local") #f "h2" "h2"))
  (define ex (make-http-server:exchange ci))
  (test-assert "exchange predicate" (http-server:exchange? ex))
  (test-equal "exchange initial state" 'created (http-server:exchange-state ex))
  (test-assert "exchange has connection info"
	       (eq? ci (http-server:exchange-connection-info ex)))
  (test-eqv "exchange unknown attribute defaults to #f"
	    #f
	    (http-server:exchange-attribute-ref ex 'trace-id))
  (http-server:exchange-attribute-set! ex 'trace-id "abc")
  (test-equal "exchange stores attributes"
	      "abc"
	      (http-server:exchange-attribute-ref ex 'trace-id)))

(let ()
  (define handled-exchange #f)
  (define req-exchange #f)
  (define res-exchange #f)
  (define state-in-handler #f)
  (define (app req res)
    (set! req-exchange (http-server:request-exchange req))
    (set! res-exchange (http-server:response-exchange res))
    (set! state-in-handler (http-server:exchange-state req-exchange))
    (set! handled-exchange req-exchange)
    (http-server:response-text! res "ok")
    res)
  (define server (make-http-server "0" app))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"GET /exchange HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "exchange request status" (contains? txt "HTTP/1.1 200 OK")))
    (socket-close sock))
  (server-stop! server)
  (test-assert "request and response share exchange"
	       (and req-exchange res-exchange (eq? req-exchange res-exchange)))
  (test-equal "exchange is handling in app" 'handling state-in-handler)
  (test-equal "exchange completes after response"
	      'completed
	      (and handled-exchange
		   (http-server:exchange-state handled-exchange))))

(let ()
  (define observed-request-exchange #f)
  (define replacement #f)
  (define (app req res)
    (set! observed-request-exchange (http-server:request-exchange req))
    (set! replacement (make-http-server:response 201))
    (http-server:response-text! replacement "created")
    replacement)
  (define server (make-http-server "0" app))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"GET /replacement HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "replacement response status" (contains? txt "HTTP/1.1 201 Created"))
      (test-assert "replacement response body" (contains? txt "created")))
    (socket-close sock))
  (server-stop! server)
  (test-assert "replacement response is bound to request exchange"
	       (and replacement observed-request-exchange
		    (eq? (http-server:response-exchange replacement)
			 observed-request-exchange))))

(let ()
  (define seen-exchange #f)
  (define (app req res)
    (set! seen-exchange (http-server:request-exchange req))
    (let* ((foreign-info (make-http-server:connection-info #f))
	   (foreign-exchange (make-http-server:exchange foreign-info))
	   (foreign-response (make-http-server:response 200 foreign-exchange)))
      (http-server:response-text! foreign-response "must not be used")
      foreign-response))
  (define server (make-http-server "0" app))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"GET /invalid-replacement HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "cross exchange replacement rejected"
		   (contains? txt "HTTP/1.1 500 Internal Server Error")))
    (socket-close sock))
  (server-stop! server)
  (test-equal "exchange remains completed after safe error response"
	      'completed
	      (and seen-exchange (http-server:exchange-state seen-exchange))))

(let ()
  (define observed-request-exchange #f)
  (define observed-response-exchange #f)
  (define provided-response #f)
  (define (app req res)
    (set! observed-request-exchange (http-server:request-exchange req))
    (set! observed-response-exchange (http-server:response-exchange res))
    (set! provided-response res)
    (http-server:response-text! res "fallback")
    'ignored)
  (define server (make-http-server "0" app))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"GET /fallback HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (recv-text sock)
    (socket-close sock))
  (server-stop! server)
  (test-assert "non response handler was invoked"
	       observed-request-exchange)
  (test-equal "non response result keeps fallback response body"
	      "fallback"
	      (and provided-response
		   (utf8->string
		    (http-server:response-body provided-response))))
  (test-assert "fallback response uses request exchange"
	       (and observed-request-exchange observed-response-exchange
		    (eq? observed-request-exchange observed-response-exchange))))

(let ()
  (define exchanges '())
  (define (app req res)
    (set! exchanges (cons (http-server:request-exchange req) exchanges))
    (http-server:response-text! res (http-server:request-path req))
    res)
  (define server (make-http-server "0" app))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock
		 #*"GET /first HTTP/1.1\r\nHost: localhost\r\n\r\nGET /second HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (recv-text sock)
    (socket-close sock))
  (server-stop! server)
  (let ((seen (reverse exchanges)))
    (test-equal "two exchanges observed on keep-alive connection" 2 (length seen))
    (test-assert "keep-alive requests use different exchanges"
		 (and (= (length seen) 2)
		      (not (eq? (car seen) (cadr seen)))))
    (test-assert "keep-alive requests share connection info"
		 (and (= (length seen) 2)
		      (eq? (http-server:exchange-connection-info (car seen))
			   (http-server:exchange-connection-info (cadr seen)))))))

;; TODO driver tests
;; (let ()
;;   (define legacy-driver
;;     (make-http-server:protocol-driver
;;      "legacy"
;;      (lambda (state buffer . rest)
;;        (values 'start #f #f buffer state))
;;      (lambda (socket req result)
;;        #f)))
;;   (test-assert "legacy protocol driver"
;;                (http-server:protocol-driver? legacy-driver))
;;   (test-equal "legacy protocol driver name"
;;               "legacy"
;;               (http-server:protocol-driver-name legacy-driver))
;;   (test-assert "legacy driver not connection-oriented"
;;                (not (http-server:connection-oriented-driver? legacy-driver)))
;;   (let-values (((kind req x remainder next-state)
;;                 (http-server:protocol-driver-consume!
;;                  legacy-driver
;;                  'state
;;                  #vu8(1 2 3))))
;;     (test-equal "legacy consume kind" 'start kind)
;;     (test-eqv "legacy consume req" #f req)
;;     (test-eqv "legacy consume extra" #f x)
;;     (test-equal "legacy consume remainder" #vu8(1 2 3) remainder)
;;     (test-eqv "legacy consume next-state" 'state next-state))
;;   (test-eqv "legacy serve" #f
;;             (http-server:protocol-driver-serve! legacy-driver #f #f #f))
;;   (test-error "legacy connect unsupported" condition?
;;               (http-server:protocol-driver-connect!
;;                legacy-driver
;;                'socket
;;                'config
;;                'handler)))

;; (let ()
;;   (define connect-args #f)
;;   (define conn-driver
;;     (make-http-server:protocol-driver
;;      "conn"
;;      (lambda args (assertion-violation 'conn-driver "unused consume" args))
;;      (lambda args #f)
;;      (lambda (socket config app-handler . opts)
;;        (set! connect-args (list socket config app-handler opts))
;;        (make-http-server:connection
;;         (lambda (chunk) (bytevector? chunk))
;;         (lambda () 'closed)))))
;;   (test-assert "connection oriented driver"
;;                (http-server:connection-oriented-driver? conn-driver))
;;   (let ((conn (http-server:protocol-driver-connect!
;;                conn-driver
;;                'socket
;;                'config
;;                'handler
;;                'extra-option)))
;;     (test-assert "connection object" (http-server:connection? conn))
;;     (test-equal "connection args"
;;                 '(socket config handler (extra-option))
;;                 connect-args)
;;     (test-eqv "connection process" #t
;;               (http-server:connection-process! conn #vu8(0)))
;;     (test-equal "connection close" 'closed
;;                 (http-server:connection-close! conn))))

;; (let ()
;;   (define driver *http-server:http1-driver*)
;;   (define line #*"POST /c HTTP/1.1\r\n")
;;   (define head #*"Host: localhost\r\nContent-Length: 4\r\n\r\n")
;;   (let-values (((s1 req1 x1 rem1 st1)
;;                 (http-server:protocol-driver-consume! driver #f line)))
;;     (test-equal "consume state line" 'line s1)
;;     (test-eqv "consume line req" #f req1)
;;     (test-eqv "consume line extra" #f x1)
;;     (let-values (((s2 req2 x2 rem2 st2)
;;                   (http-server:protocol-driver-consume!
;;                    driver
;;                    st1
;;                    (bytevector-append rem1 head #*"tes"))))
;;       (test-equal "consume state header" 'header s2)
;;       (test-eqv "consume header req" #f req2)
;;       (test-eqv "consume header extra" #f x2)
;;       (let-values (((s3 req3 x3 rem3 st3)
;;                     (http-server:protocol-driver-consume!
;;                      driver
;;                      st2
;;                      (bytevector-append rem2 #*"t"))))
;;         (test-equal "consume state ready" 'ready s3)
;;         (test-equal "consume body" #*"test" (http-server:request-body-bytevector req3))
;;         (test-eqv "consume ready extra" #f x3)
;;         (test-equal "consume ready remainder" #vu8() rem3)
;;         (test-eqv "consume ready state reset" #f st3)))))

;; (let ()
;;   (define driver *http-server:http1-driver*)
;;   (define pipeline
;;     #*"GET /a HTTP/1.1\r\nHost: localhost\r\n\r\nGET /b HTTP/1.1\r\nHost: localhost\r\n\r\n")
;;   (let-values (((s1 req1 x1 rem1 st1)
;;                 (http-server:protocol-driver-consume! driver #f pipeline)))
;;     (test-equal "pipeline first status" 'ready s1)
;;     (test-equal "pipeline first path" "/a" (http-server:request-path req1))
;;     (test-eqv "pipeline first extra" #f x1)
;;     (test-eqv "pipeline first state reset" #f st1)
;;     (let-values (((s2 req2 x2 rem2 st2)
;;                   (http-server:protocol-driver-consume! driver #f rem1)))
;;       (test-equal "pipeline second status" 'ready s2)
;;       (test-equal "pipeline second path" "/b" (http-server:request-path req2))
;;       (test-eqv "pipeline second extra" #f x2)
;;       (test-eqv "pipeline second state reset" #f st2)
;;       (test-equal "pipeline second remainder" #vu8() rem2))))

(let ()
  (define (app req res)
    (http-server:response-status-set! res 201)
    (http-server:response-header-set! res "x-test" "ok")
    (http-server:response-text! res "created")
    res)
  (define server (make-http-server "0" app))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "status 201" (contains? txt "HTTP/1.1 201 Created"))
      (test-assert "custom header" (contains? txt "x-test: ok"))
      (test-assert "body" (contains? txt "created")))
    (socket-close sock))
  (server-stop! server))

(let* ((router (make-http-server:router)))
  (http-server:router-add-route!
   router
   'GET
   "/hello/:name"
   (lambda (req res)
     (http-server:response-text!
      res
      (string-append "hello " (http-server:request-attribute-ref req 'name "?")))))
  (let* ((app (http-server:make-router-handler router))
         (server (make-http-server "0" app)))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"GET /hello/sagittarius HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "router path var" (contains? txt "hello sagittarius")))
    (socket-close sock))
  (server-stop! server)))

(let ()
  (define count 0)
  (define (app req res)
    (set! count (+ count 1))
    (http-server:response-cacheable?-set! res #t)
    (http-server:response-cache-ttl-set! res 10)
    (http-server:response-text! res (string-append "count=" (number->string count)))
    res)
  (define config
    (make-http-server-config
     :cache (make-http-server:memory-cache :capacity 16)))
  (define server (make-http-server "0" app :config config))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock1 (make-client-socket "localhost" (server-port server)))
        (sock2 (make-client-socket "localhost" (server-port server))))
    (socket-send sock1 #*"GET /cached HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt1 (recv-text sock1)))
      (test-assert "first response" (contains? txt1 "count=1")))
    (socket-send sock2 #*"GET /cached HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt2 (recv-text sock2)))
      (test-assert "second response from cache" (contains? txt2 "count=1")))
    (socket-close sock1)
    (socket-close sock2))
  (test-equal "handler invoked once" 1 count)
  (server-stop! server))

(let ()
  (define (app req res)
    (http-server:response-text! res (http-server:request-path req))
    res)
  (define server (make-http-server "0" app))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"GET /a HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt1 (recv-text sock)))
      (test-assert "first response status" (contains? txt1 "HTTP/1.1 200 OK"))
      (test-assert "first response body" (contains? txt1 "/a")))
    (socket-close sock))
  (server-stop! server))

(let ()
  (define payload #*"hello-stream")
  (define (app req res)
    (http-server:response-port! res (open-bytevector-input-port payload))
    res)
  (define server (make-http-server "0" app :config default-config))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"GET /stream HTTP/1.1\r\nHost: localhost\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "response-port sets default content-type"
                   (contains? txt "content-type: application/octet-stream"))
      (test-assert "response-port uses chunked transfer"
                   (contains? txt "transfer-encoding: chunked"))
      (test-assert "response-port does not force content-length"
                   (not (contains? txt "content-length:")))
      (test-assert "response-port emits chunked payload"
                   (contains? txt "c\r\nhello-stream\r\n0\r\n\r\n")))
    (socket-close sock))
  (server-stop! server))

(let ()
  (define (app req res)
    (http-server:response-text! res "ok")
    res)
  (define server (make-http-server "0" app :config default-config))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n4\r\ntest\r\n0\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "chunked accepted" (contains? txt "HTTP/1.1 200 OK")))
    (socket-close sock))

  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\n\r\ntest")
    (let ((txt (recv-text sock)))
      (test-assert "content length accepted" (contains? txt "HTTP/1.1 200 OK")))
    (socket-close sock))
  
  ;; need more situation (chunked)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\n")
    (thread-sleep! 0.01)
    (socket-send sock #*"Host: localhost\r\nTransfer-Encoding: chunked\r\n\r\n")
    ;; trigger need more on chunk
    (socket-send sock #*"4\r\ntes")
    (thread-sleep! 0.01)
    (socket-send sock #*"t\r\n0\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "chunked accepted" (contains? txt "HTTP/1.1 200 OK")))
    (socket-close sock))

  ;; need more situation (content-length)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\n")
    (thread-sleep! 0.01)
    (socket-send sock #*"Host: localhost\r\nContent-Length: 4\r\n\r\n")
    (thread-sleep! 0.01)
    (socket-send sock #*"tes")
    (thread-sleep! 0.01)
    (socket-send sock #*"t")
    (let ((txt (recv-text sock)))
      (test-assert "content length accepted" (contains? txt "HTTP/1.1 200 OK")))
    (socket-close sock))

  ;; error case
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\n")
    (socket-send sock #*"Host: localhost\r\n")
    (socket-send sock #*"Transfer-Encoding: chunked\r\n")
    (socket-send sock #*"Content-Length: 4\r\n\r\n")
    (socket-send sock #*"test")
    (let ((txt (recv-text sock)))
      (test-assert "chunked and contnet length" (contains? txt "HTTP/1.1 400 Bad Request")))
    (socket-close sock))

  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\n")
    (socket-send sock #*"Host: localhost\r\n")
    (socket-send sock #*"Content-Length: nan\r\n\r\n")
    (socket-send sock #*"test")
    (let ((txt (recv-text sock)))
      (test-assert "content length invalid" (contains? txt "HTTP/1.1 400 Bad Request")))
    (socket-close sock))

  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\n")
    (socket-send sock #*"Host: localhost\r\n")
    (socket-send sock #*"Transfer-Encoding: chunked\r\n\r\n")
    (socket-send sock #*"nan\r\ntest\r\n0\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "chunked invalid" (contains? txt "HTTP/1.1 400 Bad Request")))
    (socket-close sock))

  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\n")
    (socket-send sock #*"Host: localhost\r\n")
    (socket-send sock #*"Transfer-Encoding: chunked\r\n\r\n")
    (socket-send sock #*"-1\r\ntest\r\n0\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "chunked invalid (negative)" (contains? txt "HTTP/1.1 400 Bad Request")))
    (socket-close sock))

  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock #*"POST /c HTTP/1.1\r\n")
    (socket-send sock #*"Host: localhost\r\n")
    (socket-send sock #*"Transfer-Encoding: chunked\r\n\r\n")
    (socket-send sock #*"4\r\ntest boo")
    (let ((txt (recv-text sock)))
      (test-assert "chunked invalid (length)" (contains? txt "HTTP/1.1 400 Bad Request")))
    (socket-close sock))
  
  
  (server-stop! server))

(let ()
  (define last-port #f)
  (define (app req res)
    (let ((path (http-server:request-path req))
          (method (http-server:request-method req)))
      (cond
       ((and (eq? method 'CONNECT)
             (string=? path "example.com:443"))
        (http-server:response-status-set! res 200)
        (http-server:response-text! res "connect-body")
        res)
       ((string=? path "/ok")
        (http-server:response-text! res "ok")
        res)
       ((string=? path "/204-default")
        (http-server:response-status-set! res 204)
        res)
       ((string=? path "/204-bytes")
        (http-server:response-status-set! res 204)
        (http-server:response-bytes! res #*"abc")
        res)
       ((string=? path "/204-cl")
        (http-server:response-status-set! res 204)
        (http-server:response-header-set! res "content-length" "3")
        (http-server:response-bytes! res #*"abc")
        res)
       ((string=? path "/204-te")
        (http-server:response-status-set! res 204)
        (http-server:response-header-set! res "transfer-encoding" "chunked")
        (http-server:response-bytes! res #*"abc")
        res)
       ((string=? path "/205-bytes")
        (http-server:response-status-set! res 205)
        (http-server:response-bytes! res #*"abc")
        res)
       ((string=? path "/205-port")
        (http-server:response-status-set! res 205)
        (set! last-port (open-bytevector-input-port #*"abc"))
        (http-server:response-port! res last-port)
        res)
       ((string=? path "/304-valid")
        (http-server:response-status-set! res 304)
        (http-server:response-header-set! res "content-length" "7")
        (http-server:response-bytes! res #*"ignored")
        res)
       ((string=? path "/304-empty")
        (http-server:response-status-set! res 304)
        res)
       ((string=? path "/head-bytes")
        (http-server:response-bytes! res #*"abcdef")
        res)
       ((string=? path "/head-port")
        (set! last-port (open-bytevector-input-port #*"abcdef"))
        (http-server:response-port! res last-port)
        res)
       ((string=? path "/200-bad-cl")
        (http-server:response-header-set! res "content-length" "999")
        (http-server:response-text! res "hello")
        res)
       ((string=? path "/200-app-te")
        (http-server:response-header-set! res "transfer-encoding" "chunked")
        (http-server:response-bytes! res #*"hello")
        res)
       ((string=? path "/stream")
        (http-server:response-port! res (open-bytevector-input-port #*"hello-stream"))
        res)
       ((string=? path "/status-1xx")
        (http-server:response-status-set! res 103)
        (http-server:response-text! res "hint")
        res)
       (else
        (http-server:response-text! res "unknown")
        res))))
  (define (send-request payload)
    (let ((sock (make-client-socket "localhost" (server-port server))))
      (socket-send sock payload)
      (let ((raw (recv-bytes sock)))
        (socket-close sock)
        raw)))
  (define server (make-http-server "0" app :config default-config))
  (server-start! server :background #t)
  (thread-sleep! 0.2)

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /204-default HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "204 default status" (contains? h "HTTP/1.1 204 No Content"))
      (test-assert "204 default no content-length" (not (contains? h "content-length:")))
      (test-assert "204 default no transfer-encoding" (not (contains? h "transfer-encoding:")))
      (test-equal "204 default no body" 0 (bytevector-length body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /204-bytes HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "204 bytes no content-length" (not (contains? h "content-length:")))
      (test-assert "204 bytes no transfer-encoding" (not (contains? h "transfer-encoding:")))
      (test-equal "204 bytes no body" 0 (bytevector-length body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /204-cl HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "204 strips supplied content-length" (not (contains? h "content-length:")))
      (test-assert "204 cl no transfer-encoding" (not (contains? h "transfer-encoding:")))
      (test-equal "204 cl no body" 0 (bytevector-length body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /204-te HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "204 strips supplied transfer-encoding" (not (contains? h "transfer-encoding:")))
      (test-assert "204 te no content-length" (not (contains? h "content-length:")))
      (test-equal "204 te no body" 0 (bytevector-length body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /205-bytes HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "205 bytes status" (contains? h "HTTP/1.1 205 Reset Content"))
      (test-assert "205 bytes has zero content-length" (contains? h "content-length: 0"))
      (test-assert "205 bytes no transfer-encoding" (not (contains? h "transfer-encoding:")))
      (test-equal "205 bytes no body" 0 (bytevector-length body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /205-port HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "205 port has zero content-length" (contains? h "content-length: 0"))
      (test-equal "205 port no body" 0 (bytevector-length body))
      (test-assert "205 port is closed" (and last-port (port-closed? last-port)))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /304-valid HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "304 valid status" (contains? h "HTTP/1.1 304 Not Modified"))
      (test-assert "304 valid keeps representation content-length" (contains? h "content-length: 7"))
      (test-equal "304 valid no body" 0 (bytevector-length body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /304-empty HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "304 empty no content-length" (not (contains? h "content-length:")))
      (test-equal "304 empty no body" 0 (bytevector-length body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"HEAD /head-bytes HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "HEAD bytes has representation length" (contains? h "content-length: 6"))
      (test-equal "HEAD bytes no body" 0 (bytevector-length body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"HEAD /head-port HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "HEAD port has no unverifiable content-length"
                   (not (contains? h "content-length:")))
      (test-equal "HEAD port no body" 0 (bytevector-length body))
      (test-assert "HEAD port is closed" (and last-port (port-closed? last-port)))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /200-bad-cl HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "200 normalizes mismatched content-length" (contains? h "content-length: 5"))
      (test-assert "200 removed bad supplied content-length" (not (contains? h "content-length: 999")))
      (test-equal "200 bad-cl body" "hello" (utf8->string body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /200-app-te HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "200 app transfer-encoding removed" (not (contains? h "transfer-encoding:")))
      (test-assert "200 app transfer-encoding keeps content-length" (contains? h "content-length: 5"))
      (test-equal "200 app te body" "hello" (utf8->string body))))

  (let-values (((head body)
                (split-http-response
                 (send-request #*"GET /stream HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (let ((h (utf8->string head)))
      (test-assert "stream response is chunked" (contains? h "transfer-encoding: chunked"))
      (test-assert "stream response omits content-length" (not (contains? h "content-length:")))
      (test-assert "stream response ends with terminating chunk"
                   (contains? (utf8->string body) "0\r\n\r\n"))))

  (let ((txt (utf8->string
              (send-request #*"GET /status-1xx HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))))
    (test-assert "final 1xx handler response is rejected"
                 (contains? txt "HTTP/1.1 500 Internal Server Error")))

  (let ((txt (utf8->string
              (send-request #*"CONNECT example.com:443 HTTP/1.1\r\nHost: example.com:443\r\nConnection: close\r\n\r\n"))))
    (test-assert "successful CONNECT is rejected without tunnel support"
                 (contains? txt "HTTP/1.1 501 Not Implemented")))

  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send sock
                 #*"GET /204-default HTTP/1.1\r\nHost: localhost\r\n\r\nGET /ok HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
    (let* ((raw (recv-bytes sock)))
      (let-values (((head tail) (split-http-response raw)))
        (let ((h1 (utf8->string head))
              (h2 (utf8->string tail)))
          (test-assert "persistent connection survives 204"
                       (contains? h2 "HTTP/1.1 200 OK"))
          (test-assert "first 204 has no content-length"
                       (not (contains? h1 "content-length:")))
          (test-assert "first 204 has no transfer-encoding"
                       (not (contains? h1 "transfer-encoding:"))))))
    (socket-close sock))

  (server-stop! server))

;; prior knowledge is not supported. 
;; Maybe if we have better handling
;; (let ()
;;   (define request-headers
;;     '((#*":method" #*"GET")
;;       (#*":scheme" #*"http")
;;       (#*":path" #*"/h2c-prior")
;;       (#*":authority" #*"localhost")))
;;   (define (app req res)
;;     (http-server:response-text! res "h2c-prior-ok")
;;     res)
;;   (define config
;;     (make-http-server-config :http2-cleartext? #t))
;;   (define server (make-http-server "0" app :config config))
;;   (server-start! server :background #t)
;;   (thread-sleep! 0.2)
;;   (let ((sock (make-client-socket "localhost" (server-port server))))
;;     (socket-send
;;      sock
;;      (bytevector-append
;;       +http2-connection-preface+
;;       (encode-frames
;;        (list (cons (make-http2-frame-settings 0 0 '()) #f)
;;              (cons (make-http2-frame-headers 0 1 #f #f request-headers) #t)))))
;;     (thread-sleep! 0.05)
;;     (let* ((frames (decode-frames (recv-bytes sock)))
;;            (response-headers-frame
;;             (find (lambda (f)
;;                     (and (http2-frame-headers? f)
;;                          (= (http2-frame-stream-identifier f) 1)))
;;                   frames))
;;            (response-data-frame
;;             (find (lambda (f)
;;                     (and (http2-frame-data? f)
;;                          (= (http2-frame-stream-identifier f) 1)))
;;                   frames)))
;;       (test-assert "h2c prior-knowledge response headers" response-headers-frame)
;;       (test-assert "h2c prior-knowledge response data" response-data-frame)
;;       (test-equal "h2c prior-knowledge status"
;;                   "200"
;;                   (and response-headers-frame
;;                        (header-value (http2-frame-headers-headers response-headers-frame)
;;                                      ":status")))
;;       (test-equal "h2c prior-knowledge body"
;;                   "h2c-prior-ok"
;;                   (and response-data-frame
;;                        (utf8->string (http2-frame-data-data response-data-frame)))))
;;     (socket-close sock))
;;   (server-stop! server))

(let ()
  (define (app req res)
    (http-server:response-text! res "h2c-upgrade-ok")
    res)
  (define config
    (make-http-server-config :http2-cleartext? #t
			     :upgrades (list *http-server:h2c-upgrade*)))
  (define server (make-http-server "0" app :config config))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send
     sock
     #*"GET /h2c-upgrade HTTP/1.1\r\nHost: localhost\r\nConnection: Upgrade, HTTP2-Settings\r\nUpgrade: h2c\r\nHTTP2-Settings: AAMAAABkAAQAAP__\r\n\r\n")
    (thread-sleep! 0.05)
    (let-values (((head tail) (split-http-response (recv-bytes sock))))
      (let* ((txt (utf8->string head))
             (frames (decode-frames tail))
             (response-headers-frame
              (find (lambda (f)
                      (and (http2-frame-headers? f)
                           (= (http2-frame-stream-identifier f) 1)))
                    frames))
             (response-data-frame
              (find (lambda (f)
                      (and (http2-frame-data? f)
                           (= (http2-frame-stream-identifier f) 1)))
                    frames))
             (settings-ack-frame
              (find (lambda (f)
                      (and (http2-frame-settings? f)
                           (flag-set? (http2-frame-flags f) +http2-frame-flag-ack+)))
                    frames)))
        (test-assert "h2c upgrade sends 101" (contains? txt "HTTP/1.1 101 Switching Protocols"))
        (test-assert "h2c upgrade sends settings ack" settings-ack-frame)
        (test-assert "h2c upgrade response headers" response-headers-frame)
        (test-assert "h2c upgrade response data" response-data-frame)
        (test-equal "h2c upgrade status"
                    "200"
                    (and response-headers-frame
                         (header-value (http2-frame-headers-headers response-headers-frame)
                                       ":status")))
        (test-equal "h2c upgrade body"
                    "h2c-upgrade-ok"
                    (and response-data-frame
                         (utf8->string (http2-frame-data-data response-data-frame))))))
    (socket-close sock))
  (server-stop! server))

(let ()
  (define keypair (generate-key-pair *key:rsa*))
  (define cert
    (make-x509-basic-certificate keypair 1
                                 (make-x509-issuer '((C . "NL")))
                                 (make-validity (current-date) (current-date))
                                 (make-x509-issuer '((C . "NL")))))
  (define request-headers
    '((#*":method" #*"GET")
      (#*":scheme" #*"https")
      (#*":path" #*"/tls-h2")
      (#*":authority" #*"localhost")))
  (define (app req res)
    (http-server:response-text! res "tls-h2-ok")
    res)
  (define config
    (make-http-server-config
     :secure? #t
     :certificates (list cert)
     :private-key (key-pair-private keypair)))
  (define server (make-http-server "0" app :config config))
  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-tls-socket
               "localhost"
               (server-port server)
               :certificate-verifier #f
               :hello-extensions (list (make-protocol-name-list '("h2"))))))
    (test-equal "tls alpn selected h2" "h2" (tls-socket-selected-alpn sock))
    (socket-send
     sock
     (bytevector-append
      +http2-connection-preface+
      (encode-frames
       (list (cons (make-http2-frame-settings 0 0 '()) #f)
             (cons (make-http2-frame-headers 0 1 #f #f request-headers) #t)))))
    (thread-sleep! 0.05)
    (let* ((frames (decode-frames (recv-bytes sock)))
           (response-headers-frame
            (find (lambda (f)
                    (and (http2-frame-headers? f)
                         (= (http2-frame-stream-identifier f) 1)))
                  frames))
           (response-data-frame
            (find (lambda (f)
                    (and (http2-frame-data? f)
                         (= (http2-frame-stream-identifier f) 1)))
                  frames)))
      (test-assert "tls h2 response headers" response-headers-frame)
      (test-assert "tls h2 response data" response-data-frame)
      (test-equal "tls h2 status"
                  "200"
                  (and response-headers-frame
                       (header-value (http2-frame-headers-headers response-headers-frame)
                                     ":status")))
      (test-equal "tls h2 body"
                  "tls-h2-ok"
                  (and response-data-frame
                       (utf8->string (http2-frame-data-data response-data-frame)))))
    (socket-close sock))
  (server-stop! server))

(let ()
  (define (app req res)
    (http-server:response-text! res "http1-fallback")
    res)
  (define server (make-http-server "0" app :config default-config))
  (define x-echo-handler
    (lambda (conn req remainder app-handler)
      (socket-send
       (http-server:connection-socket conn)
       #*"HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: x-echo\r\n\r\n")
      (values 'handled
	      (make-http-server:custom-connection
	       (http-server:connection-server conn)
	       (http-server:connection-socket conn)
	       (lambda (conn chunk)
		 (print 'here)
		 #t)
	       (lambda () #t))
	      #t)))
  (define x-echo (make-http-server:upgrade "x-echo" x-echo-handler))
  (http-server:register-upgrade! server x-echo)

  (server-start! server :background #t)
  (thread-sleep! 0.2)
  (let ((sock (make-client-socket "localhost" (server-port server))))
    (socket-send
     sock
     #*"GET /upgrade HTTP/1.1\r\nHost: localhost\r\nConnection: Upgrade\r\nUpgrade: x-echo\r\n\r\n")
    (let ((txt (recv-text sock)))
      (test-assert "custom upgrade handler sends 101"
                   (contains? txt "HTTP/1.1 101 Switching Protocols"))
      (test-assert "custom upgrade protocol token"
                   (contains? txt "Upgrade: x-echo")))
    (socket-send sock #*"hello")
    (socket-close sock))
  (server-stop! server))

(test-end)
