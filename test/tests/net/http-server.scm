#!read-macro=sagittarius/bv-string
(import (rnrs)
        (net socket)
        (net server)
        (net http-server)
	(net http-server upgrade)
	(net http-server protocol)
	(net http-server http1)
  (rfc http2 frame)
  (rfc http2 hpack)
  (srfi :1)
        (srfi :18)
        (srfi :64))

(test-begin "net/http-server")

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
  (define legacy-driver
    (make-http-server:protocol-driver
     "legacy"
     (lambda (state buffer . rest)
       (values 'start #f #f buffer state))
     (lambda (socket req result)
       #f)))
  (test-assert "legacy protocol driver"
               (http-server:protocol-driver? legacy-driver))
  (test-equal "legacy protocol driver name"
              "legacy"
              (http-server:protocol-driver-name legacy-driver))
  (test-assert "legacy driver not connection-oriented"
               (not (http-server:connection-oriented-driver? legacy-driver)))
  (let-values (((kind req x remainder next-state)
                (http-server:protocol-driver-consume!
                 legacy-driver
                 'state
                 #vu8(1 2 3))))
    (test-equal "legacy consume kind" 'start kind)
    (test-eqv "legacy consume req" #f req)
    (test-eqv "legacy consume extra" #f x)
    (test-equal "legacy consume remainder" #vu8(1 2 3) remainder)
    (test-eqv "legacy consume next-state" 'state next-state))
  (test-eqv "legacy serve" #f
            (http-server:protocol-driver-serve! legacy-driver #f #f #f))
  (test-error "legacy connect unsupported" condition?
              (http-server:protocol-driver-connect!
               legacy-driver
               'socket
               'config
               'handler)))

(let ()
  (define connect-args #f)
  (define conn-driver
    (make-http-server:protocol-driver
     "conn"
     (lambda args (assertion-violation 'conn-driver "unused consume" args))
     (lambda args #f)
     (lambda (socket config app-handler . opts)
       (set! connect-args (list socket config app-handler opts))
       (make-http-server:connection
        (lambda (chunk) (bytevector? chunk))
        (lambda () 'closed)))))
  (test-assert "connection oriented driver"
               (http-server:connection-oriented-driver? conn-driver))
  (let ((conn (http-server:protocol-driver-connect!
               conn-driver
               'socket
               'config
               'handler
               'extra-option)))
    (test-assert "connection object" (http-server:connection? conn))
    (test-equal "connection args"
                '(socket config handler (extra-option))
                connect-args)
    (test-eqv "connection process" #t
              (http-server:connection-process! conn #vu8(0)))
    (test-equal "connection close" 'closed
                (http-server:connection-close! conn))))

(let ()
  (define driver *http-server:http1-driver*)
  (define line #*"POST /c HTTP/1.1\r\n")
  (define head #*"Host: localhost\r\nContent-Length: 4\r\n\r\n")
  (let-values (((s1 req1 x1 rem1 st1)
                (http-server:protocol-driver-consume! driver #f line)))
    (test-equal "consume state line" 'line s1)
    (test-eqv "consume line req" #f req1)
    (test-eqv "consume line extra" #f x1)
    (let-values (((s2 req2 x2 rem2 st2)
                  (http-server:protocol-driver-consume!
                   driver
                   st1
                   (bytevector-append rem1 head #*"tes"))))
      (test-equal "consume state header" 'header s2)
      (test-eqv "consume header req" #f req2)
      (test-eqv "consume header extra" #f x2)
      (let-values (((s3 req3 x3 rem3 st3)
                    (http-server:protocol-driver-consume!
                     driver
                     st2
                     (bytevector-append rem2 #*"t"))))
        (test-equal "consume state ready" 'ready s3)
        (test-equal "consume body" #*"test" (http-server:request-body-bytevector req3))
        (test-eqv "consume ready extra" #f x3)
        (test-equal "consume ready remainder" #vu8() rem3)
        (test-eqv "consume ready state reset" #f st3)))))

(let ()
  (define driver *http-server:http1-driver*)
  (define pipeline
    #*"GET /a HTTP/1.1\r\nHost: localhost\r\n\r\nGET /b HTTP/1.1\r\nHost: localhost\r\n\r\n")
  (let-values (((s1 req1 x1 rem1 st1)
                (http-server:protocol-driver-consume! driver #f pipeline)))
    (test-equal "pipeline first status" 'ready s1)
    (test-equal "pipeline first path" "/a" (http-server:request-path req1))
    (test-eqv "pipeline first extra" #f x1)
    (test-eqv "pipeline first state reset" #f st1)
    (let-values (((s2 req2 x2 rem2 st2)
                  (http-server:protocol-driver-consume! driver #f rem1)))
      (test-equal "pipeline second status" 'ready s2)
      (test-equal "pipeline second path" "/b" (http-server:request-path req2))
      (test-eqv "pipeline second extra" #f x2)
      (test-eqv "pipeline second state reset" #f st2)
      (test-equal "pipeline second remainder" #vu8() rem2))))

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
  (define (app req res)
    (http-server:response-text! res "ok")
    res)
  (define server (make-http-server "0" app))
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
    (make-http-server-config :http2-cleartext? #t))
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
  (define registry (make-http-server:upgrade-registry))
  (define (app req res)
    (http-server:response-text! res "http1-fallback")
    res)
  (define server (make-http-server "0" app :upgrade-registry registry))
  (http-server:register-upgrade-handler!
   registry
   "x-echo"
   (lambda (conn req remainder app-handler)
     (socket-send
      (http-server:connection-socket conn)
      #*"HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: x-echo\r\n\r\n")
     (values 'handled
             (make-http-server:custom-connection
              (http-server:connection-server conn)
              (http-server:connection-socket conn)
              (lambda (chunk) #t)
              (lambda () #t))
             #t)))
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
    (socket-close sock))
  (server-stop! server))

(test-end)
