#!read-macro=sagittarius/bv-string
(import (rnrs)
        (net socket)
        (net http-server)
        (net http-server protocol)
        (net http-server response)
        (net http-server http2)
        (rfc http2 frame)
  (rfc http2 conditions)
        (rfc http2 hpack)
        (srfi :1)
        (srfi :18)
        (srfi :64))

(test-begin "net/http-server-http2")

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

(define (recv-bytes sock)
  (socket-set-read-timeout! sock 150)
  (let loop ((chunks '()))
    (guard (e ((socket-read-timeout-error? e)
               (bytevector-concatenate (reverse chunks)))
              ((socket-closed-error? e)
               (bytevector-concatenate (reverse chunks))))
      (let ((bv (socket-recv sock 8192)))
        (if (and bv (bytevector? bv) (> (bytevector-length bv) 0))
            (loop (cons bv chunks))
            (bytevector-concatenate (reverse chunks)))))))

(define (pump-connection! conn socket)
  (socket-set-read-timeout! socket 100)
  (let loop ()
    (guard (e ((socket-read-timeout-error? e) #t)
              ((socket-closed-error? e) #t))
      (let ((bv (socket-recv socket 8192)))
        (if (and bv (bytevector? bv) (> (bytevector-length bv) 0))
            (and (http-server:connection-process! conn bv)
                 (loop))
            #t)))))

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

(define (data-bytes-on-stream frames sid)
  (let loop ((rest frames) (sum 0))
    (if (null? rest)
        sum
        (let ((f (car rest)))
          (if (and (http2-frame-data? f)
                   (= (http2-frame-stream-identifier f) sid))
              (loop (cdr rest)
                    (+ sum (bytevector-length (http2-frame-data-data f))))
              (loop (cdr rest) sum))))))

(define (with-http2-connection app proc :optional (config (make-http-server-config)))
  (define server-sock #f)
  (define accepted #f)
  (define client #f)
  (dynamic-wind
    (lambda ()
      (set! server-sock (make-server-socket "0"))
      (let ((port (number->string (socket-info-port (socket-info server-sock)))))
        (set! client (make-client-socket "localhost" port))
        (set! accepted (socket-accept server-sock))))
    (lambda ()
      (let ((conn (make-http-server:http2-connection
		   (make-http-server "0" (lambda (req resp) #t) :config config)
                   accepted
                   app)))
        (proc client accepted conn)))
    (lambda ()
      (guard (e (else #f)) (when client (socket-close client)))
      (guard (e (else #f)) (when accepted (socket-close accepted)))
      (guard (e (else #f)) (when server-sock (socket-close server-sock))))))

(test-assert "http2 driver is connection-oriented"
             (http-server:connection-oriented-driver? *http-server:http2-driver*))

(test-assert "protocol-driver-connect! returns connection"
             (with-http2-connection
              (lambda (req res)
                (http-server:response-text! res "ok")
                res)
              (lambda (client accepted conn)
                (http-server:connection? conn))))

(let ()
  (define request-headers
    '((#*":method" #*"GET")
      (#*":scheme" #*"http")
      (#*":path" #*"/hello")
      (#*":authority" #*"localhost")))
  (with-http2-connection
   (lambda (req res)
     (http-server:response-status-set! res 201)
     (http-server:response-header-set! res "x-test" "ok")
     (http-server:response-text! res "created")
     res)
   (lambda (client accepted conn)
     (define payload
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
         (list (cons (make-http2-frame-settings 0 0 '()) #f)
               (cons (make-http2-frame-headers 0 1 #f #f request-headers) #t)))))
     (test-assert "connection accepts request payload"
                  (http-server:connection-process! conn payload))
     (thread-sleep! 0.02)
     (let* ((raw (recv-bytes client))
            (frames (decode-frames raw))
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
            (settings-frame
             (find (lambda (f)
                     (and (http2-frame-settings? f)
                          (not (flag-set? (http2-frame-flags f)
                                          +http2-frame-flag-ack+))))
                   frames))
            (settings-ack-frame
             (find (lambda (f)
                     (and (http2-frame-settings? f)
                          (flag-set? (http2-frame-flags f)
                                     +http2-frame-flag-ack+)))
                   frames)))
       (test-assert "server sends settings" settings-frame)
       (test-assert "server sends settings ack" settings-ack-frame)
       (test-assert "server sends response headers" response-headers-frame)
       (test-assert "server sends response data" response-data-frame)
       (test-equal "response :status"
                   "201"
              (and response-headers-frame
                (header-value (http2-frame-headers-headers response-headers-frame)
                     ":status")))
       (test-equal "response x-test"
                   "ok"
              (and response-headers-frame
                (header-value (http2-frame-headers-headers response-headers-frame)
                     "x-test")))
       (test-equal "response body"
                   "created"
              (and response-data-frame
                (utf8->string (http2-frame-data-data response-data-frame))))))))

(let ()
  (with-http2-connection
   (lambda (req res)
     (http-server:response-text! res "ignored")
     res)
   (lambda (client accepted conn)
     (define ping-data #vu8(1 2 3 4 5 6 7 8))
     (define payload
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
         (list (cons (make-http2-frame-settings 0 0 '()) #f)
               (cons (make-http2-frame-ping 0 0 ping-data) #f)))))
     (test-assert "connection accepts ping payload"
                  (http-server:connection-process! conn payload))
     (thread-sleep! 0.02)
     (let* ((raw (recv-bytes client))
            (frames (decode-frames raw))
            (ping-ack
             (find (lambda (f)
                     (and (http2-frame-ping? f)
                          (flag-set? (http2-frame-flags f)
                                     +http2-frame-flag-ack+)))
                   frames)))
       (test-assert "server replies ping ack" ping-ack)
       (test-equal "ping payload"
                   ping-data
              (and ping-ack
                 (http2-frame-ping-opaque-data ping-ack)))))))

(let ()
  (define invalid-headers
    '((#*"x-before" #*"1")
      (#*":method" #*"GET")
      (#*":scheme" #*"http")
      (#*":path" #*"/bad")
      (#*":authority" #*"localhost")))
  (with-http2-connection
   (lambda (req res)
     (http-server:response-text! res "should not run")
     res)
   (lambda (client accepted conn)
     (define payload
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
         (list (cons (make-http2-frame-settings 0 0 '()) #f)
               (cons (make-http2-frame-headers 0 1 #f #f invalid-headers) #t)))))
     (test-assert "connection accepts invalid-header payload"
                  (http-server:connection-process! conn payload))
     (thread-sleep! 0.02)
     (let* ((raw (recv-bytes client))
            (frames (decode-frames raw))
            (rst (find (lambda (f)
                         (and (http2-frame-rst-stream? f)
                              (= (http2-frame-stream-identifier f) 1)))
                       frames)))
       (test-assert "invalid header triggers RST_STREAM" rst)
       (test-equal "RST_STREAM protocol error"
                   +http2-error-code-protocol-error+
              (and rst
                 (http2-frame-rst-stream-error-code rst)))))))

(let ()
  (define seen-body #f)
  (define request-headers
    '((#*":method" #*"POST")
      (#*":scheme" #*"http")
      (#*":path" #*"/upload")
      (#*":authority" #*"localhost")))
  (with-http2-connection
   (lambda (req res)
     (set! seen-body (utf8->string (http-server:request-body-bytevector req)))
     (http-server:response-text! res "ok")
     res)
   (lambda (client accepted conn)
     (define preface+headers
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
         (list (cons (make-http2-frame-settings 0 0 '()) #f)
               (cons (make-http2-frame-headers 0 1 #f #f request-headers) #f)))))
     (define body-part-1
       (encode-frames (list (cons (make-http2-frame-data 0 1 #*"hello ") #f))))
     (define body-part-2
       (encode-frames (list (cons (make-http2-frame-data 0 1 #*"world") #t))))
     (test-assert "connection accepts split request headers"
                  (http-server:connection-process! conn preface+headers))
     (test-assert "connection accepts request data chunk 1"
                  (http-server:connection-process! conn body-part-1))
     (test-assert "connection accepts request data chunk 2"
                  (http-server:connection-process! conn body-part-2))
     (test-equal "split body is assembled"
                 "hello world"
                  seen-body))))

(let ()
  (define large-request-body (make-bytevector 40000 88))
  (define request-headers
    '((#*":method" #*"POST")
      (#*":scheme" #*"http")
      (#*":path" #*"/bulk")
      (#*":authority" #*"localhost")))
  (with-http2-connection
   (lambda (req res)
     (http-server:response-text! res "ok")
     res)
   (lambda (client accepted conn)
     (define payload
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
         (list (cons (make-http2-frame-settings 0 0 '()) #f)
               (cons (make-http2-frame-headers 0 1 #f #f request-headers) #f)
               (cons (make-http2-frame-data 0 1 large-request-body) #t)))))
     (test-assert "connection accepts large request payload"
                  (http-server:connection-process! conn payload))
     (thread-sleep! 0.02)
     (let* ((raw (recv-bytes client))
            (frames (decode-frames raw))
            (conn-wu
             (find (lambda (f)
                     (and (http2-frame-window-update? f)
                          (= (http2-frame-stream-identifier f) 0)))
                   frames))
            (stream-wu
             (find (lambda (f)
                     (and (http2-frame-window-update? f)
                          (= (http2-frame-stream-identifier f) 1)))
                   frames)))
       (test-assert "connection window update emitted" conn-wu)
       (test-assert "stream window update emitted" stream-wu)
       (test-equal "connection window update increment"
                   40000
                   (and conn-wu
                        (http2-frame-window-update-window-size-increment conn-wu)))
       (test-equal "stream window update increment"
                   40000
                   (and stream-wu
                        (http2-frame-window-update-window-size-increment stream-wu)))))))

(let ()
  (define large-response-body (make-bytevector 70000 65))
  (define request-headers
    '((#*":method" #*"GET")
      (#*":scheme" #*"http")
      (#*":path" #*"/large")
      (#*":authority" #*"localhost")))
  (with-http2-connection
   (lambda (req res)
     (http-server:response-bytes! res large-response-body)
     res)
   (lambda (client accepted conn)
     (define request-payload
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
         (list (cons (make-http2-frame-settings 0 0 '()) #f)
               (cons (make-http2-frame-headers 0 1 #f #f request-headers) #t)))))
     (test-assert "connection accepts large response request"
                  (http-server:connection-process! conn request-payload))
     (thread-sleep! 0.02)
     (let* ((frames-1 (decode-frames (recv-bytes client)))
            (sent-before-update (data-bytes-on-stream frames-1 1)))
       (test-equal "send side is capped by initial peer window"
                   65535
                   sent-before-update)
      (let ((update-payload
        (encode-frames
         (list (cons (make-http2-frame-window-update 0 0 70000) #f)
          (cons (make-http2-frame-window-update 0 1 70000) #f)))))
        (test-assert "connection accepts peer window updates"
           (http-server:connection-process! conn update-payload))
        (thread-sleep! 0.02)
        (let* ((frames-2 (decode-frames (recv-bytes client)))
          (sent-after-update (data-bytes-on-stream frames-2 1))
          (end-frame
           (find (lambda (f)
              (and (http2-frame-data? f)
              (= (http2-frame-stream-identifier f) 1)
              (http2-frame-end-stream? f)))
            frames-2)))
          (test-equal "window update flushes remaining response bytes"
            70000
            (+ sent-before-update sent-after-update))
          (test-assert "final data frame carries END_STREAM" end-frame)))))))

(let ()
  (define request-headers
    '((#*":method" #*"GET")
      (#*":scheme" #*"http")
      (#*":path" #*"/push-default-off")
      (#*":authority" #*"localhost")))
  (with-http2-connection
   (lambda (req res)
     (http-server:response-push! res 'GET "/asset.css" '(("x-push" "1")))
     (http-server:response-text! res "ok")
     res)
   (lambda (client accepted conn)
     (define request-payload
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
         (list (cons (make-http2-frame-settings 0 0 '()) #f)
               (cons (make-http2-frame-headers 0 1 #f #f request-headers) #t)))))
     (test-assert "connection accepts request when push is disabled"
                  (http-server:connection-process! conn request-payload))
     (thread-sleep! 0.02)
     (let* ((frames (decode-frames (recv-bytes client)))
            (push-frame
             (find (lambda (f) (http2-frame-push-promise? f)) frames)))
       (test-assert "push promise is not emitted by default" (not push-frame))))))

(let ()
  (define request-headers
    '((#*":method" #*"GET")
      (#*":scheme" #*"http")
      (#*":path" #*"/index")
      (#*":authority" #*"localhost")))
  (define config (make-http-server-config :http2-enable-push? #t))
  (with-http2-connection
   (lambda (req res)
     (cond ((string=? (http-server:request-path req) "/index")
            (http-server:response-push! res 'GET "/style.css" '(("x-push" "1")))
            (http-server:response-text! res "index")
            res)
           ((string=? (http-server:request-path req) "/style.css")
            ;; pushed requests must not recursively trigger push promises.
            (http-server:response-push! res 'GET "/nested.css" '())
            (http-server:response-text! res "css")
            res)
           (else
            (http-server:response-text! res "other")
            res)))
   (lambda (client accepted conn)
     (define request-payload
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
         (list (cons (make-http2-frame-settings 0 0 '()) #f)
               (cons (make-http2-frame-headers 0 1 #f #f request-headers) #t)))))
     (test-assert "connection accepts request when push is enabled"
                  (http-server:connection-process! conn request-payload))
     (thread-sleep! 0.03)
     (let* ((frames (decode-frames (recv-bytes client)))
            (push-frames (filter http2-frame-push-promise? frames))
            (push-frame (and (pair? push-frames) (car push-frames)))
            (push-id (and push-frame (http2-frame-push-promise-pushed-promise-id push-frame)))
            (main-data
             (find (lambda (f)
                     (and (http2-frame-data? f)
                          (= (http2-frame-stream-identifier f) 1)))
                   frames))
            (pushed-data
             (and push-id
                  (find (lambda (f)
                          (and (http2-frame-data? f)
                               (= (http2-frame-stream-identifier f) push-id)))
                        frames)))
            (nested-push
             (and push-id
                  (find (lambda (f)
                          (and (http2-frame-push-promise? f)
                               (= (http2-frame-stream-identifier f) push-id)))
                        frames))))
       (test-equal "exactly one push promise is emitted"
                   1
                   (length push-frames))
       (test-assert "pushed stream id is even"
                    (and push-id (even? push-id)))
       (test-assert "main response data exists" main-data)
       (test-assert "pushed response data exists" pushed-data)
       (test-equal "main response body"
                   "index"
                   (and main-data
                        (utf8->string (http2-frame-data-data main-data))))
       (test-equal "pushed response body"
                   "css"
                   (and pushed-data
                        (utf8->string (http2-frame-data-data pushed-data))))
       (test-assert "no recursive push from pushed stream" (not nested-push))))
   config))

(let ()
  (define request-headers
    '((#*":method" #*"GET")
      (#*":scheme" #*"http")
      (#*":path" #*"/push-disabled-by-peer")
      (#*":authority" #*"localhost")))
  (define config (make-http-server-config :http2-enable-push? #t))
  (with-http2-connection
   (lambda (req res)
     (http-server:response-push! res 'GET "/asset.css" '())
     (http-server:response-text! res "ok")
     res)
   (lambda (client accepted conn)
     (define request-payload
       (bytevector-append
        +http2-connection-preface+
        (encode-frames
           (list (cons (make-http2-frame-settings 0 0 `((,+http2-settings-enable-push+ 0))) #f)
               (cons (make-http2-frame-headers 0 1 #f #f request-headers) #t)))))
     (test-assert "connection accepts request with peer push disabled"
                  (http-server:connection-process! conn request-payload))
     (thread-sleep! 0.02)
     (let* ((frames (decode-frames (recv-bytes client)))
            (push-frame
             (find (lambda (f) (http2-frame-push-promise? f)) frames)))
       (test-assert "push promise is suppressed by peer settings" (not push-frame))))
   config))

(test-end)
