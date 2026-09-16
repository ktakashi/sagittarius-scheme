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

(define (with-http2-connection app proc)
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
                   accepted
                   (make-http-server-config)
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

(test-end)
