;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/h2c.scm - h2c upgrade and prior-knowledge handling
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
#!read-macro=sagittarius/bv-string
(library (net http-server h2c)
  (export http-server:register-h2c-upgrade-handler!
          http-server:http-connection-check-prior-knowledge!)
  (import (rnrs)
          (clos user)
          (net socket)
          (net http-server types)
          (net http-server request)
          (net http-server response)
          (net http-server http1)
          (net http-server http2)
          (net http-server protocol)
          (net http-server upgrade)
          (rfc base64)
          (rfc http2 frame))

(define (config-ref config key default)
  (guard (e (else default))
    (slot-ref config key)))

(define (http-server:register-h2c-upgrade-handler! registry)
  (unless (http-server:upgrade-handler-registered? registry "h2c")
    (http-server:register-upgrade-handler! registry "h2c" h2c-upgrade-handler))
  registry)

(define (http2-preface-status buffer)
  (let ((n (bytevector-length buffer))
        (m (bytevector-length +http2-connection-preface+)))
    (let loop ((i 0))
      (cond ((= i m) 'full)
            ((= i n) 'partial)
            ((= (bytevector-u8-ref buffer i)
                (bytevector-u8-ref +http2-connection-preface+ i))
             (loop (+ i 1)))
            (else 'mismatch)))))

(define (write-h2c-switching-protocols! socket)
  (socket-send socket
   #*"HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: h2c\r\n\r\n"))

(define (decode-http2-settings-value value)
  (define (parse-settings-payload bv)
    (let ((size (bytevector-length bv)))
      (unless (zero? (mod size 6))
        (assertion-violation 'decode-http2-settings-value
                             "Malformed HTTP2-Settings payload"
                             value))
      (let loop ((i 0) (r '()))
        (if (= i size)
            (reverse r)
            (let ((id (bytevector-u16-ref bv i (endianness big)))
                  (v (bytevector-u32-ref bv (+ i 2) (endianness big))))
              (loop (+ i 6) (cons (cons id v) r)))))))
  (parse-settings-payload (base64url-decode-string value :transcoder #f)))

(define (make-http2-connection-from-http1 conn app-handler . opts)
  (let* ((server (http-server:connection-server conn))
	 (socket (http-server:connection-socket conn)))
    (apply make-http-server:http2-connection server socket app-handler opts)))

(define (h2c-upgrade-settings req)
  (define headers (http-server:request-headers req))
  (and (http-server:headers-contains-token? headers "connection" "upgrade")
       (http-server:headers-contains-token? headers "connection" "http2-settings")
       (http-server:headers-contains-token? headers "upgrade" "h2c")
       (http-server:request-header-ref req "http2-settings" #f)))

;; Handler contract: returns status, new-connection, keep-open?
;; status is one of 'declined, 'handled, 'error.
(define (h2c-upgrade-handler conn req remainder app-handler)
  (let* ((server (http-server:connection-server conn))
         (socket (http-server:connection-socket conn))
         (config (slot-ref server 'config))
         (driver (http-server:http-connection-driver conn)))
    (cond ((not (http-server:http1-connection? conn))
           (values 'declined conn #t))
          ((not (config-ref config 'http2-cleartext? #f))
           (values 'declined conn #t))
          ((not (config-ref config 'http2? #t))
           (values 'declined conn #t))
          ((tls-socket? socket)
           (values 'declined conn #t))
          ((not (h2c-upgrade-settings req))
           (values 'declined conn #t))
          (else
           (guard (e (else
                      (let ((res (make-http-server:response 400)))
                        (http-server:response-text! res "Malformed HTTP2-Settings")
                        (http-server:protocol-driver-serve! driver conn req res)
                        (values 'error conn #f))))
             (let* ((h (http-server:request-header-ref req "http2-settings" #f))
		    (settings (decode-http2-settings-value h)))
               (write-h2c-switching-protocols! socket)
               (let ((new-conn
                      (make-http2-connection-from-http1
                       conn
                       app-handler
                       :settings settings
                       :upgrade-request req
                       :expect-preface? #f)))
                 (http-server:http-connection-request-count-set!
                  new-conn
                  (+ 1 (http-server:http-connection-request-count conn)))
                 (if (http-server:connection-process! new-conn remainder)
                     (values 'handled new-conn #t)
                     (values 'handled new-conn #f)))))))))

;; Returns 3 values: status, new-connection, keep-open?
;; status is one of 'none, 'wait, 'handled.
(define (http-server:http-connection-check-prior-knowledge! conn app-handler)
  (let* ((server (http-server:connection-server conn))
         (socket (http-server:connection-socket conn))
         (config (slot-ref server 'config)))
    (if (and (http-server:http1-connection? conn)
             (config-ref config 'http2-cleartext? #f)
             (config-ref config 'http2? #t)
             (not (tls-socket? socket))
             (zero? (http-server:http-connection-request-count conn))
             (eq? (http-server:http-connection-driver conn)
                  *http-server:http1-driver*))
        (case (http2-preface-status (http-server:http-connection-buffer conn))
          ((partial) (values 'wait conn #t))
          ((full)
           (let ((chunk (http-server:http-connection-buffer conn))
		 (new-conn
                  (make-http2-connection-from-http1 conn app-handler)))
             (http-server:http-connection-buffer-set! new-conn #vu8())
             (if (http-server:connection-process! new-conn chunk)
                 (values 'handled new-conn #t)
                 (values 'handled new-conn #f))))
          (else
           (values 'none conn #t)))
        (values 'none conn #t))))
)
