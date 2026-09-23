;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http2.scm - HTTP/2 server driver
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
(library (net http-server http2)
    (export http-server:http2-request?
	    http-server:http2-request-stream-id
	    http-server:http2-connection?
	    make-http-server:http2-connection
	    *http-server:http2-driver*

	    <http2-config> http2-config? make-http2-config
	    http2-config-max-concurrent-streams
	    http2-config-enable-push?)
    (import (rnrs)
	    (clos user)
	    (srfi :18)
	    (net server)
	    (net http-server protocol)
	    (net http-server request)
	    (net http-server types)
	    (rfc http2 frame)
	    (rfc http2 conditions)
	    (rfc http2 hpack)
	    (rfc http2 priority)
	    (util bytevector)
	    (net http-server http2 state)
	    (net http-server http2 const)
	    (net http-server http2 headers)
	    (net http-server http2 output)
	    (net http-server http2 frame))

(define-record-type (http-server:http2-connection
                     %make-http-server:http2-connection
                     http-server:http2-connection?)
  (parent http-server:http-connection)
  (protocol (lambda (n)
	      (lambda (server socket process close state driver)
		((n server socket process close state driver))))))

(define (request->upgrade-stream conn req stream-id)
  (let ((wire-weight +default-priority-wire-weight+)
        (dependency 0)
        (body (http-server:request-body-bytevector req)))
    (http2-priority-tree-add!
     (http2-server-connection-state-priority-tree conn)
     stream-id
     dependency
     wire-weight
     #f
     #f)
    (make-http2-server-stream
     stream-id
     (http-server:request-method req)
     (http-server:request-target req)
     (http-server:request-path req)
     (http-server:request-query req)
     (make-http-server:headers
      (http-server:headers->alist (http-server:request-headers req)))
     (if (and body (> (bytevector-length body) 0))
         (list body)
         '())
     (if body (bytevector-length body) 0)
     (http2-server-connection-state-remote-initial-window-size conn)
     +default-initial-window-size+
     0
     #vu8()
     #f
     dependency
     wire-weight)))

(define (replay-upgrade-request! conn req)
  (let ((stream (request->upgrade-stream conn req 1)))
    (register-stream! conn stream)
    (dispatch-application! conn stream)))

(define (preface-prefix-matches? pending)
  (let ((n (bytevector-length pending))
        (m (bytevector-length +http2-connection-preface+)))
    (let loop ((i 0))
  (cond ((= i n) #t)
    ((= i m) #t)
            ((= (bytevector-u8-ref pending i)
                (bytevector-u8-ref +http2-connection-preface+ i))
             (loop (+ i 1)))
            (else #f)))))

(define (send-initial-settings! conn)
  (define config (http2-server-connection-state-config conn))
  (let ((max-header-bytes (http-config-max-header-bytes config))
        (max-concurrent-streams (http2-config-max-concurrent-streams config)))
    (send-frame! conn
                 (make-http2-frame-settings
                  0
                  0
                  `((,+http2-settings-enable-push+ 0)
                    (,+http2-settings-header-table-size+
                     ,+default-header-table-size+)
                    (,+http2-settings-max-concurrent-streams+
                     ,max-concurrent-streams)
                    (,+http2-settings-initial-window-size+
                     ,+http2-default-window-size+)
                    (,+http2-settings-max-frame-size+
                     ,+http2-initial-frame-buffer-size+)
                    (,+http2-settings-max-header-list-size+
                     ,max-header-bytes)))
                 #f)))

(define (send-settings-ack! conn)
  (send-frame! conn (make-http2-frame-settings +http2-frame-flag-ack+ 0 '()) #f))

(define (consume-preface! conn)
  (let* ((pending (http2-server-connection-state-pending conn))
         (plen (bytevector-length +http2-connection-preface+))
         (n (bytevector-length pending)))
    (unless (preface-prefix-matches? pending)
      (http2-protocol-error 'consume-preface! "Invalid HTTP/2 preface"))
    (when (>= n plen)
      (let ((prefix (bytevector-copy pending 0 plen)))
        (unless (bytevector=? prefix +http2-connection-preface+)
          (http2-protocol-error 'consume-preface! "Invalid HTTP/2 preface"))
        (http2-server-connection-state-pending-set!
         conn
         (bytevector-copy pending plen n))
        (send-initial-settings! conn)
        (http2-server-connection-state-stage-set! conn 'ready)))))

(define (process-ready! conn state)
  (let loop ()
    (let ((pending (http2-server-connection-state-pending state)))
      (or (zero? (bytevector-length pending))
          (let ((end (http2-frame-block-end
                      pending
                      0
                      (http2-server-connection-state-local-max-frame-size state))))
            (or (not end)
                (let ((frame-bytes (bytevector-copy pending 0 end))
                      (next (bytevector-copy pending end (bytevector-length pending))))
                  (http2-server-connection-state-pending-set! state next)
                  (let ((frame
                         (read-http2-frame
                          (open-bytevector-input-port frame-bytes)
                          (http2-server-connection-state-read-buffer state)
                          (http2-server-connection-state-decoder-context state))))
                    (if (dispatch-frame! conn frame)
                        (loop)
                        #f)))))))))

(define (process-connection! conn chunk)
  (define state (http-server:http-connection-parse-state conn))
  (guard (e ((http2-error? e)
             (send-goaway! state (http2-error-code e) (condition-message e)))
            (else
             (send-goaway! state +http2-error-code-internal-error+
                           (condition-message e))))
    (when (connection-closed? state)
      (http2-protocol-error 'process-connection!
                            "Connection closed" conn))
    (when (not (bytevector? chunk))
      (http2-protocol-error 'process-connection!
                            "Bytevector chunk required" chunk))
    (when (positive? (bytevector-length chunk))
      (http2-server-connection-state-pending-set!
       state
       (bytevector-append (http2-server-connection-state-pending state)
                          chunk)))

    (when (eq? (http2-server-connection-state-stage state) 'await-preface)
      (consume-preface! state))

    (if (eq? (http2-server-connection-state-stage state) 'ready)
        (let ((result (process-ready! conn state)))
          (and result (flush-pending-output! state)))
        #t)))

(define (make-http-server:http2-connection server socket app-handler
                                           :key
                                           (settings '())
                                           (upgrade-request #f)
                                           (expect-preface? #t))
  (define (driver-config server)
    (define registry  (slot-ref server 'protocol-registry))
    (define driver *http-server:http2-driver*)
    (cond ((http-server:protocol-registry-driver-config registry driver))
	  (else (make-http2-config))))
  
  (let* ((config (driver-config server))
	 (max-concurrent-streams (http2-config-max-concurrent-streams config))
         (priority-node-cap (+ 5 (* 2 max-concurrent-streams)))
         (decoder (make-hpack-context +default-header-table-size+))
         (encoder (make-hpack-context +default-header-table-size+))
         (state (make-http2-server-connection-state
                 socket
                 config
                 app-handler
                 (make-eqv-hashtable)
                 decoder
                 encoder
                 (make-frame-buffer)
                 (make-frame-buffer)
                 (make-mutex)
                 #vu8()
                 (if expect-preface? 'await-preface 'ready)
                 0
                 2
                 +default-max-frame-size+
                 +default-max-frame-size+
                 #t
                 +default-initial-window-size+
                 +default-initial-window-size+
                 +default-initial-window-size+
                 0
                 (make-http2-priority-tree priority-node-cap))))
    (unless expect-preface?
      (send-initial-settings! state))
    (unless (null? settings)
      (apply-peer-settings! state settings)
      (send-settings-ack! state))
    (when upgrade-request
      (replay-upgrade-request! state upgrade-request)
      (flush-pending-output! state))
    (%make-http-server:http2-connection
     server socket
     (lambda (conn chunk) (process-connection! conn chunk))
     (lambda (conn) (close-connection! conn) #t)
     state
     *http-server:http2-driver*)))

(define (http-server:http2-connect server socket app-handler . opts)
  (apply make-http-server:http2-connection server socket app-handler opts))

(define (http-server:http2-consume conn)
  (let ((chunk (http-server:http-connection-buffer conn)))
    (http-server:http-connection-buffer-set! conn #vu8())
    (process-connection! conn chunk)))

(define (http-server:http2-serve socket req result)
  #t)

(define *http-server:http2-driver*
  (make-http-server:protocol-driver
   "h2"
   http-server:http2-consume
   http-server:http2-serve
   http-server:http2-connect))

)
