;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http2/state.scm - HTTP/2 server driver state
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
#!read-macro=sagittarius/bv-string
(library (net http-server http2 output)
    (export with-send-lock
	    send-frame!
	    send-goaway!
	    send-rst-stream!

	    flush-pending-output!
	    maybe-send-connection-window-update!
	    maybe-send-stream-window-update!

	    write-stream-response!
	    emit-push-responses!
	    push-enabled?
	    allocate-push-stream-id!
	    make-push-stream
	    prepare-pushed-request
	    dispatch-pushed-application!)
    (import (rnrs)
	    (srfi :18)
	    (net socket)
	    (net http-server request)
	    (net http-server response)
	    (net http-server types)
	    (rfc http2 frame)
	    (rfc http2 conditions)
	    (rfc http2 priority)
	    (util bytevector)
	    (net http-server http2 const)
	    (net http-server http2 state)
	    (net http-server http2 headers))

(define (with-send-lock conn proc)
  (let ((mutex (http2-server-connection-state-mutex conn)))
    (dynamic-wind
      (lambda () (mutex-lock! mutex))
      proc
      (lambda () (mutex-unlock! mutex)))))

(define (send-frame! conn frame end?)
  (define out-bv
    (with-send-lock
        conn
      (lambda ()
        (call-with-bytevector-output-port
         (lambda (out)
           (write-http2-frame out
	    (http2-server-connection-state-write-buffer conn)
	    frame
	    end?
	    (http2-server-connection-state-encoder-context conn)))))))
  (socket-send (http2-server-connection-state-socket conn) out-bv))


(define (send-goaway! conn code message)
  (define state (->connection-state conn))
  (unless (connection-closed? state)
    (let ((debug (if message (string->utf8 message) #vu8())))
      (guard (e (else #f))
        (send-frame! state
                     (make-http2-frame-goaway
                      0
                      0
                      (http2-server-connection-state-last-stream-id state)
                      code
                      debug)
                     #t)))
    (close-connection! state))
  #f)

(define (send-rst-stream! conn sid code)
  (drop-stream! conn sid)
  (send-frame! conn (make-http2-frame-rst-stream 0 sid code) #f))

;; flow control
(define (maybe-send-connection-window-update! conn)
  (let ((consumed (http2-server-connection-state-connection-recv-consumed conn))
        (threshold (div +default-initial-window-size+ 2)))
    (when (and (> consumed 0) (>= consumed threshold))
      (http2-server-connection-state-connection-recv-consumed-set! conn 0)
      (http2-server-connection-state-connection-recv-window-set!
       conn
       (+ (http2-server-connection-state-connection-recv-window conn) consumed))
      (send-frame! conn (make-http2-frame-window-update 0 0 consumed) #f))))

(define (maybe-send-stream-window-update! conn stream)
  (let ((consumed (http2-server-stream-recv-consumed stream))
        (threshold (div +default-initial-window-size+ 2)))
    (when (and (> consumed 0) (>= consumed threshold))
      (http2-server-stream-recv-consumed-set! stream 0)
      (http2-server-stream-recv-window-set!
       stream
       (+ (http2-server-stream-recv-window stream) consumed))
      (send-frame! conn
                   (make-http2-frame-window-update
                    0
                    (http2-server-stream-id stream)
                    consumed)
                   #f))))

;; write response
(define (write-stream-response! conn stream req res)
  (emit-push-responses! conn stream req res)
  (let-values (((headers body skip-body?) (response->hpack-headers req res)))
    (define sid (http2-server-stream-id stream))
    (send-frame! conn
                 (make-http2-frame-headers 0 sid #f #f headers)
                 (or skip-body? (zero? (bytevector-length body))))
    (if (or skip-body? (zero? (bytevector-length body)))
        (drop-stream! conn sid)
        (begin
          (http2-server-stream-pending-output-set! stream body)
          (http2-server-stream-pending-end-stream?-set! stream #t)
          (flush-pending-output! conn)))))


;; push
(define (push-enabled? conn req)
  (and req
       (http-server:http2-request? req)
       (odd? (http-server:http2-request-stream-id req))
       (http2-config-enable-push? (http2-server-connection-state-config conn))
       (http2-server-connection-state-remote-enable-push? conn)))

(define (allocate-push-stream-id! conn)
  (let ((sid (http2-server-connection-state-next-push-stream-id conn)))
    (if (> sid #x7fffffff)
        #f
        (begin
          (http2-server-connection-state-next-push-stream-id-set! conn (+ sid 2))
          sid))))

(define (prepare-pushed-request state parent-req stream-id method target headers)
  (define socket (http2-server-connection-state-socket state))
  (define scheme (if (tls-socket? socket) "https" "http"))
  (define authority
    (or (http-server:headers-ref headers "host" #f)
        (and parent-req (http-server:request-header-ref parent-req "host" #f))
        "localhost"))
  (define method-token (method->http-token method))
  (define push-headers 
    (make-http-server:headers (http-server:headers->alist headers)))
  (unless (http-server:headers-ref push-headers "host" #f)
    (http-server:headers-add! push-headers "host" authority))
  (let-values (((path query) (parse-target target)))
    (let ((req (make-http-server:http2-request
                (string->symbol method-token)
                target
                path
                query
                push-headers
                #vu8()
                stream-id)))
      (values req
              (append `((#*":method"    ,(string->utf8 method-token))
                        (#*":scheme"    ,(string->utf8 scheme))
                        (#*":authority" ,(string->utf8 authority))
                        (#*":path"      ,(string->utf8 target)))
                      (collect-push-headers push-headers))))))

(define (make-push-stream conn stream-id parent-id)
  (let ((wire-weight +default-priority-wire-weight+)
        (dependency parent-id))
    (http2-priority-tree-add!
     (http2-server-connection-state-priority-tree conn)
     stream-id
     dependency
     wire-weight
     #f
     #f)
    (register-stream!
     conn
     (make-http2-server-stream
      stream-id
      'GET
      "/"
      "/"
      #f
      (make-http-server:headers)
      '()
      0
      (http2-server-connection-state-remote-initial-window-size conn)
      +default-initial-window-size+
      0
      #vu8()
      #f
      dependency
      wire-weight))))

(define (dispatch-pushed-application! conn stream req)
  (let* ((res (make-http-server:response))
         (app-handler (http2-server-connection-state-app-handler conn))
         (result
          (guard (e (else
                     (let ((er (make-http-server:response 500)))
                       (http-server:response-text!
                        er
                        "Unhandled application error")
                       er)))
            (let ((r (app-handler req res)))
              (if (http-server:response? r) r res)))))
    (write-stream-response! conn stream req result)
    #t))

(define (emit-push-responses! state stream req res)
  (when (and (push-enabled? state req)
             (pair? (http-server:response-pushes res)))
    (for-each
     (lambda (push)
       (let ((method (car push))
             (target (cadr push))
             (headers (caddr push)))
         (let ((push-id (allocate-push-stream-id! state)))
           (when push-id
             (let-values (((push-req promise-headers)
                           (prepare-pushed-request state req push-id method target headers)))
               (let ((push-stream (make-push-stream state
                                   push-id (http2-server-stream-id stream))))
                 (send-frame! state
                              (make-http2-frame-push-promise
                               0
                               (http2-server-stream-id stream)
                               push-id
                               promise-headers)
                              #f)
                 (dispatch-pushed-application! state push-stream push-req)))))))
     (http-server:response-pushes res))))

;; priority scheduler
(define (flush-pending-output! conn)
  (define (stream-active? sid)
    (let ((stream (find-stream conn sid)))
      (and stream
           (> (bytevector-length (http2-server-stream-pending-output stream)) 0)
           (> (http2-server-stream-send-window stream) 0)
           (> (http2-server-connection-state-connection-send-window conn) 0))))

  (let loop ()
    (let* ((tree (http2-server-connection-state-priority-tree conn))
           (sid (http2-priority-tree-schedule tree stream-active?)))
      (or (not sid)
          (let ((stream (find-stream conn sid)))
            (if (not stream)
                (begin
                  (http2-priority-tree-remove! tree sid)
                  (loop))
                (let* ((pending (http2-server-stream-pending-output stream))
                       (pending-size (bytevector-length pending))
                       (send-size (min pending-size
                                       (http2-server-stream-send-window stream)
                                       (http2-server-connection-state-connection-send-window conn)
                                       (http2-server-connection-state-remote-max-frame-size conn))))
                  (or (<= send-size 0)
                      (let-values (((chunk remain)
				    (bytevector-split-at* pending send-size)))
                        (let ((end? (and (zero? (bytevector-length remain))
                                         (http2-server-stream-pending-end-stream? stream))))
                          (send-frame! conn (make-http2-frame-data 0 sid chunk) end?)
                          (http2-server-stream-pending-output-set! stream remain)
                          (http2-server-connection-state-connection-send-window-set!
                           conn
                           (- (http2-server-connection-state-connection-send-window conn)
                              send-size))
                          (http2-server-stream-send-window-set!
                           stream
                           (- (http2-server-stream-send-window stream) send-size))
                          (http2-priority-tree-account! tree sid send-size)
                          (when end?
                            (drop-stream! conn sid))
                          (loop)))))))))))
)
