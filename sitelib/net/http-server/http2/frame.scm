;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http2/state.scm - HTTP/2 server driver frame
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
(library (net http-server http2 frame)
    (export frame-flag-set?
	    decode-priority-dependency
	    ensure-stream-id-valid
	    apply-peer-settings!
	    dispatch-frame!
	    dispatch-application!)
    (import (rnrs)
	    (net http-server protocol)
	    (net http-server request)
	    (net http-server response)
	    (net http-server types)
	    (rfc http2 frame)
	    (rfc http2 conditions)
	    (rfc http2 hpack)
	    (rfc http2 priority)
	    (util bytevector)
	    (net http-server http2 const)
	    (net http-server http2 state)
	    (net http-server http2 headers)
	    (net http-server http2 output))

(define (frame-flag-set? flags mask)
  (not (zero? (bitwise-and flags mask))))

(define (decode-priority-dependency raw)
  (if (and raw (integer? raw))
      (values (bitwise-and raw #x7fffffff)
              (not (zero? (bitwise-and raw #x80000000))))
      (values 0 #f)))

(define (ensure-stream-id-valid conn sid)
  (when (or (zero? sid) (even? sid)
            (<= sid (http2-server-connection-state-last-stream-id conn)))
    (http2-protocol-error 'http2-dispatch-frame!
                          "Invalid client stream identifier"
                          sid)))

(define (apply-peer-settings! conn settings)
  (for-each
   (lambda (kv)
     (let ((id (car kv))
           (value (cdr kv)))
       (cond
        ((= id +http2-settings-header-table-size+)
         (set-hpack-table-size-limit!
          (http2-server-connection-state-encoder-context conn)
          value)
         (update-hpack-table-size!
          (http2-server-connection-state-encoder-context conn)
          value))
        ((= id +http2-settings-enable-push+)
         (http2-server-connection-state-remote-enable-push?-set!
          conn
          (not (zero? value))))
        ((= id +http2-settings-initial-window-size+)
         (when (> value (- (expt 2 31) 1))
           (http2-protocol-error 'apply-peer-settings!
                                 "Invalid initial window size" value))
         (let* ((w (http2-server-connection-state-remote-initial-window-size conn))
		(delta (- value w)))
           (for-each-stream
            conn
            (lambda (stream)
              (http2-server-stream-send-window-set!
               stream
               (+ (http2-server-stream-send-window stream) delta)))))
         (http2-server-connection-state-remote-initial-window-size-set!
          conn
          value)
         (flush-pending-output! conn))
        ((= id +http2-settings-max-frame-size+)
         (when (or (< value +http2-initial-frame-buffer-size+)
                   (> value +http2-max-frame-buffer-size+))
           (http2-protocol-error 'apply-peer-settings!
                                 "Invalid max frame size"
                                 value))
         (http2-server-connection-state-remote-max-frame-size-set! conn value)
         (update-frame-buffer! (http2-server-connection-state-write-buffer conn)
                               value)))))
   settings))

;; dispatch frame
(define (dispatch-frame! conn frame)
  (define sid (http2-frame-stream-identifier frame))
  (define state (http-server:http-connection-parse-state conn))
  (cond
   ((http2-frame-settings? frame)
    (if (frame-flag-set? (http2-frame-flags frame) +http2-frame-flag-ack+)
        #t
        (begin
          (apply-peer-settings! state (http2-frame-settings-settings frame))
          (send-frame! state
                       (make-http2-frame-settings +http2-frame-flag-ack+ 0 '())
                       #f)
          (flush-pending-output! state)
          #t)))

   ((http2-frame-ping? frame)
    (if (frame-flag-set? (http2-frame-flags frame) +http2-frame-flag-ack+)
        #t
        (begin
          (send-frame! state
                       (make-http2-frame-ping
                        +http2-frame-flag-ack+
                        0
                        (http2-frame-ping-opaque-data frame))
                       #f)
          #t)))

   ((http2-frame-window-update? frame)
    (let ((increment
           (bitwise-and
            (http2-frame-window-update-window-size-increment frame)
            #x7fffffff)))
      (when (zero? increment)
        (http2-protocol-error 'dispatch-frame!
                              "WINDOW_UPDATE increment must not be zero"
                              sid))
      (if (zero? sid)
          (http2-server-connection-state-connection-send-window-set!
	   state
           (+ increment (http2-server-connection-state-connection-send-window state)))
          (let ((stream (find-stream state sid)))
            (when stream
              (http2-server-stream-send-window-set!
               stream
               (+ increment (http2-server-stream-send-window stream))))))
      (flush-pending-output! state)
      #t))

   ((http2-frame-goaway? frame)
    (http2-server-connection-state-stage-set! state 'closing)
    #f)

   ((http2-frame-rst-stream? frame)
    (drop-stream! state sid)
    #t)

   ((http2-frame-priority? frame)
    (let-values (((dependency exclusive?)
                  (decode-priority-dependency
                   (http2-frame-priority-stream-dependency frame))))
      (let ((stream (find-stream state sid))
	    (tree (http2-server-connection-state-priority-tree state)))
        (http2-priority-tree-add! tree
                                  sid
                                  dependency
                                  (http2-frame-priority-weight frame)
                                  exclusive?
                                  (not stream))
        (when stream
          (http2-server-stream-priority-dependency-set! stream dependency)
          (http2-server-stream-priority-weight-set!
           stream
           (http2-frame-priority-weight frame)))))
    #t)

   ((http2-frame-headers? frame) (handle-header-frame! state frame))
   ((http2-frame-data? frame) (handle-data-frame! state frame))
   ((http2-frame-continuation? frame)
    (http2-protocol-error 'dispatch-frame!
                          "Unexpected CONTINUATION frame" sid))

   (else #t)))


(define (stream->request state stream)
  (let ((body (bytevector-concatenate
               (reverse (http2-server-stream-body-chunks stream)))))
    (make-http-server:http2-request
     (http2-server-stream-method stream)
     (http2-server-stream-target stream)
     (http2-server-stream-path stream)
     (http2-server-stream-query stream)
     (http2-server-stream-headers stream)
     body
     (http2-server-stream-id stream))))

(define (dispatch-application! state stream)
  (let* ((req (stream->request state stream))
         (res (make-http-server:response))
         (app-handler (http2-server-connection-state-app-handler state))
         (result
          (guard (e (else
                     (let ((er (make-http-server:response 500)))
                       (http-server:response-text!
                        er
                        "Unhandled application error")
                       er)))
            (let ((r (app-handler req res)))
              (if (http-server:response? r) r res)))))
    (write-stream-response! state stream req result)
    #t))

;; helper
(define (handle-header-frame! state frame)
  (define sid (http2-frame-stream-identifier frame))

  (ensure-stream-id-valid state sid)
  (when (find-stream state sid)
    (http2-protocol-error 'dispatch-frame!
                          "HEADERS received for existing stream"
                          sid))
  (let-values (((method target path query header-map err)
                (parse-http2-request-headers
                 (http2-frame-headers-headers frame))))
    (if err
        (begin
          (send-rst-stream! state sid +http2-error-code-protocol-error+)
          #t)
        (let-values (((dependency exclusive?)
                      (decode-priority-dependency
                       (http2-frame-headers-stream-dependency frame))))
          (let* ((wire-weight
                  (or (http2-frame-headers-weight frame)
                      +default-priority-wire-weight+))
                 (stream
                  (make-http2-server-stream
                   sid
                   method
                   target
                   path
                   query
                   header-map
                   '()
                   0
                   (http2-server-connection-state-remote-initial-window-size state)
                   +default-initial-window-size+
                   0
                   #vu8()
                   #f
                   dependency
                   wire-weight)))
            (http2-priority-tree-add!
             (http2-server-connection-state-priority-tree state)
             sid
             dependency
             wire-weight
             exclusive?
             #f)
            (register-stream! state stream)
            (if (http2-frame-end-stream? frame)
                (dispatch-application! state stream)
                #t))))))

(define (handle-data-frame! state frame)
  (define sid (http2-frame-stream-identifier frame))

  (let ((stream (find-stream state sid)))
    (if (not stream)
        (begin
          (send-rst-stream! state sid +http2-error-code-stream-closed+)
          #t)
        (let* ((data (http2-frame-data-data frame))
               (received (bytevector-length data))
               (conn-recv-window
                (http2-server-connection-state-connection-recv-window state))
               (stream-recv-window (http2-server-stream-recv-window stream))
               (new-size (+ (http2-server-stream-body-size stream)
                            received))
	       (config (http2-server-connection-state-config state))
               (max-body (http-config-max-body-bytes config)))
          (when (or (> received conn-recv-window)
                    (> received stream-recv-window))
            (http2-flow-control-error 'dispatch-frame!
                                      "DATA exceeds flow control window"
                                      sid))
          (http2-server-connection-state-connection-recv-window-set!
           state
           (- conn-recv-window received))
          (http2-server-stream-recv-window-set!
           stream
           (- stream-recv-window received))
          (http2-server-connection-state-connection-recv-consumed-set!
           state
           (+ (http2-server-connection-state-connection-recv-consumed state)
              received))
          (http2-server-stream-recv-consumed-set!
           stream
           (+ (http2-server-stream-recv-consumed stream)
              received))
          (if (> new-size max-body)
              (begin
                (send-rst-stream! state sid +http2-error-code-enhance-your-calm+)
                #t)
              (begin
                (http2-server-stream-body-size-set! stream new-size)
                (http2-server-stream-body-chunks-set!
                 stream
                 (cons data (http2-server-stream-body-chunks stream)))
                (maybe-send-connection-window-update! state)
                (maybe-send-stream-window-update! state stream)
                (if (http2-frame-end-stream? frame)
                    (dispatch-application! state stream)
                    #t)))))))
)
