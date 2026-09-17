;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http2.scm - HTTP/2 server driver
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
#!read-macro=sagittarius/bv-string
(library (net http-server http2)
    (export http-server:http2-request?
	    http-server:http2-request-stream-id
	    http-server:http2-connection?
	    make-http-server:http2-upgrade-connection
	    make-http-server:http2-connection
	    *http-server:http2-driver*)
    (import (rnrs)
	    (clos user)
	    (srfi :1)
	    (srfi :18)
	    (net socket)
	    (net http-server protocol)
	    (only (net http-server upgrade)
		  http-server:http-connection)
	    (net http-server request)
	    (net http-server response)
	    (net http-server types)
	    (rfc http2 frame)
	    (rfc http2 conditions)
	    (rfc http2 hpack)
	    (rfc http2 priority)
	    (util bytevector))

(define-record-type (http-server:http2-connection
                     make-http-server:http2-upgrade-connection
                     http-server:http2-connection?)
  (parent http-server:http-connection))

(define +default-max-header-bytes+ 65536)
(define +default-max-body-bytes+ 1048576)
(define +default-max-concurrent-streams+ 100)
(define +default-header-table-size+ 4096)
(define +default-initial-window-size+ +http2-default-window-size+)
(define +default-max-frame-size+ +http2-initial-frame-buffer-size+)
(define +default-priority-wire-weight+ 15)

(define (frame-flag-set? flags mask)
  (not (zero? (bitwise-and flags mask))))

(define-record-type http-server:http2-request
  (parent http-server:request)
  (fields stream-id)
  (protocol
   (lambda (p)
     (lambda (method target path query headers body stream-id)
       ((p method target path query "HTTP/2" headers body #f '()) stream-id)))))

(define-record-type http2-server-stream
  (fields id
          method
          target
          path
          query
          headers
          (mutable body-chunks)
          (mutable body-size)
          (mutable send-window)
          (mutable recv-window)
          (mutable recv-consumed)
          (mutable pending-output)
          (mutable pending-end-stream?)
          (mutable priority-dependency)
          (mutable priority-weight)))

(define-record-type http2-server-connection-state
  (fields socket
          config
          app-handler
          streams
          decoder-context
          encoder-context
          read-buffer
          write-buffer
          mutex
          (mutable pending)
          (mutable stage)
          (mutable last-stream-id)
          (mutable next-push-stream-id)
          (mutable local-max-frame-size)
          (mutable remote-max-frame-size)
          (mutable remote-enable-push?)
          (mutable remote-initial-window-size)
          (mutable connection-send-window)
          (mutable connection-recv-window)
          (mutable connection-recv-consumed)
          priority-tree))

(define (config-ref config name default)
  (guard (e (else default))
    (slot-ref config name)))

(define (connection-closed? conn)
  (eq? (http2-server-connection-state-stage conn) 'closed))

(define (close-connection! conn)
  (unless (connection-closed? conn)
    (http2-server-connection-state-stage-set! conn 'closed)
    (guard (e (else #f))
      (socket-close (http2-server-connection-state-socket conn)))))

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
  (unless (connection-closed? conn)
    (let ((debug (if message (string->utf8 message) #vu8())))
      (guard (e (else #f))
        (send-frame! conn
                     (make-http2-frame-goaway
                      0
                      0
                      (http2-server-connection-state-last-stream-id conn)
                      code
                      debug)
                     #t)))
    (close-connection! conn))
  #f)

(define (split-once s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((= i len) (values s #f))
            ((char=? (string-ref s i) ch)
             (values (substring s 0 i) (substring s (+ i 1) len)))
            (else
             (loop (+ i 1)))))))

(define (parse-target target)
  (let-values (((path query) (split-once target #\?)))
    (values (if (string=? path "") "/" path) query)))

(define (contains-uppercase-ascii? s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (and (< i n)
           (let ((c (string-ref s i)))
             (or (and (char>=? c #\A) (char<=? c #\Z))
                 (loop (+ i 1))))))))

(define (connection-specific-header? name)
  (or (string=? name "connection")
      (string=? name "keep-alive")
      (string=? name "proxy-connection")
      (string=? name "transfer-encoding")
      (string=? name "upgrade")))

(define (entry->name&value e)
  (let ((name (and (pair? e) (car e)))
        (value (and (pair? e) (pair? (cdr e)) (cadr e))))
    (if (and (bytevector? name) (bytevector? value))
        (values name value)
        (values #f #f))))

(define (decode-header-entry e)
  (let-values (((name value) (entry->name&value e)))
    (if (not name)
        (values #f #f "Malformed HPACK entry")
        (guard (ex (else (values #f #f "Malformed UTF-8 in header")))
          (values (utf8->string name) (utf8->string value) #f)))))

(define (parse-http2-request-headers headers)
  (define header-map (make-http-server:headers))
  (define pseudo-open? #t)
  (define method #f)
  (define scheme #f)
  (define target #f)
  (define authority #f)
  (define (fail message)
    (values #f #f #f #f #f message))

  (let loop ((rest headers))
    (if (null? rest)
        (if (and method scheme target)
            (let-values (((path query) (parse-target target)))
              (when (and authority
                         (not (http-server:headers-ref header-map "host" #f)))
                (http-server:headers-add! header-map "host" authority))
              (values (string->symbol method)
                      target
                      path
                      query
                      header-map
                      #f))
            (fail "Missing required pseudo headers"))
        (let-values (((name value err) (decode-header-entry (car rest))))
          (if err
              (fail err)
              (let ((pseudo? (and (> (string-length name) 0)
                                  (char=? (string-ref name 0) #\:))))
                (cond
                 ((contains-uppercase-ascii? name)
                  (fail "Uppercase header name is not allowed"))
                 ((and pseudo? (not pseudo-open?))
                  (fail "Pseudo header must appear before regular headers"))
                 (pseudo?
                  (cond
                   ((string=? name ":method")
                    (if method
                        (fail "Duplicate :method")
                        (begin (set! method value)
                               (loop (cdr rest)))))
                   ((string=? name ":scheme")
                    (if scheme
                        (fail "Duplicate :scheme")
                        (begin (set! scheme value)
                               (loop (cdr rest)))))
                   ((string=? name ":path")
                    (if target
                        (fail "Duplicate :path")
                        (begin (set! target value)
                               (loop (cdr rest)))))
                   ((string=? name ":authority")
                    (if authority
                        (fail "Duplicate :authority")
                        (begin (set! authority value)
                               (loop (cdr rest)))))
                   (else
                    (fail "Unknown pseudo header"))))
                 (else
                  (begin
                    (set! pseudo-open? #f)
                    (cond
                     ((connection-specific-header? name)
                      (fail "Connection-specific header is not allowed in HTTP/2"))
                     ((and (string=? name "te")
                           (not (string-ci=? value "trailers")))
                      (fail "Only TE: trailers is allowed in HTTP/2"))
                     (else
                      (http-server:headers-add! header-map name value)
                      (loop (cdr rest)))))))))))))

(define (stream->request conn stream)
  (let* ((remote (guard (e (else #f))
		   (socket-info (http2-server-connection-state-socket conn))))
	 (body (bytevector-concatenate
                (reverse (http2-server-stream-body-chunks stream))))
         (req (make-http-server:http2-request
               (http2-server-stream-method stream)
               (http2-server-stream-target stream)
               (http2-server-stream-path stream)
               (http2-server-stream-query stream)
               (http2-server-stream-headers stream)
               body
               (http2-server-stream-id stream))))
    (http-server:request-remote-set! req remote)
    req))

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

(define (response-body->bytevector body)
  (cond ((bytevector? body) body)
        ((string? body) (string->utf8 body))
        (else #f)))

(define (status-has-no-body? code)
  (or (eqv? code 204)
      (eqv? code 304)
      (and (<= 100 code) (< code 200))))

(define (response->hpack-headers req res)
  (define (header-entry name value)
    (list (string->utf8 name) (string->utf8 value)))

  (define (collect headers)
    (let loop ((rest headers) (out '()))
      (if (null? rest)
          (reverse out)
          (let ((name (http-server:normalize-header-name (caar rest)))
                (values (cdar rest)))
            (if (or (http-server:hop-by-hop-header? name)
                    (and (> (string-length name) 0)
                         (char=? (string-ref name 0) #\:)))
                (loop (cdr rest) out)
                (loop (cdr rest)
                      (append (map (lambda (v) (header-entry name v)) values)
                              out)))))))

  (let* ((status (http-server:response-status res))
         (reason (or (http-server:response-reason res)
                     (http-server:reason-phrase status)))
         (body (response-body->bytevector (http-server:response-body res))))
    (if (not body)
        (let ((err (make-http-server:response 500)))
          (http-server:response-text! err "Unsupported response body type")
          (response->hpack-headers req err))
        (let* ((skip-body? (or (status-has-no-body? status)
                               (and req
                                    (eq? (http-server:request-method req) 'HEAD))))
               (body-bytes (if skip-body? #vu8() body)))
          (unless (or skip-body?
                      (http-server:response-header-ref res "content-length" #f))
            (http-server:response-header-set! res
                                              "content-length"
                                              (number->string
                                               (bytevector-length body-bytes))))
          (unless (http-server:response-header-ref res "date" #f)
            (http-server:response-header-set! res "date"
                                              (http-server:current-http-date)))
          (values (cons (header-entry ":status" (number->string status))
                        (collect (http-server:headers->alist
                                  (http-server:response-headers res))))
                  body-bytes
                  skip-body?)))))

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

(define (method->http-token method)
  (cond ((symbol? method) (string-upcase (symbol->string method)))
        ((string? method) (string-upcase method))
        (else
         (assertion-violation 'method->http-token
                              "Unsupported HTTP method"
                              method))))

(define (collect-push-headers headers)
  (define (header-entry name value)
    (list (string->utf8 name) (string->utf8 value)))
  (let loop ((rest (http-server:headers->alist headers)) (out '()))
    (if (null? rest)
        (reverse out)
        (let ((name (http-server:normalize-header-name (caar rest)))
              (values (cdar rest)))
          (if (or (http-server:hop-by-hop-header? name)
                  (and (> (string-length name) 0)
                       (char=? (string-ref name 0) #\:)))
              (loop (cdr rest) out)
              (loop (cdr rest)
                    (append (map (lambda (v) (header-entry name v)) values)
                            out)))))))

(define (push-enabled? conn req)
  (and req
       (http-server:http2-request? req)
       (odd? (http-server:http2-request-stream-id req))
       (config-ref (http2-server-connection-state-config conn)
                   'http2-enable-push?
                   #f)
       (http2-server-connection-state-remote-enable-push? conn)))

(define (allocate-push-stream-id! conn)
  (let ((sid (http2-server-connection-state-next-push-stream-id conn)))
    (if (> sid #x7fffffff)
        #f
        (begin
          (http2-server-connection-state-next-push-stream-id-set! conn (+ sid 2))
          sid))))

(define (prepare-pushed-request conn parent-req stream-id method target headers)
  (define socket (http2-server-connection-state-socket conn))
  (define scheme (if (tls-socket? socket) "https" "http"))
  (define authority
    (or (http-server:headers-ref headers "host" #f)
        (and parent-req (http-server:request-header-ref parent-req "host" #f))
        "localhost"))
  (define method-token (method->http-token method))
  (define push-headers (make-http-server:headers (http-server:headers->alist headers)))
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
      (when parent-req
        (http-server:request-remote-set! req (http-server:request-remote parent-req)))
      (values req
              (append (list (list (string->utf8 ":method") (string->utf8 method-token))
                            (list (string->utf8 ":scheme") (string->utf8 scheme))
                            (list (string->utf8 ":authority") (string->utf8 authority))
                            (list (string->utf8 ":path") (string->utf8 target)))
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

(define (emit-push-responses! conn stream req res)
  (when (and (push-enabled? conn req)
             (pair? (http-server:response-pushes res)))
    (for-each
     (lambda (push)
       (let ((method (car push))
             (target (cadr push))
             (headers (caddr push)))
         (let ((push-id (allocate-push-stream-id! conn)))
           (when push-id
             (let-values (((push-req promise-headers)
                           (prepare-pushed-request conn req push-id method target headers)))
               (let ((push-stream (make-push-stream conn
                                                    push-id
                                                    (http2-server-stream-id stream))))
                 (send-frame! conn
                              (make-http2-frame-push-promise
                               0
                               (http2-server-stream-id stream)
                               push-id
                               promise-headers)
                              #f)
                 (dispatch-pushed-application! conn push-stream push-req)))))))
     (http-server:response-pushes res))))

(define (ensure-stream-id-valid conn sid)
  (when (or (zero? sid) (even? sid)
            (<= sid (http2-server-connection-state-last-stream-id conn)))
    (http2-protocol-error 'http2-dispatch-frame!
                          "Invalid client stream identifier"
                          sid)))

(define (find-stream conn sid)
  (hashtable-ref (http2-server-connection-state-streams conn) sid #f))

(define (drop-stream! conn sid)
  (hashtable-delete! (http2-server-connection-state-streams conn) sid)
  (http2-priority-tree-remove! (http2-server-connection-state-priority-tree conn)
                               sid))

(define (for-each-stream conn proc)
  (let-values (((keys values)
                (hashtable-entries (http2-server-connection-state-streams conn))))
    (let ((size (vector-length keys)))
      (let loop ((i 0))
        (unless (= i size)
          (let ((stream (vector-ref values i)))
            (when stream
              (proc stream)))
          (loop (+ i 1)))))))

(define (decode-priority-dependency raw)
  (if (and raw (integer? raw))
      (values (bitwise-and raw #x7fffffff)
              (not (zero? (bitwise-and raw #x80000000))))
      (values 0 #f)))

(define (flush-pending-output! conn)
  (define (stream-active? sid)
    (let ((stream (find-stream conn sid)))
      (and stream
           (> (bytevector-length (http2-server-stream-pending-output stream)) 0)
           (> (http2-server-stream-send-window stream) 0)
           (> (http2-server-connection-state-connection-send-window conn) 0))))

  (define (split-bytevector bv count)
    (let ((size (bytevector-length bv)))
      (values (bytevector-copy bv 0 count)
              (bytevector-copy bv count size))))

  (let loop ()
    (let* ((tree (http2-server-connection-state-priority-tree conn))
           (sid (http2-priority-tree-schedule tree stream-active?)))
      (if (not sid)
          #t
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
                  (if (<= send-size 0)
                      #t
                      (let-values (((chunk remain) (split-bytevector pending send-size)))
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

(define (register-stream! conn stream)
  (hashtable-set! (http2-server-connection-state-streams conn)
                  (http2-server-stream-id stream)
                  stream)
  (when (odd? (http2-server-stream-id stream))
    (http2-server-connection-state-last-stream-id-set!
     conn
     (http2-server-stream-id stream)))
  stream)

(define (dispatch-application! conn stream)
  (let* ((req (stream->request conn stream))
         (res (make-http-server:response))
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

(define (send-rst-stream! conn sid code)
  (drop-stream! conn sid)
  (send-frame! conn (make-http2-frame-rst-stream 0 sid code) #f))

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
                                 "Invalid initial window size"
                                 value))
         (let ((delta (- value
                         (http2-server-connection-state-remote-initial-window-size conn))))
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
                               value))
        (else #f))))
   settings))

(define (dispatch-frame! conn frame)
  (define sid (http2-frame-stream-identifier frame))
  (cond
   ((http2-frame-settings? frame)
    (if (frame-flag-set? (http2-frame-flags frame) +http2-frame-flag-ack+)
        #t
        (begin
          (apply-peer-settings! conn (http2-frame-settings-settings frame))
          (send-frame! conn
                       (make-http2-frame-settings +http2-frame-flag-ack+ 0 '())
                       #f)
          (flush-pending-output! conn)
          #t)))

   ((http2-frame-ping? frame)
    (if (frame-flag-set? (http2-frame-flags frame) +http2-frame-flag-ack+)
        #t
        (begin
          (send-frame! conn
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
           conn
           (+ increment (http2-server-connection-state-connection-send-window conn)))
          (let ((stream (find-stream conn sid)))
            (when stream
              (http2-server-stream-send-window-set!
               stream
               (+ increment (http2-server-stream-send-window stream))))))
      (flush-pending-output! conn)
      #t))

   ((http2-frame-goaway? frame)
    (http2-server-connection-state-stage-set! conn 'closing)
    #f)

   ((http2-frame-rst-stream? frame)
    (drop-stream! conn sid)
    #t)

   ((http2-frame-priority? frame)
    (let-values (((dependency exclusive?)
                  (decode-priority-dependency
                   (http2-frame-priority-stream-dependency frame))))
      (let ((stream (find-stream conn sid)))
        (http2-priority-tree-add! (http2-server-connection-state-priority-tree conn)
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

   ((http2-frame-headers? frame)
    (ensure-stream-id-valid conn sid)
    (when (find-stream conn sid)
      (http2-protocol-error 'dispatch-frame!
                            "HEADERS received for existing stream"
                            sid))
    (let-values (((method target path query header-map err)
                  (parse-http2-request-headers
                   (http2-frame-headers-headers frame))))
      (if err
          (begin
            (send-rst-stream! conn sid +http2-error-code-protocol-error+)
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
                     (http2-server-connection-state-remote-initial-window-size conn)
                     +default-initial-window-size+
                     0
                     #vu8()
                     #f
                     dependency
                     wire-weight)))
              (http2-priority-tree-add!
               (http2-server-connection-state-priority-tree conn)
               sid
               dependency
               wire-weight
               exclusive?
               #f)
              (register-stream! conn stream)
              (if (http2-frame-end-stream? frame)
                  (dispatch-application! conn stream)
                  #t))))))

   ((http2-frame-data? frame)
    (let ((stream (find-stream conn sid)))
      (if (not stream)
          (begin
            (send-rst-stream! conn sid +http2-error-code-stream-closed+)
            #t)
          (let* ((data (http2-frame-data-data frame))
                 (received (bytevector-length data))
                 (conn-recv-window
                  (http2-server-connection-state-connection-recv-window conn))
                 (stream-recv-window (http2-server-stream-recv-window stream))
                 (new-size (+ (http2-server-stream-body-size stream)
                              received))
                 (max-body (config-ref (http2-server-connection-state-config conn)
                                       'max-body-bytes
                                       +default-max-body-bytes+)))
            (when (or (> received conn-recv-window)
                      (> received stream-recv-window))
              (http2-flow-control-error 'dispatch-frame!
                                        "DATA exceeds flow control window"
                                        sid))
            (http2-server-connection-state-connection-recv-window-set!
             conn
             (- conn-recv-window received))
            (http2-server-stream-recv-window-set!
             stream
             (- stream-recv-window received))
            (http2-server-connection-state-connection-recv-consumed-set!
             conn
             (+ (http2-server-connection-state-connection-recv-consumed conn)
                received))
            (http2-server-stream-recv-consumed-set!
             stream
             (+ (http2-server-stream-recv-consumed stream)
                received))
            (if (> new-size max-body)
                (begin
                  (send-rst-stream! conn sid +http2-error-code-enhance-your-calm+)
                  #t)
                (begin
                  (http2-server-stream-body-size-set! stream new-size)
                  (http2-server-stream-body-chunks-set!
                   stream
                   (cons data (http2-server-stream-body-chunks stream)))
                    (maybe-send-connection-window-update! conn)
                    (maybe-send-stream-window-update! conn stream)
                  (if (http2-frame-end-stream? frame)
                      (dispatch-application! conn stream)
                      #t)))))))

   ((http2-frame-continuation? frame)
    (http2-protocol-error 'dispatch-frame!
                          "Unexpected CONTINUATION frame" sid))

  (else #t)))

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
  (let ((max-header-bytes (config-ref (http2-server-connection-state-config conn)
                                      'max-header-bytes
                                      +default-max-header-bytes+))
        (max-concurrent-streams (config-ref (http2-server-connection-state-config conn)
                                            'http2-max-concurrent-streams
                                            +default-max-concurrent-streams+)))
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
  (send-frame! conn
               (make-http2-frame-settings +http2-frame-flag-ack+ 0 '())
               #f))

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

(define (process-ready! conn)
  (let loop ()
    (let ((pending (http2-server-connection-state-pending conn)))
      (if (zero? (bytevector-length pending))
          #t
          (let ((end (http2-frame-block-end
                      pending
                      0
                      (http2-server-connection-state-local-max-frame-size conn))))
            (if (not end)
                #t
                (let ((frame-bytes (bytevector-copy pending 0 end))
                      (next (bytevector-copy pending end (bytevector-length pending))))
                  (http2-server-connection-state-pending-set! conn next)
                  (let ((frame
                         (read-http2-frame
                          (open-bytevector-input-port frame-bytes)
                          (http2-server-connection-state-read-buffer conn)
                          (http2-server-connection-state-decoder-context conn))))
                    (if (dispatch-frame! conn frame)
                        (loop)
                        #f)))))))))

(define (process-connection! conn chunk)
  (guard (e ((http2-error? e)
             (send-goaway! conn (http2-error-code e) (condition-message e)))
            (else
             (send-goaway! conn +http2-error-code-internal-error+
                           (condition-message e))))
    (when (connection-closed? conn)
      #f)
    (when (not (bytevector? chunk))
      (http2-protocol-error 'process-connection!
                            "Bytevector chunk required"
                            chunk))
    (when (positive? (bytevector-length chunk))
      (http2-server-connection-state-pending-set!
       conn
       (bytevector-append (http2-server-connection-state-pending conn)
                          chunk)))

    (when (eq? (http2-server-connection-state-stage conn) 'await-preface)
      (consume-preface! conn))

    (if (eq? (http2-server-connection-state-stage conn) 'ready)
        (let ((result (process-ready! conn)))
          (and result (flush-pending-output! conn)))
        #t)))

(define (make-http-server:http2-connection socket config app-handler
                                           :key
                                           (settings '())
                                           (upgrade-request #f)
                                           (expect-preface? #t))
  (let* ((max-concurrent-streams
          (config-ref config
                      'http2-max-concurrent-streams
                      +default-max-concurrent-streams+))
         (priority-node-cap (+ 5 (* 2 max-concurrent-streams)))
         (decoder (make-hpack-context +default-header-table-size+))
         (encoder (make-hpack-context +default-header-table-size+))
         (conn (make-http2-server-connection-state
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
      (send-initial-settings! conn))
    (unless (null? settings)
      (apply-peer-settings! conn settings)
      (send-settings-ack! conn))
    (when upgrade-request
      (replay-upgrade-request! conn upgrade-request)
      (flush-pending-output! conn))
    (make-http-server:connection
     (lambda (chunk)
       (process-connection! conn chunk))
     (lambda ()
       (close-connection! conn)
       #t))))

(define (http-server:http2-consume state buffer :key (max-header-bytes 65536)
                                   (max-body-bytes 1048576))
  (values 'error 500 "HTTP/2 driver is connection-oriented" buffer #f))

(define (http-server:http2-serve socket req result)
  #t)

(define *http-server:http2-driver*
  (make-http-server:protocol-driver
   "h2"
   http-server:http2-consume
   http-server:http2-serve
   make-http-server:http2-connection))

)
