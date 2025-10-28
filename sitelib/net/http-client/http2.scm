;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/http2.scm - HTTP/2 engine for HTTP client
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; I wanted to reuse (rfc http2 client) but it's rather difficult
;; without breaking backward compatibility so rewriting it...
#!nounbound
#!read-macro=sagittarius/bv-string
(library (net http-client http2)
    (export http2-connection-context?
	    socket->http2-connection)
    (import (rnrs)
	    (net http-client connection)
	    (net http-client request)
	    (net socket)
	    (net uri)
	    (rfc http2 frame)
	    (rfc http2 conditions)
	    (rfc http2 hpack)
	    (srfi :18 multithreading)
	    (util hashtables))

(define +window-size+ 65535)
(define-record-type http2-connection-context
  (parent <http-connection-context>)
  (fields streams
	  request-hpack-context
	  response-hpack-context
	  buffer
	  mutex
	  (mutable next-id)
	  (mutable initial-window-size)
	  (mutable window-size))
  (protocol (lambda (n)
	      (lambda ()
		((n)
		 (make-eqv-hashtable)
		 (make-hpack-context 4096)
		 (make-hpack-context 4096)
		 (make-frame-buffer)
		 (make-mutex)
		 1
		 +window-size+
		 +window-size+)))))
(define (make-http2-connection socket socket-option node service)
  (make-http-connection node service socket-option
			socket
			http2-send-header http2-send-data
			http2-receive-header http2-receive-data
			(make-http2-connection-context)))

(define-enumeration http2:stream-state
  (idle reserved open half-closed closed)
  ;; won't be used ;)
  http2-stream-state-set)
  
(define-record-type http2-stream
  (fields id
	  connection
	  request
	  (mutable state)
	  (mutable remote-state)
	  (mutable window-size)
	  (mutable local-window-size)
	  (mutable data-sent)
	  (mutable data-received))
  (protocol (lambda (p)
	      (lambda (connection request)
		(define id (http2-next-stream-id connection))
		(define ctx (http-connection-context-data connection))
		(let ((r (p id connection
			    request
			    (http2:stream-state idle)
			    (http2:stream-state idle)
			    (http2-connection-context-initial-window-size ctx)
			    +client-window-size+
			    0
			    0)))
		  (hashtable-set! (http2-connection-context-streams ctx) id r)
		  r)))))

;;(define +client-window-size+ (- (expt 2 31) 1))
(define +client-window-size+ +window-size+)
(define +default-settings+
  `(;; for now, we don't handle push
    (,+http2-settings-enable-push+ 0)
    (,+http2-settings-initial-window-size+ ,+client-window-size+)
    (,+http2-settings-max-concurrent-streams+ 100)))

;;; API
(define (socket->http2-connection socket socket-option node service)
  (let ((conn (make-http2-connection socket socket-option node service)))
    (http2-send-preface conn)
    (http2-send-settings conn 0 +default-settings+)
    ;; Set the global window size 2^31 - 1 (max)
    ;; NOTE
    ;; If we don't do this, then all the window size is limited to the
    ;; initial size. This mechanism is probably used in the memory
    ;; strict environment, but we can do rich programming here, so
    ;; just make it unlimited.
    (send-window-update conn 0 (- (- (expt 2 31) 1) +window-size+))
    ;; Wait until server ACK settings
    (let loop ()
      (let-values (((frame stream) (handle-frame conn #f)))
	(unless (and (http2-frame-settings? frame)
		     (bitwise-bit-set? (http2-frame-flags frame) 0))
	  (loop))))
    conn))

;;; API
;; this must be done asynchronousely by client (otherwise blocks)
(define (http2-send-header connection request)
  (define stream (make-http2-stream connection request))
  ;; send request here
  (define header-frame (stream:request->header-frame stream request))

  (write-header-log connection "[Request header]" header-frame)
  
  (let ((est? (not (request-has-body? request))))
    (http2-write-stream stream header-frame est?)
    (if est?
	(http2-stream-state-set! stream (http2:stream-state half-closed))
	(http2-stream-state-set! stream (http2:stream-state open)))))

(define (http2-send-data connection request)
  (define stream (search-stream connection request))
  (when (eq? (http2:stream-state open) (http2-stream-state stream))
    (stream:send-request-data-frame stream request)
    (http2-stream-state-set! stream (http2:stream-state half-closed)))
  connection)

;;; API
;; We receive a response of the given request, requests are associated
;; to the stream, so we can detect which request we need to return
;; (NB: at this moment, we only have one stream)
(define (http2-receive-header connection response-context)
  (define request (http:response-context-request response-context))
  (define ((wrap-header-handler response-context) frame es?)
    (define handler (http:response-context-header-handler response-context))
    (let* ((headers (map (lambda (kv)
			   (list (utf8->string (car kv))
				 (utf8->string (cadr kv))))
			 (http2-frame-headers-headers frame))))
      (handler response-context
	       (cond ((assoc ":status" headers) => cadr)
		     (else #f))
	       headers
	       (not es?))))
  (define header-handler (wrap-header-handler response-context))
  (http2-response connection request 'header header-handler))

(define (http2-receive-data connection response-context)
  (define request (http:response-context-request response-context))
  (define ((wrap-data-handler response-context) frame es?)
    (define handler (http:response-context-data-handler response-context))
    (handler response-context (http2-frame-data-data frame) es?))
  (define data-handler (wrap-data-handler response-context))
  ;; after data receival, the stream is no longer available, so remove it
  (define (remove-stream connection request)
    (define context (http-connection-context-data connection))
    (define streams (http2-connection-context-streams context))
    (define target-stream (search-stream connection request))
    (http2-remove-stream! connection (http2-stream-id target-stream))
    connection)
  (define data-ready? http-connection-data-ready?)

  (let loop ((es? (http2-response connection request 'data data-handler)))
    (cond (es?
	   (remove-stream connection request)
	   (http:response-body-state done))
	  (else
	   (if (data-ready? connection)
	       (loop (http2-response connection request 'data data-handler))
	       (http:response-body-state continue))))))
  
(define (http2-response connection request to-receive handler)
  (define target-stream (search-stream connection request))
  (define rstate (http2-stream-remote-state target-stream))
  ;; already received?
  (cond ((and (eq? to-receive 'header) (memq rstate '(half-closed closed)))
	 ;; Okay, the stream already received header, so return
	 #t)
	((and (eq? to-receive 'data) (eq? rstate (http2:stream-state closed)))
	 ;; Okay, the stream already received data so return
	 #t)
	(else
	 (let loop ()
	   (let-values (((frame stream) (handle-frame connection handler)))
	     (if (and (eq? stream target-stream)
		      (or (http2-frame-headers? frame)
			  (http2-frame-data? frame)))
		 (http2-frame-end-stream? frame)
		 (loop)))))))

;;; helpers
(define (search-stream connection request)
  (define context (http-connection-context-data connection))
  (hashtable-seek (http2-connection-context-streams context)
		  (lambda (k v) (eq? (http2-stream-request v) request))
		  (lambda (t k v) v)
		  (lambda () (assertion-violation 'http2-search-stream
						  "No stream associated"
						  request))))

(define (handle-misc-frames connection stream frame)
  (define context (http-connection-context-data connection))
  (define streams (http2-connection-context-streams context))
  (define (send-rst-stream connection code stream-id)
    (send-frame connection (make-http2-frame-rst-stream 0 stream-id code) #t)
    (http2-stream-remote-state-set! stream (http2:stream-state closed)))
  (let* ((sid (http2-frame-stream-identifier frame))
	 (stream (hashtable-ref streams sid #f)))
    (cond ((http2-frame-settings? frame)
	   (http2-apply-server-settings connection frame)
	   #t)
	  ((http2-frame-ping? frame)
	   (write-http2-frame
	    (http-connection-output connection)
	    (http2-connection-context-buffer context)
	    (make-http2-frame-ping 1 sid (http2-frame-ping-opaque-data frame))
	    #t
	    (http2-connection-context-request-hpack-context context))
	   #t)
	  ((http2-frame-goaway? frame)
	   (http-connection-close! connection)
	   (let ((msg? (http2-frame-goaway-data frame)))
	     ;; TODO proper condition
	     (error 'http2-request
		    (if (zero? (bytevector-length msg?))
			"Got GOAWAY frame"
			(utf8->string msg?))
		    (http2-frame-goaway-last-stream-id frame)
		    (http2-frame-goaway-error-code frame))))
	  ((http2-frame-rst-stream? frame)
	   (http2-stream-remote-state-set! stream (http2:stream-state closed))
	   #t)
	  ((http2-frame-window-update? frame)
	   (let ((size (http2-frame-window-update-window-size-increment frame)))
	     (cond ((zero? size)
		    (send-rst-stream connection
				     +http2-error-code-protocol-error+ sid))
		   ((zero? sid)
		    (let ((v (http2-connection-context-window-size context)))
		      (http2-connection-context-window-size-set! context
		       (+ v size))))
		   (else
		    (let ((v (http2-stream-window-size stream)))
		      (http2-stream-window-size-set! stream (+ v size)))))
	     #t))
	  (else #f))))

(define (send-frame connection frame end?)
  (define context (http-connection-context-data connection))
  (define out (http-connection-output connection))
  (write-http2-frame out (http2-connection-context-buffer context)
    frame end? (http2-connection-context-request-hpack-context context)))

(define (send-goaway connection e ec stream-id)
  (send-frame connection
	      (make-http2-frame-goaway 
	       0 stream-id stream-id
	       ec
	       (if (message-condition? e)
		   (string->utf8 (condition-message e))
		   #vu8()))
	      #t)
  (http-connection-close! connection)
  (raise e))

(define (send-window-update connection si increment-size)
  (define frame (make-http2-frame-window-update 0 si increment-size))
  (send-frame connection frame #f))

(define (read-frame connection)
  (define context (http-connection-context-data connection))
  (define in (http-connection-input connection))
  (define res-hpack (http2-connection-context-response-hpack-context context))
  (guard (e ((http2-error? e)
	     ;; treat as connection error.
	     (send-goaway connection e (http2-error-code e) 0))
	    (else
	     ;; internal error. just close the session
	     (send-goaway connection e +http2-error-code-internal-error+ 0)))
    (read-http2-frame in (http2-connection-context-buffer context)
		      res-hpack)))

(define (update-window-size! stream size)
  (define received (http2-stream-data-received stream))
  (define new-received (+ received size))
  (define local-window (http2-stream-local-window-size stream))
  (http2-stream-data-received-set! stream new-received)
  (when (< (div local-window 2) new-received)
    ;; double the size
    (let ((delta local-window))
      (send-window-update (http2-stream-connection stream)
			  (http2-stream-id stream) delta)
      (http2-stream-local-window-size-set! stream (+ local-window delta)))))


(define (handle-frame connection handler)
  (define context (http-connection-context-data connection))
  (define streams (http2-connection-context-streams context))

  (let* ((frame (read-frame connection))
	 (sid (http2-frame-stream-identifier frame))
	 (stream (hashtable-ref streams sid #f))
	 (es? (http2-frame-end-stream? frame)))
    ;; SID 0 == global 
    (unless (or (zero? sid) stream)
      (assertion-violation 'http2-response
			   "Stream is not registered or already removed" sid))
    (when es?
      (http2-stream-remote-state-set! stream
	(case (http2-stream-state stream)
	  ;; local already sent ES, 
	  ;; - header retrieval no body
	  ;; - data retrieval
	  ((half-closed) (http2:stream-state closed))
	  ;; local is still open (header retrieval with body)
	  ((open)        (http2:stream-state half-closed))
	  ;; mustn't be but in case, we put half-closed)
	  (else          (http2:stream-state half-closed)))))
    (cond ((handle-misc-frames connection stream frame) (values frame #f))
	  ((http2-frame-headers? frame)
	   (write-header-log connection "[Response header]" frame)
	   (handler frame es?)
	   (values frame stream))
	  ((http2-frame-data? frame)
	   (let ((data (http2-frame-data-data frame)))
	     (http-connection-write-log connection
					"HTTP2 data length ~a, ~a"
					(bytevector-length data) es?)
	     (handler frame es?)
	     (update-window-size! stream (bytevector-length data))
	     (values frame stream)))
	  (else
	   (assertion-violation 'http2-response "Unknown frame type" frame)))))

(define (request-has-body? request)
  (define method (http:request-method request))
  (and (http:request-body request)
       (not (http:no-body-method? method))))

(define (stream:send-request-data-frame stream request)
  (define method (http:request-method request))
  (define (send-data-frame stream request)
    (define body (http:request-body request))
    (define window-size (http2-stream-window-size stream))
    ;; this shouldn't be needed though...
    (define (pause! size)
      (let loop ()
	(handle-frame (http2-stream-connection stream #f))
	(when (<= (http2-stream-window-size stream) size) (loop)))
      (set! window-size (http2-stream-window-size stream)))

    (define (send-bytevector-data-frame stream bv es?)
      ;; TODO split frames in case of the bv is bigger than window size
      (define sent (http2-stream-data-sent stream))
      (define new-sent (+ sent (bytevector-length bv)))
      (when (>= new-sent window-size) (pause! new-sent))
      (http2-stream-data-sent-set! stream new-sent)
      (let ((frame (make-http2-frame-data 0 (http2-stream-id stream) bv)))
	(http2-write-stream stream frame es?)))

    (define (send-input-port-data-frame stream bin)
      (define bufsize 8192)
      (define buffer (make-bytevector bufsize))
      (define (fill!) (get-bytevector-n! bin buffer 0 bufsize))
      (define (send! buffer n es?)
	(define bv (cond ((= n bufsize) buffer)
			 ((zero? n) #vu8())
			 (else (bytevector-copy buffer 0 n))))
	(send-bytevector-data-frame stream bv es?))
      (do ((n (fill!) (fill!)))
	  ((not (= n bufsize))
	   (if (eof-object? n) (send! #vu8() 0 #t) (send! buffer n #t)))
	(send! buffer n #f)))
    
    (cond ((bytevector? body)
	   (send-bytevector-data-frame stream body #t))
	  ((and (input-port? body) (binary-port? body))
	   (send-input-port-data-frame stream body))
	  (else (assertion-violation 'stream:request->data-frame-sender
				     "Unknown body type" body))))
  (and (request-has-body? request)
       (send-data-frame stream request)))

(define (stream:request->header-frame stream request)
  (define id (http2-stream-id stream))
  (define (request->headers stream request)
    (define conn (http2-stream-connection stream))
    (define options (http-connection-socket-options conn))
    (define headers (http:request-headers request))
    (define ->bv string->utf8)
    (define (headers->http2-headers headers)
      ;; headers is multi valued map
      (map (lambda (name)
	     (let ((k (string-downcase name)))
	       (if (memp (lambda (n) (string=? k n)) +http:managed-headers+)
		   '()
		   (map (lambda (v) (list (->bv k) (->bv v)))
			(http:headers-ref* headers name)))))
	   (http:headers-names headers)))
      
    `((#*":method" ,(->bv (symbol->string (http:request-method request))))
      (#*":scheme" ,(if (tls-socket-options? options) #*"https" #*"http"))
      (#*":authority" ,(->bv (or (http:headers-ref headers "host")
				 (uri-host (http:request-uri request))
				 (http-connection-node conn))))
      (#*":path"   ,(->bv (http:request->request-uri request)))
      ,@(cond ((and (http:request-body request)
		    (http:request-content-type request))
	       => (lambda (type)
		    `((#*"content-type" ,(->bv type)))))
	      (else '()))
      ,@(apply append (headers->http2-headers headers))))
  (let ((headers (request->headers stream request)))
    (make-http2-frame-headers 0 id #f #f headers)))

(define (http2-write-stream stream frame end?)
  (define conn (http2-stream-connection stream))
  (define context (http-connection-context-data conn))
  (define out (http-connection-output conn))
  (write-http2-frame out (http2-connection-context-buffer context)
		     frame end?
		     (http2-connection-context-request-hpack-context context)))

(define (http2-remove-stream! conn id)
  (define context (http-connection-context-data conn))
  (define streams (http2-connection-context-streams context))
  (hashtable-delete! streams id))

(define (http2-next-stream-id conn)
  (define context (http-connection-context-data conn))
  ;; I don't think with this http-client we share connection among threads
  ;; so this may not needed, but not sure at this moment
  (mutex-lock! (http2-connection-context-mutex context))
  (let ((next-id (http2-connection-context-next-id context)))
    (http2-connection-context-next-id-set! context (+ next-id 2))
    (mutex-unlock! (http2-connection-context-mutex context))
    next-id))

(define +http2-preface+ #*"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")
(define (http2-send-preface conn)
  (let ((out (http-connection-output conn)))
    (put-bytevector out +http2-preface+)))
(define (http2-send-settings conn flags settings)
  (define context (http-connection-context-data conn))
  (send-frame conn (make-http2-frame-settings flags 0 settings) #f))
(define (http2-handle-server-settings conn)
  (define context (http-connection-context-data conn))
  (let ((in (http-connection-input conn)))
    (http2-apply-server-settings 
     conn (read-http2-frame in (http2-connection-context-buffer context) #f))))

(define (http2-apply-server-settings conn frame)
  (define ctx (http-connection-context-data conn))
  (define out (http-connection-output conn))
  (define settings (http2-frame-settings-settings frame))
  ;; Ignore ACK SETTINGS
  (and (not (bitwise-bit-set? (http2-frame-flags frame) 0))
       ;; TODO handle it properly
       (let loop ((settings settings))
	 (cond ((null? settings) (http2-send-settings conn 1 '()))
	       (else
		(cond ((= (caar settings) +http2-settings-header-table-size+)
		       ;; initial request HPACK table size.
		       (update-hpack-table-size!
			(http2-connection-context-request-hpack-context ctx) 
			(cdar settings)))
		      ((= (caar settings) +http2-settings-initial-window-size+)
		       (http2-connection-context-initial-window-size-set! ctx
			(cdar settings))))
		(loop (cdr settings)))))))

(define (write-header-log connection key header-frame)
  ;; FIXME I don't want to do this check
  (when (http-logging-connection? connection)
    (for-each (lambda (hv)
		(http-connection-write-log connection
					   "~a ~a: ~a"
					   key
					   (utf8->string (car hv))
					   (utf8->string (cadr hv))))
	      (http2-frame-headers-headers header-frame))))
)

;; Appendix :-)
#|
5.1. Stream States
The lifecycle of a stream is shown in Figure 2.

            
                             +--------+
                     send PP |        | recv PP
                    ,--------|  idle  |--------.
                   /         |        |         \
                  v          +--------+          v
           +----------+          |           +----------+
           |          |          | send H /  |          |
    ,------| reserved |          | recv H    | reserved |------.
    |      | (local)  |          |           | (remote) |      |
    |      +----------+          v           +----------+      |
    |          |             +--------+             |          |
    |          |     recv ES |        | send ES     |          |
    |   send H |     ,-------|  open  |-------.     | recv H   |
    |          |    /        |        |        \    |          |
    |          v   v         +--------+         v   v          |
    |      +----------+          |           +----------+      |
    |      |   half   |          |           |   half   |      |
    |      |  closed  |          | send R /  |  closed  |      |
    |      | (remote) |          | recv R    | (local)  |      |
    |      +----------+          |           +----------+      |
    |           |                |                 |           |
    |           | send ES /      |       recv ES / |           |
    |           | send R /       v        send R / |           |
    |           | recv R     +--------+   recv R   |           |
    | send R /  `----------->|        |<-----------'  send R / |
    | recv R                 | closed |               recv R   |
    `----------------------->|        |<----------------------'
                             +--------+

       send:   endpoint sends this frame
       recv:   endpoint receives this frame

       H:  HEADERS frame (with implied CONTINUATIONs)
       PP: PUSH_PROMISE frame (with implied CONTINUATIONs)
       ES: END_STREAM flag
       R:  RST_STREAM frame
|#
