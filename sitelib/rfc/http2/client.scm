;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http2/client.scm - HTTP2 client
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/bv-string
(library (rfc http2 client)
    (export http2-client-connection?
	    make-http2-client-connection
	    close-http2-client-connection!
	    http2-construct-header
	    ;; expose them as well
	    http2-add-request!
	    http2-invoke-requests!
	    ;; convenient macro
	    http2-request
	    http2-multi-requests

	    ;; common methods
	    http2-get ;; TODO the rest
	    http2-head
	    http2-post

	    ;; primitive senders&receivers
	    http2-headers-sender
	    http2-data-sender
	    http2-multipart-sender
	    http2-composite-sender
	    
	    http2-data-receiver
	    http2-binary-receiver
	    http2-null-receiver
	    http2-gzip-receiver make-gzip-receiver

	    http2-redirect-handler
	    ;; TODO redirect handler using HTTP1.1
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius socket)
	    (binary io)
	    (match)
	    (rfc tls)
	    (rfc uri)
	    (rfc http2 frame)
	    (rfc http2 conditions)
	    (rfc http2 hpack)
	    (rfc gzip)
	    (rfc mime)
	    (srfi :1)
	    (srfi :18))

  ;; HTTP2 client connection
  (define-record-type (http2-client-connection 
		       %make-http2-client-connection 
		       http2-client-connection?)
    ;; HPACK buffers must be separated for request and response.
    (fields request-hpack-context      ; request HPACK context
	    response-hpack-context     ; response HPACK context
	    source		       ; input/output port
	    buffer		       ; frame buffer
	    (mutable next-id)	       ; next stream-id
	    streams		       ; streams (hashtable)
	    (mutable window-size)      ; window size. do we need this?
				       ; above is only for server one
	    mutex		       ; lock for id
	    user-agent		       ; for user-agent header
	    ;; belows are for reconnection/debugging
	    server		       ; request server
	    port		       ; request port
	    secure?		       ; TLS or not
	    )
    (protocol
     (lambda (n)
       (lambda (server port secure? user-agent)
	 (let ((socket (if secure?
			   (make-client-tls-socket 
			    server port
			    ;; at least try to use ALPN
			    :hello-extensions 
			    (list (make-server-name-indication (list server))
				  (make-protocol-name-list (list "h2"))))
			   (make-client-socket server port))))
	   ;; TODO check TLS socket extension. 
	   ;; (though, we can't do anything here since this only does HTTP2...)
	   (n (make-hpack-context 4096)
	      (make-hpack-context 4096)
	      (socket-port socket)
	      (make-frame-buffer)
	      1
	      (make-eqv-hashtable)
	      65535			; initial value 
	      (make-mutex)
	      user-agent
	      server port secure?))))))

  (define (stream-not-opened-error-raiser stream-id)
    (lambda (stream converter)
      (http2-stream-closed
       'http2-sender
       "The stream is not opened. Maybe forgot to call http2-headers-sender?"
       `(stream-identifier, stream-id))))
	    
  ;; A stream is created when either HEADERS is sent or PUSH_PROMISE
  ;; is received. Either case, the state should be open.
  ;; For now we don't preserve streams so there is no idel state.
  ;; TODO we might want to reuse this
  (define-record-type http2-stream
    (fields identifier
	    method	  ;; needed for redirect
	    uri		  ;; ditto
	    sender
	    (mutable receiver)	  ;; this can be later
	    (mutable state)	  ;; not really used though
	    (mutable window-size) ;; again do we need this?
	    connection		  ;; connection of this stream
	    redirect-handler	  ;; for redirect, must be per stream?
	    (mutable header-sender)     ;; will be set by http2-headers-sender
	    )
    (protocol
     (lambda (p)
       (lambda (identifier method uri sender receiver connection
			   :key (redirect-handler http2-redirect-handler))
	 (p identifier method uri sender receiver 'open 
	    (http2-client-connection-window-size connection)
	    connection
	    redirect-handler
	    (stream-not-opened-error-raiser identifier))))))
  (define (http2-stream-send-header stream conv)
    ((http2-stream-header-sender stream) stream conv))
  (define (http2-stream-need-header? stream)
    (http2-stream-header-sender stream))
  ;; open stream
  ;; associate sender and receiver to the created stream
  ;; associate stream to identifier
  (define (http2-open-stream conn method uri sender receiver . opts)
    (define stream-id (http2-next-stream-identifier conn))
    (let ((stream (apply make-http2-stream stream-id method uri sender 
			 receiver conn opts)))
      (hashtable-set! (http2-client-connection-streams conn) stream-id stream)
      stream))

  (define (http2-lookup-stream conn id)
    (let ((lookup (http2-client-connection-streams conn)))
      (hashtable-ref lookup id #f)))

  (define (http2-remove-stream! conn id)
    (define streams (http2-client-connection-streams conn))
    (hashtable-delete! streams id))

  ;; send or received END_STREAM
  (define (http2-half-close-stream! stream remote?)
    (http2-stream-state-set! stream (list 'half-closed remote?)))

  (define (http2-read-stream stream)
    (define in (%h2-source (http2-stream-connection stream)))
    ;; TODO read
    )
  (define (http2-write-stream stream frame end?)
    (define conn (http2-stream-connection stream))
    (define out (%h2-source conn))
    (write-http2-frame out (%h2-buffer conn) frame end? (%h2-req-hpack conn)))

  ;; make things shorter...
  (define-syntax %h2-source
    (identifier-syntax http2-client-connection-source))
  (define-syntax %h2-buffer
    (identifier-syntax http2-client-connection-buffer))
  (define-syntax %h2-req-hpack
    (identifier-syntax http2-client-connection-request-hpack-context))
  (define-syntax %h2-res-hpack
    (identifier-syntax http2-client-connection-response-hpack-context))
  (define-syntax %h2-secure?
    (identifier-syntax http2-client-connection-secure?))
  (define-syntax %h2-agent
    (identifier-syntax http2-client-connection-user-agent))

  (define-syntax %h2-redirect
    (identifier-syntax http2-stream-redirect-handler))

  (define-constant +http2-default-user-agent+
    (string-append "Sagittarius-" (sagittarius-version)))
  ;; TODO add option to configure SETTINGS frame
  (define (make-http2-client-connection server port :key (secure? #f)
					(negotiate? #t)
					(user-agent +http2-default-user-agent+))
    (rlet1 conn (%make-http2-client-connection server port secure? user-agent)
      (when negotiate?
	(http2-send-preface conn)
	(http2-send-settings conn
	 `(;; for now disable push
	   (,+http2-settings-enable-push+ 0)
	   ;; no less than 100 huh?
	   (,+http2-settings-max-concurrent-streams+ 100)))
	(http2-handle-server-settings conn))))

  (define (close-http2-client-connection! conn)
    ;; Closing port would also close socket
    ;; see creation of the port
    (close-port (%h2-source conn)))
  
  (define-constant +http2-preface+ #*"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")
  (define (http2-send-preface conn)
    (let1 in/out (%h2-source conn)
      (put-bytevector in/out +http2-preface+)))

  (define (http2-send-settings conn settings)
    (let1 in/out (%h2-source conn)
      (write-http2-frame in/out (%h2-buffer conn)
			 (make-http2-frame-settings 0 0 settings)
			 ;; we know context is not needed and 
			 ;; this won't be the end of stream
			 #f #f)))
  (define (http2-apply-server-settings conn frame)
    (define in/out (%h2-source conn))
    (define settings (http2-frame-settings-settings frame))
    ;; Ignore ACK SETTINGS
    (unless (bitwise-bit-set? (http2-frame-flags frame) 0)
      ;; TODO handle it properly
      (let loop ((settings settings))
	(unless (null? settings)
	  (cond ((= (caar settings) +http2-settings-header-table-size+)
		 ;; initial request HPACK table size.
		 (update-hpack-table-size! (%h2-req-hpack conn) 
					   (cadar settings)))
		)
	  (loop (cdr settings))))
      ;; send SETTING with ACK
      (write-http2-frame in/out (%h2-buffer conn)
			 (make-http2-frame-settings 1 0 '())
			 ;; we know context is not needed and 
			 ;; this won't be the end of stream
			 #f #f)))
  ;; should only be called when the next frame is SETTINGS.
  ;; (e.g. during negotiation)
  (define (http2-handle-server-settings conn)
    (let1 in/out (%h2-source conn)
      (http2-apply-server-settings 
       conn (read-http2-frame in/out (%h2-buffer conn) #f))))
  
  (define (http2-next-stream-identifier conn)
    (mutex-lock! (http2-client-connection-mutex conn))
    (let ((next-id (http2-client-connection-next-id conn)))
      (http2-client-connection-next-id-set! conn (+ next-id 2))
      (mutex-unlock! (http2-client-connection-mutex conn))
      next-id))

  ;; TODO do we need this?
  (define (http2-add-request! conn method uri sender receiver . opts)
    (apply http2-open-stream conn method uri sender receiver opts)
    conn)

  (define (http2-has-pending-streams? conn)
    (not (zero? (hashtable-size (http2-client-connection-streams conn)))))

  ;; This is an interface of generic HTTP2 request.
  ;; TODO we need to handle push/promise, window update and others.
  (define (http2-invoke-requests! conn)
    (define streams (http2-client-connection-streams conn))
    (define (terminate-stream id) (http2-remove-stream! conn id))
    (let ((in/out (%h2-source conn))
	  (req-hpack (%h2-req-hpack conn))
	  (res-hpack (%h2-res-hpack conn)))
      (define (send-frame frame)
	(write-http2-frame in/out (%h2-buffer conn)
			   frame #t (%h2-req-hpack conn)))
      (define (send-goaway e ec stream-id)
	(send-frame (make-http2-frame-goaway 
		     0 stream-id stream-id
		     ec
		     (if (message-condition? e)
			 (string->utf8 (condition-message e))
			 #vu8())))
	(close-port in/out)
	(raise e))
      (define (send-rst-stream code stream-id)
	(send-frame (make-http2-frame-rst-stream 0 stream-id code))
	(terminate-stream stream-id))
      (define (read-frame)
	(guard (e ((http2-error? e)
		   ;; treat as connection error.
		   (send-goaway e (http2-error-code e) 0))
		  (else
		   ;; internal error. just close the session
		   (send-goaway e +http2-error-code-internal-error+ 0)
		   #f))
	  (read-http2-frame in/out (%h2-buffer conn) res-hpack)))
      (define (get-header sid alist)
	(cond ((assv sid alist) => cadr)
	      ;; TODO should we allow this?
	      (else '())))
      (define (call-receiver sid stream alist frame)
	(let ((receiver (http2-stream-receiver stream)))
	  (receiver stream 
		    (get-header sid alist)
		    frame ;; TODO how should we expose this?
		    (http2-frame-end-stream? frame))))

      (vector-for-each (lambda (sid)
			 (let* ((stream (hashtable-ref streams sid #f))
				(sender (http2-stream-sender stream)))
			   ;; we always pass end-stream? #t
			   ;; because we only call sender once.
			   (sender stream #t)))
		       (let ((keys (hashtable-keys streams)))
			 (vector-sort! < keys)
			 keys))
      ;; loop until connection has no open stream
      ;; the results is alist of sid, headers and receiver results.
      ;; the receiver results are only collected when the stream is
      ;; ended.
      (let loop ((results '()) (redirected-sids '()) (redirected-results '()))
	(define (continue/quit results sid? redirect?)
	  (if (http2-has-pending-streams? conn)
	      (loop results 
		    (if sid? (cons sid? redirected-sids) redirected-sids)
		    (if redirect? 
			(append redirect? redirected-results)
			redirected-results))
	      ;; drop stream identifier
	      (apply append (filter-map 
			     (lambda (slot) 
			       (and (not (memv (car slot) redirected-sids))
				    (list (cadr slot) (cddr slot))))
			     results)
		     redirected-results)))
	(let* ((frame (read-frame))
	       (sid   (http2-frame-stream-identifier frame))
	       (stream (hashtable-ref streams sid #f)))
	  (cond ((not frame) #f)
		;; these 2 must be first
		((http2-frame-settings? frame)
		 (http2-apply-server-settings conn frame)
		 (continue/quit results #f #f))
		((http2-frame-goaway? frame)
		 (close-port in/out)
		 (let ((msg? (http2-frame-goaway-data frame)))
		   (error 'http2-request
			  (if (zero? (bytevector-length msg?))
			      "Got GOAWAY frame"
			      (utf8->string msg?))
			  (http2-frame-goaway-last-stream-id frame)
			  (http2-frame-goaway-error-code frame)
			  msg?)))
		((http2-frame-rst-stream? frame)
		 (terminate-stream sid)
		 ;; TODO should we remove the stored result of this
		 ;; stream identifier?
		 ;; TODO might be better to raise an error when
		 ;; this is the last stream.
		 (continue/quit results #f #f)
		 #;
		 (error 'http2-request "protocol error" 
			(http2-frame-rst-stream-error-code frame)))
		;; window update
		((http2-frame-window-update? frame)
		 (let ((size (http2-frame-window-update-window-size-increment
			      frame)))
		   (cond ((zero? size)
			  (terminate-stream sid)
			  (send-rst-stream +http2-error-code-protocol-error+
					   sid))
			 ((zero? sid)
			  (http2-client-connection-window-size-set! conn size))
			 (else
			  (http2-stream-window-size-set! stream size))))
		 (continue/quit results #f #f))
		((http2-frame-headers? frame)
		 (let ((headers (http2-frame-headers-headers frame)))
		   (when (http2-frame-end-stream? frame)
		     (terminate-stream sid))
		   ;; TODO can HTTP2 send HEADERS more than once?
		   ;; if so, do we want to update the stored one?
		   (let ((status (utf8->string 
				  (http2-header-ref #*":status" headers))))
		     (if (and status 
			      (char=? (string-ref status 0) #\3)
			      ;; 304 and 305 are not redirect
			      (not (memv (string-ref status 2) '(#\4 #\5))))
			 ;; redirect
			 (let ((r ((%h2-redirect stream) stream headers)))
			   (if r
			       ;; handled externally
			       (continue/quit results sid r)
			       ;; redirect handler should push
			       ;; new stream
			       (continue/quit results sid #f)))
			 (continue/quit (acons sid (cons headers #f) 
					       results) #f #f)))))
		((http2-frame-data? frame)
		 (let ((r (call-receiver sid stream results frame)))
		   (when (http2-frame-end-stream? frame)
		     (terminate-stream sid)
		     ;; TODO should we raise an error
		     ;; if there is no slot created?
		     (cond ((assv sid results) =>
			    (lambda (slot) (set-cdr! (cdr slot) r)))))
		   (continue/quit results #f #f)))
		(else
		 (error 'http2-request "frame not supported" frame)))))))

  ;; convenient macro
  (define-syntax http2-request
    (syntax-rules ()
      ((_ conn method uri sender receiver opts ...)
       (let ((c conn)
	     (s sender)
	     (r receiver))
	 (http2-add-request! c method uri s r opts ...)
	 (http2-invoke-requests! c)))))

  ;; TODO 
  (define-syntax http2-multi-requests
    (syntax-rules (GET POST HEAD)
      ((_ conn "clause" (GET uri headers ...) rest ...)
       ;; TODO how should we handle other receivers?
       (begin
	 (http2-add-request! conn 
			     'GET uri
			     (http2-headers-sender headers ...)
			     (http2-binary-receiver))
	 (http2-multi-requests conn "clause" rest ...)))
      ;; finish
      ((_ conn "clause") (http2-invoke-requests! conn))
      ;; entry point
      ((_ conn (clause ...) rest ...)
       (http2-multi-requests conn "clause" (clause ...) rest ...))))
	 
  ;; helper
  (define (http2-construct-header stream . extras)
    (define conn (http2-stream-connection stream))
    (define ->bv string->utf8)
    (let1 secure? (%h2-secure? conn)
      `((#*":method" ,(->bv (symbol->string (http2-stream-method stream))))
	(#*":scheme" ,(if secure? #*"https" #*"http"))
	(#*":path"   ,(->bv (http2-stream-uri stream)))
	(#*":authority" ,(->bv (http2-client-connection-server conn)))
	(#*"user-agent" ,(->bv (%h2-agent conn)))
	,@(let loop ((h extras) (r '()))
	    (match h
	      ;; compatible format (:key "value")
	      ;; may be we should accept all expressions and
	      ;; convert them using format and string->utf8?
	      (((? keyword? name) value rest ...)
	       (loop rest
		     (cons (list (->bv (keyword->string name))
				 (->bv value)) r)))
	      (((? bytevector? name) (? bytevector? value) rest ...)
	       (loop rest (cons (list name value) r)))
	      (() (reverse! r))
	      (else
	       (error 'http2-construct-header "invalid header" extras)))))))

;;; senders
  ;; TODO should senders look like this?
  ;;      it might be better to wrap frames a bit for cutome senders
  ;;      so that users don't have to import (rfc http2 frame). but 
  ;;      for now.
  (define (http2-headers-sender . headers)
    (lambda (stream end-stream?)
      (http2-stream-header-sender-set! stream
       (lambda (stream conv)
	 (let* ((headers (apply http2-construct-header stream (conv headers)))
		(id (http2-stream-identifier stream))
		(frame (make-http2-frame-headers 0 id #f #f headers)))
	   (http2-write-stream stream frame end-stream?))
	 ;; ok it's ugly but we can check now :)
	 (http2-stream-header-sender-set! stream #f)))))
  
  (define (http2-data-sender data)
    (lambda (stream end-stream?)
      (when (http2-stream-need-header? stream)
	(http2-stream-send-header stream values))
      (let ((frame (make-http2-frame-data 0 
					  (http2-stream-identifier stream)
					  data)))
	(http2-write-stream stream frame end-stream?))))

  (define (make-stream-write-port stream end-stream? buffer-size)
    (define buffer (make-bytevector buffer-size))
    (define sid (http2-stream-identifier stream))
    (define pos 0)
    (define (flush-buffer! end-stream? buffer)
      (let ((frame (make-http2-frame-data 0 sid buffer)))
	(http2-write-stream stream frame end-stream?)))
    (define (write! bv start count)
      (let loop ((i start) (c count))
	(if (zero? c)
	    count
	    (let ((size (min c (- buffer-size pos))))
	      (bytevector-copy! bv i buffer pos size)
	      (set! pos (+ pos size))
	      (when (= pos buffer-size)
		(flush-buffer! #f buffer)
		(set! pos 0))
	      (loop (+ i size) (- c size))))))
    (define (close!)
      (define (copy-buffer buffer)
	(if (zero? pos)
	    #vu8()
	    (bytevector-copy buffer 0 pos)))
      (flush-buffer! end-stream? (copy-buffer buffer)))
    (make-custom-binary-output-port "stream-write-port" write! #f #f close!))
  
  (define multipart-transcoder (make-transcoder (latin-1-codec) 'none))
  (define (adjust-for-multipart boundary)
    (lambda (headers)
      (define ->bv string->utf8)
      (define (reconstruct headers boundary)
	(define (content-type? v)
	  (or (eq? v :content-type)
	      (and (bytevector? v)
		   (bytevector=? #*"content-type" v))))
	(let loop ((r '()) (h headers) (ct? #f))
	  (match h
	    (()
	     (cons* #*"mime-version" #*"1.0"
		    (if ct? r (cons* #*"content-type" boundary r))))
	    (((? content-type? ct) value rest ...)
	     (loop (if ct? r (cons* #*"content-type" boundary r)) rest #t))
	    ((name value rest ...)
	     (loop (cons* name value r) rest ct?)))))
	    
      (let1 b (string-append "multipart/form-data; boundary=\"" boundary "\"")
	(reconstruct headers (->bv b)))))
    
  (define (http2-multipart-sender parts
				  :key (buffer-size 4096)
				       (boundary (mime-make-boundary)))
    (lambda (stream end-stream?)
      ;; ok hope user sent it properly then
      (when (http2-stream-need-header? stream)
	(http2-stream-send-header stream (adjust-for-multipart boundary)))
      (let ((out (transcoded-port
		  (make-stream-write-port stream end-stream? buffer-size)
		  multipart-transcoder)))
	(mime-compose-message parts out :boundary boundary)
	(close-output-port out))))
  
  (define (http2-composite-sender . senders)
    (lambda (stream end-stream?)
      (let loop ((senders senders))
	(unless (null? senders)
	  (cond ((null? (cdr senders))
		 ((car senders) stream end-stream?))
		(else 
		 ((car senders) stream #f)
		 (loop (cdr senders))))))))

;;; receivers
  ;; TODO better handling...
  (define (http2-data-receiver sink flusher)
    (lambda (stream headers frame end-stream?)
      (when (http2-frame-data? frame)
	(put-bytevector sink (http2-frame-data-data frame)))
      (and end-stream? (flusher sink))))

  (define (http2-binary-receiver)
    (let ((out (open-output-bytevector)))
      (http2-data-receiver out get-output-bytevector)))

  ;; do nothing :)
  (define (http2-null-receiver) 
    (lambda (stream header frame end-stream?) #vu8()))

  (define (http2-gzip-receiver receiver)
    (let ((in/out (open-chunked-binary-input/output-port)))
      (lambda (stream headers frame end-stream?)
	(define (handle-compressed-stream)
	  (define (call-next)
	    (set-port-position! in/out 0)
	    (let* ((gin (open-gzip-input-port in/out :owner? #t))
		   (frame (make-http2-frame-data 0 stream
						 (get-bytevector-all gin))))
	      (receiver stream headers frame #t)))
	  (when (http2-frame-data? frame)
	    (put-bytevector in/out (http2-frame-data-data frame)))
	  (and end-stream? (call-next)))
	(if (equal? (http2-header-ref #*"content-encoding" headers) #*"gzip")
	    (handle-compressed-stream)
	    (receiver stream headers frame end-stream?)))))
  
  ;; TODO more receivers


;;; common APIs
  (define (make-gzip-receiver)
    (http2-gzip-receiver (http2-binary-receiver)))
  ;; GET
  (define (http2-get conn uri 
		     :key (receiver (make-gzip-receiver))
			  (redirect-handler http2-redirect-handler)
		     :allow-other-keys headers)
    ;; discards the stored streams first
    (when (http2-has-pending-streams? conn) (http2-invoke-requests! conn))
    (http2-add-request! conn 'GET uri (apply http2-headers-sender headers)
			receiver
			:redirect-handler redirect-handler)
    (apply values (car (http2-invoke-requests! conn))))

  ;; HEAD
  (define (http2-head conn uri 
		      :key (receiver (make-gzip-receiver))
			   (redirect-handler http2-redirect-handler)
		     :allow-other-keys headers)
    ;; discards the stored streams first
    (when (http2-has-pending-streams? conn) (http2-invoke-requests! conn))
    (http2-add-request! conn 'HEAD uri (apply http2-headers-sender headers)
			receiver
			:redirect-handler redirect-handler)
    (apply values (car (http2-invoke-requests! conn))))

  ;; POST (not properly tested)
  (define (http2-post conn uri data 
		      :key (receiver (make-gzip-receiver))
			   (redirect-handler http2-redirect-handler)
		      :allow-other-keys headers)
    ;; discards the stored streams first
    (when (http2-has-pending-streams? conn) (http2-invoke-requests! conn))
    (let ((size (number->string (bytevector-length data))))
      (http2-add-request! conn 'POST uri
			  (http2-composite-sender 
			   (apply http2-headers-sender
				  :content-length size
				  headers)
			   (http2-data-sender data))
			  receiver
			  :redirect-handler redirect-handler)
      (apply values (car (http2-invoke-requests! conn)))))


;;; handlers
  ;; redirect
  ;; TODO redirect history
  (define (http2-redirect-handler stream headers)
    ;; get location
    (define (get-location headers)
      ;; for now only header
      (cond ((http2-header-ref #*"location" headers) => utf8->string)
	    (else #f)))
    (let ((loc (get-location headers))
	  (receiver (http2-stream-receiver stream)))
      (let-values (((s ui host port path q f) (uri-parse loc)))
	;; update stream receiver to null receiver
	(http2-stream-receiver-set! stream (http2-null-receiver))
	(if (and host (not (string=? host
				     (http2-client-connection-server
				      (http2-stream-connection stream)))))
	    (let ((conn (make-http2-client-connection 
			 host (if port (number->string port) "80")
			 :secure? (and s (string=? s "https")))))
	      ;; we use the same http2 redirect handler
	      (http2-request conn
			     (http2-stream-method stream)
			     path
			     (http2-stream-sender stream)
			     receiver))
	    ;; make new stream
	    (let ((s (http2-open-stream (http2-stream-connection stream)
					(http2-stream-method stream)
					path
					(http2-stream-sender stream)
					receiver)))
	      ;; invoke sender
	      ;; TODO shouls we handle this in `http2-invoke-requests!`?
	      ((http2-stream-sender stream) s #t)
	      #f)))))

;;; helpers
  (define (http2-header-ref bv headers)
    (cond ((assoc bv headers bytevector=?) => cadr)
	  (else #f)))

  )
