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
    (export http2-connection?
	    socket->http2-connection)
    (import (rnrs)
	    (net http-client connection)
	    (net http-client request)
	    (net socket)
	    (net uri)
	    (rfc http2 frame)
	    (rfc http2 conditions)
	    (rfc http2 hpack)
	    (srfi :18 multithreading))
  
(define-record-type http2-connection
  (parent <http-connection>)
  (fields streams
	  request-hpack-context
	  response-hpack-context
	  buffer
	  mutex
	  (mutable next-id)
	  (mutable window-size))
  (protocol (lambda (n)
	      (lambda (socket option node service)
		((n node service option socket http2-request http2-response)
		 (make-eqv-hashtable)
		 (make-hpack-context 4096)
		 (make-hpack-context 4096)
		 (make-frame-buffer)
		 (make-mutex)
		 1
		 65535)))))

(define-enumeration http2:stream-state
  (idel reserved open half-closed closed)
  ;; won't be used ;)
  http2-stream-state-set)
  
(define-record-type http2-stream
  (fields id
	  connection
	  header-handler
	  data-handler
	  (mutable state)
	  (mutable window-size))
  (protocol (lambda (p)
	      (lambda (connection header-handler data-handler)
		(define id (http2-next-stream-id connection))
		(let ((r (p id connection header-handler data-handler
			    (http2:stream-state idel)
			    (http2-connection-window-size connection))))
		  (hashtable-set! (http2-connection-streams connection) id r)
		  r)))))

(define +client-window-size+ (- (expt 2 31) 1))
;;; API
(define (socket->http2-connection socket socket-option node service)
  (let ((conn (make-http2-connection socket socket-option node service)))
    (http2-send-preface conn)
    (http2-send-settings conn 0
     `(;; for now, we don't handle push
       (,+http2-settings-enable-push+ 0)
       (,+http2-settings-initial-window-size+ ,+client-window-size+)
       (,+http2-settings-max-concurrent-streams+ 100)))
    conn))

;;; API
;; this must be done asynchronousely by client (otherwise blocks)
(define (http2-request connection request header-handler data-handler)
  (define stream (make-http2-stream connection header-handler data-handler))
  ;; send request here
  (define header-frame (stream:request->header-frame stream request))
  (define data-frame-sender (stream:request->data-frame-sender stream request))

  (http2-write-stream stream header-frame (and (not data-frame-sender) #t))
  (when data-frame-sender (data-frame-sender))
  (http2-stream-state-set! stream (http2:stream-state half-closed))
  connection)

;;; API
;; http2-request will only send a request, to receive a response,
;; this is needed
(define (http2-response connection)
  (define streams (http2-connection-streams connection))
  
  (define (send-frame connection frame end?)
    (define out (http-connection-output connection))
    (write-http2-frame out (http2-connection-buffer connection)
		       frame end?
		       (http2-connection-request-hpack-context connection)))
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
  (define (send-rst-stream connection code stream-id)
    (send-frame connection (make-http2-frame-rst-stream 0 stream-id code) #t)
    (http2-remove-stream! connection stream-id))
  
  (define (read-frame connection)
    (define in (http-connection-input connection))
    (define res-hpack (http2-connection-response-hpack-context connection))
    (guard (e ((http2-error? e)
	       ;; treat as connection error.
	       (send-goaway connection e (http2-error-code e) 0))
	      (else
	       ;; internal error. just close the session
	       (send-goaway connection e +http2-error-code-internal-error+ 0)))
      (read-http2-frame in (http2-connection-buffer connection) res-hpack)))
  
  (define used-window-sizes (make-eqv-hashtable))
  (let loop ()
    (unless (zero? (hashtable-size streams))
      (let* ((frame (read-frame connection))
	     (sid (http2-frame-stream-identifier frame))
	     (stream (hashtable-ref streams sid #f))
	     (es? (http2-frame-end-stream? frame)))
	(when es? (http2-remove-stream! connection sid))
	(cond ((http2-frame-settings? frame)
	       (http2-apply-server-settings connection frame)
	       (loop))
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
	       (http2-remove-stream! connection sid) ;; in case
	       (loop))
	      ((http2-frame-window-update? frame)
	       (let ((size
		      (http2-frame-window-update-window-size-increment frame)))
		 (cond ((zero? size)
			(send-rst-stream connection
					 +http2-error-code-protocol-error+
					 sid))
		       ((zero? sid)
			(http2-connection-window-size-set! connection size))
		       (else
			(http2-stream-window-size-set! stream size))))
	       (loop))
	      ((http2-frame-headers? frame)
	       (let ((headers (map (lambda (kv)
				     (list (utf8->string (car kv))
					   (utf8->string (cadr kv))))
				   (http2-frame-headers-headers frame)))
		     (header-handler (http2-stream-header-handler stream)))
		 (header-handler (assoc ":status" headers) headers)
		 (loop)))
	      ((http2-frame-data? frame)
	       (let ((data (http2-frame-data-data frame))
		     (data-handler (http2-stream-data-handler stream)))
		 (data-handler data es?)
		 (hashtable-update! used-window-sizes sid
		   (lambda (v)
		     (let* ((size (bytevector-length data))
			    (new-size (+ v size)))
		       (cond ((>= new-size +client-window-size+)
			      (send-frame connection
					  (make-http2-frame-window-update
					   0 sid +client-window-size+) #t)
			      0)
			     (else new-size))))
		   0)
		 (loop)))
	      (else (assertion-violation 'http2-response "Unknown frame type"
					 frame))))))
  ;; return connection as this can be reusable
  connection)
  

;;; helpers
(define (stream:request->data-frame-sender stream request)
  ;; for now
  #f)

(define (stream:request->header-frame stream request)
  (define id (http2-stream-id stream))
  (define (request->path request)
    (define uri (http:request-uri request))
    (let ((path (uri-path uri))
	  (query (uri-query uri)))
      ;; encode?
      (if query
	  (string-append path "?" query)
	  path)))
  (define (request->headers stream request)
    (define conn (http2-stream-connection stream))
    (define options (http-connection-socket-options conn))
    (define headers (http:request-headers request))
    (define ->bv string->utf8)
    (define (headers->http2-headers headers)
      (define (->lower s) (string-downcase s))
      ;; headers is multi valued map
      (map (lambda (name)
	     (let ((k (->lower name)))
	       (if (or (string=? k "user-agent") ;; skip these
		       (string=? k "content-type"))
		   '()
		   (map (lambda (v) (list (->bv name) (->bv v)))
			(http:headers-ref* headers name)))))
	   (http:headers-names headers)))
      
    `((#*":method" ,(->bv (symbol->string (http:request-method request))))
      (#*":scheme" ,(if (tls-socket-options? options) #*"https" #*"http"))
      (#*":authority" ,(->bv (http-connection-node conn)))
      (#*":path"   ,(->bv (request->path request)))
      ,@(cond ((and (http:request-body request)
		    (http:request-content-type request))
	       => (lambda (type)
		    `((#*"content-type" ,(->bv type)))))
	      (else '()))
      (#*"user-agent" ,(->bv (or (http:headers-ref headers "user-agent")
				 (http-connection-user-agent conn))))
      ,@(apply append (headers->http2-headers headers))))
  (let ((headers (request->headers stream request)))
    (make-http2-frame-headers 0 id #f #f headers)))

(define (http2-write-stream stream frame end?)
  (define conn (http2-stream-connection stream))
  (define out (http-connection-output conn))
  (write-http2-frame out (http2-connection-buffer conn)
		     frame end?
		     (http2-connection-request-hpack-context conn)))

(define (http2-remove-stream! conn id)
  (define streams (http2-connection-streams conn))
  (hashtable-delete! streams id))

(define (http2-next-stream-id conn)
  ;; I don't think with this http-client we share connection among threads
  ;; so this may not needed, but not sure at this moment
  (mutex-lock! (http2-connection-mutex conn))
  (let ((next-id (http2-connection-next-id conn)))
    (http2-connection-next-id-set! conn (+ next-id 2))
    (mutex-unlock! (http2-connection-mutex conn))
    next-id))

(define +http2-preface+ #*"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")
(define (http2-send-preface conn)
  (let ((out (http-connection-output conn)))
    (put-bytevector out +http2-preface+)))
(define (http2-send-settings conn flags settings)
  (let ((out (http-connection-output conn)))
    (write-http2-frame out (http2-connection-buffer conn)
		       (make-http2-frame-settings flags 0 settings)
		       ;; we know context is not needed and 
		       ;; this won't be the end of stream
		       #f #f)))
(define (http2-handle-server-settings conn)
  (let ((in (http-connection-input conn)))
    (http2-apply-server-settings 
     conn (read-http2-frame in (http2-connection-buffer conn) #f))))

(define (http2-apply-server-settings conn frame)
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
			(http2-connection-request-hpack-context conn) 
			(cadar settings)))
		      ((= (caar settings)
			  +http2-settings-initial-window-size+)
		       (http2-connection-window-size-set! conn (cdar settings))))
		(loop (cdr settings)))))))


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
