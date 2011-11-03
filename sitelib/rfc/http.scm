;;; -*- Scheme -*-
;;;
;;; http.scm - HTTP protocol library.
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

(library (rfc http)
    (export &http-error
	    http-error?

	    ;; connection
	    http-connection?
	    make-http-connection

	    http-user-agent
	    http-compose-query
	    http-compose-form-data

	    http-get
	    http-head
	    http-port
	    http-put
	    http-delete
	    http-request)
    (import (rnrs)
	    (core)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius socket)
	    (sagittarius control)
	    (sagittarius partcont)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (match)
	    (encoding decoder)
	    (util list)
	    (rfc :5322)
	    (rfc uri)
	    (rfc mime))

  (define-condition-type &http-error &error
    make-http-error http-error?)
  
  (define (raise-http-error who msg . irritants)
    (raise (apply condition
		  (filter values
			  (list (make-http-error)
				(and who (make-who-condition who))
				(make-message-condition msg)
				(make-irritants-condition irritants))))))

  (define-record-type http-connection
    (fields (mutable server)		; server[:port]
	    (mutable socket)		; A socket for persistent connection.
	    (mutable secure-agent)
	    (mutable proxy)
	    (mutable extra-headers)
	    (mutable secure)
	    (mutable auth-handler)	; unused
	    (mutable auth-user)		; unused
	    (mutable auth-password)	; unused
	    )
    (protocol (lambda (p)
		(lambda (server . args)
		  (let-keywords args ((socket #f)
				      (secure-agent #f)
				      (proxy #f)
				      (extra-headers '())
				      (secure #f)
				      (auth-handler http-default-auth-handler)
				      (auth-user #f)
				      (auth-password #f))
		    (p server socket secure-agent proxy extra-headers
		       secure auth-handler auth-user auth-password))))))

  (define http-user-agent (make-parameter (format "sagittarius.http/~a" (sagittarius-version))))
  ;; redirect
  (define (redirect conn proto new-server)
    (let1 orig-server (http-connection-server conn)
      (unless (and (string=? orig-server new-server)
		   (eq? (http-connection-secure conn) (equal? proto "https")))
	(shutdown-secure-agent conn)
	(and-let* ((s (http-connection-socket conn)))
	  (socket-shutdown s)
	  (socket-close s)
	  (http-connection-socket-set! conn #f))
	(http-connection-server-set! conn new-server)
	(http-connection-secure-set! conn (equal? proto "https"))))
    conn)

  ;; 
  (define-with-key (http-request method server request-uri
				 :key (host #f)
				      (no-redirect #f)
				      auth-handler
				      auth-user
				      auth-password
				      proxy
				      extra-headers
				      (user-agent (http-user-agent))
				      (secure #f)
				      (receiver (http-string-receiver))
				      (sender #f)
				      (enc :request-encoding 'utf-8)
				  :allow-other-keys otps)
    (let1 conn (ensure-connection server auth-handler auth-user auth-password
				  proxy secure extra-headers)
      (let loop ((history '())
		 (host host)
		 (request-uri (ensure-request-uri request-uri enc)))
	(receive (code headers body)
	    (request-response method conn host request-uri sender receiver
			      `(:user-agent ,user-agent) enc)
	  (or (and-let* (( (not no-redirect) )
			 ( (string-prefix? "3" code) )
			 (loc (assoc "location" headers)))
		(receive (uri proto new-server path*)
		    (canonical-uri conn (cadr loc) (http-connection-server conn))
		  (when (or (member uri history)
			    (> (length history) 20))
		    (raise-http-error 'http-request
				      (format "redirection is looping via ~a" uri)
				      (http-connection-server conn)))
		  (loop (cons uri history)
			(http-connection-server (redirect conn proto new-server))
			path*)))
	      (values code headers body))))))

  (define *server-re* (regex "([^:]+):(\\d+)"))

  (define (server->socket server)
    (cond ((matches *server-re* server)
	   => (lambda (m) 
		(make-client-socket (m 1) (m 2))))
	  (else (make-client-socket server "80"))))

  (define (with-connection conn proc)
    (cond ((http-connection-secure conn)
	   (raise-http-error 'with-connection
			     "secure connection is not supported yet"
			     (http-connection-server conn)))
	  (else
	   (let1 s (server->socket (or (http-connection-proxy conn)
				       (http-connection-server conn)))
	     (dynamic-wind
	       (lambda () #t)
	       (lambda () (proc (transcoded-port (socket-port s) (make-transcoder (utf-8-codec) 'lf))))
	       (lambda ()
		 (socket-close s)))))))

  (define (request-response method conn host request-uri
			    sender receiver options enc)
    (define no-body-replies '("204" "304"))
    (receive (host uri)
	(consider-proxy conn (or host (http-connection-server conn)) request-uri)
      (with-connection 
       conn
       (lambda (in/out)
	 (send-request in/out method host uri sender options enc)
	 (receive (code headers) (receive-header in/out)
	   (values code
		   headers
		   (and (not (eq? method 'HEAD))
			(not (member code no-body-replies))
			(receive-body in/out code headers receiver))))))))

  (define (canonical-uri conn uri host)
    (let*-values (((scheme specific) (uri-scheme&specific uri))
		  ((h p q f) (uri-decompose-hierarchical specific)))
      (let ((scheme (or scheme (if (http-connection-secure conn) "https" "http")))
	    (host (or h host)))
	(values (uri-compose :scheme scheme :host host
			      :path p :query q :fragment f)
		scheme
		host
		;; drop "//"
		(string-drop (uri-compose :path p :query q :fragment f) 2)))))
  
  (define (consider-proxy conn host uri)
    (if (http-connection-proxy conn)
	(values host (uri-compose :scheme "http" :host (http-connection-secure conn) :path* uri))
	(values host uri)))

  ;; send
  (define (send-request out method host uri sender options enc)
    ;; this is actually not so portable. display requires textual-port but socket-port is
    ;; binary-port. but hey!
    ;;(display out (standard-error-port))(newline)
    (format out "~a ~a HTTP/1.1\r\n" method uri)
    (case method
      ((POST PUT)
       (sender (options->request-headers `(:host ,host @options)) enc
	       (lambda (hdrs)
		 (send-headers hdrs out)
		 (let ((chunked? (equal? (rfc5322-header-ref hdrs "transfer-encoding")
					 "chunked"))
		       (first-time #t))
		   (lambda (size)
		     (when chunked?
		       (unless first-time (display "\r\n" out))
		       (format out "~x\r\n" size))
		     (flush-output-port out)
		     out)))))
      (else
       (send-headers (options->request-headers `(:host ,host ,@options)) out))))

  (define (send-headers hdrs out)
    (for-each (lambda (hdr)
		(format out "~a: ~a\r\n" (car hdr) (cadr hdr)))
	      hdrs)
    (display "\r\n" out)
    (flush-output-port out))

  (define (options->request-headers options)
    (let loop ((options options) (r '()))
      (if (or (null? options) (null? (cdr options)))
	  (reverse r)
	  (loop (cddr options)
		`((,(format "~a" (car options)) ,(format "~a" (cadr options))) ,@r)))))


  ;; receive
  (define (receive-header remote)
    (receive (code reason) (parse-status-line (get-line remote))
      (values code (rfc5322-read-headers remote))))

  (define *status-re* (regex "[\\w/.]+\\s+(\\d\\d\\d)\\s+(.*)"))
  (define (parse-status-line line)
    (cond ((eof-object? line)
	   (raise-http-error 'parse-status-line
			     "http reply contains no data"))
	  ((looking-at *status-re* line)
	   => (lambda (m) (values (m 1) (m 2))))
	  (else (raise-http-error 'parse-status-line
				  "bad reply from server"
				  line))))

  (define (receive-body remote code headers receiver)
    (let* ((total (and-let* ((p (assoc "content-length" headers)))
		    (string->number (cadr p))))
	   (handler (reset (receiver code headers total (lambda () (shift k k))))))
      (cond ((assoc "transfer-encoding" headers)
	     => (lambda (p)
		  (unless (equal? (cadr p) "chunked")
		    (raise-http-error 'receive-body
				      "unsupported transfer-encoding"
				      (cadr p)))
		  (receive-body-chunked remote handler)))
	    (total
	     (when (> total 0) (set! handler (handler remote total)))
	     (handler remote 0))
	    (else
	     ;; length is unknown
	     (let* ((content (get-string-all remote))
		    (size (string-length size content))
		    (p (open-string-input-port content)))
	       (when (> size 0) (set! handler (handler p size)))
	       (handler p 0))))))

  (define (receive-body-chunked remote handler)
    (define *regexp* (regex "^([0-9a-fA-F]+)"))
    (guard (e (else (handler remote -1) (raise e)))
      (let loop ((line (get-line remote)))
	(when (eof-object? line)
	  (raise-http-error 'receive-body-chunked
			    "chunked body ended prematurely"))
	(cond ((looking-at *regexp* line)
	       => (lambda (m)
		    (let* ((digits (m 1))
			   (chunk-size (string->number digits 16)))
		      (if (zero? chunk-size)
			  (do ((line (get-line remote) (get-line remote)))
			      ((or (eof-object? line) (string-null? line))
			       (handler remote 0))
			    #f)
			  (begin
			    (set! handler (handler remote chunk-size))
			    (get-line remote) ; skip the following CRLF
			    (loop (get-line remote)))))))
	      (else
	       ;; something is wrong
	       (raise-http-error 'receive-body-chunked
				 "bad line in chunked data"
				 line))))))

  ;; secure
  ;; dummy
  (define (shutdown-secure-agent conn) #f)
	
  (define (lookup-encoding hdrs)
    (or (and-let* ((c (rfc5322-header-ref hdrs "content-type"))
		   ( c )
		   (params (mime-parse-content-type c))
		   ( (list? (cddr params)) )
		   (attr (cddr params))
		   (charset (assoc "charset" attr))
		   ( charset ))
	  (lookup-decoder (cdr charset)))
	(utf-8-codec)))

  ;; pre-defined receivers
  (define (http-string-receiver)
    (lambda (code hdrs total retr)
      (let loop ((sink (open-output-bytevector)))
	(receive (remote size) (retr)
	  (cond ((= size 0) 
		 (bytevector->string (get-output-bytevector sink)
				     (make-transcoder (lookup-encoding hdrs))))
		((> size 0)
		 (put-bytevector sink (get-bytevector-n remote size #t))
		 (loop sink)))))))

  ;; sink must be binary-port
  (define (http-oport-receiver sink flusher)
    (check-arg binary-port? sink 'http-oport-receiver)
    (lambda (code hdrs totla retr)
      (let loop ()
	(receive (remote size) (retr)
	  (cond ((= size 0) (flushre sink hdrs))
		((> size 0)
		 (put-bytevector sink (get-bytevector-n remote size #t))
		 (loop)))))))

  ;; query and request body composition
  (define-optional (http-compose-query path params (optional (encoding 'utf-8)))
    (define (esc s) (uri-encode-string (format "~a" s) encoding))
    (define (query-1 n&v)
      (match n&v
	((name value) (format "~a=~a" (esc name) (esc value)))
	(_ (assertion-violation 'http-compose-query
				"invalid request-uri form" params))))
    (define (query) (string-concatenate (intersperse "&" (map query-1 params))))
    (cond ((not path) (query))
	  ((null? params) path)
	  (else (format "~a?~a" path (query)))))

  (define (ensure-request-uri request-uri enc)
    (match request-uri
      ((? string?) request-uri)
      ((path n&v ...) (http-compose-query path n&v enc))
      (_ (error "Invalid request-uri form for http request API" request-uri))))

  (define (ensure-connection server auth-handler auth-user auth-password
			     proxy secure extra-headers)
    (let1 conn (cond ((http-connection? server) server)
		     ((string? server) (make-http-connection server))
		     (else
		      (raise-http-error 'ensure-connection
					"bad type of argument for server: must be an <http-connection> object or a string of the server's name" 
					server)))
      (let-syntax ((check-override
		    (er-macro-transformer
		     (lambda (f r c)
		       (let ((id (cadr f)))
			 `(unless (undefined? ,id)
			    (,(string->symbol (string-append "http-connection-"
							     (symbol->string (identifier->symbol id))
							     "-set!"))
			     conn
			     ,id)))))))
	(check-override auth-handler)
	(check-override auth-user)
	(check-override auth-password)
	(check-override proxy)
	(check-override extra-headers)
	(check-override secure)
	conn)))
		     

  ;; shortcuts for specific requests
  (define (http-get server request-uri . options)
    (apply %http-request-adaptor 'GET server request-uri #f options))

  (define (http-head server request-uri . options)
    (apply %http-request-adaptor 'HEAD server request-uri #f options))

  (define (http-post server request-uri body . options)
    (apply %http-request-adaptor 'POST server request-uri #f options))

  (define (http-put server request-uri body . options)
    (apply %http-request-adaptor 'PUT server request-uri #f options))

  (define (http-delete server request-uri . options)
    (apply %http-request-adaptor 'DELETE server request-uri #f options))

  (define-with-key (%http-request-adaptor method server request-uri body
					  :key receiver (sink #f) (flusher #f)
					  :allow-other-keys opts)
    (define recvr
      (if (or sink flusher)
	  (http-oport-receiver (or sink (open-output-string))
			       (or flusher (lambda (s h) (get-output-string s))))
	  receiver))
    (apply http-request method server request-uri
	   :sender (cond ((not body) (http-null-sender))
			 ((list? body) (http-multipart-sender body))
			 (else (http-blob-sender body)))
	   :receiver recvr opts))

  ;; senders
  (define (http-null-sender)
    (lambda (hdrs encoding header-sink)
      (let ((body-sink (header-sink `(("content-length" "0") ,@hdrs))))
	(body-sink 0))))

  (define (http-blob-sender blob)
    (lambda (hdrs encoding header-sink)
      (let* ((data (if (string? blob) (string->utf8 blob) blob))
	     (size (bytevector-length data))
	     ;; TODO add "content-type: type/subtype; charset=utf-8" when blob was string
	     (body-sink (header-sink `(("content-length" ,(format "~a" size))
				       ,@hdrs)))
	     (port (body-sink size)))
	(put-bytevector port data)
	(body-sink 0))))

  ;; authentication handling

  ;; dummy
  (define (http-default-auth-handler . _) #f)

)
