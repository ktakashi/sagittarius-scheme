;;; -*- Scheme -*-
;;;
;;; http.scm - HTTP protocol library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/regex
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
	    http-post
	    http-put
	    http-delete
	    http-request

	    ;; senders
	    http-null-sender
	    http-multipart-sender
	    http-blob-sender
	    http-string-sender
	    ;; receiver
	    http-string-receiver
	    http-null-receiver
	    http-oport-receiver
	    http-file-receiver
	    http-cond-receiver

	    http-default-redirect-handler
	    ;; for convenience
	    http-lookup-auth-handler
	    
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius socket)
	    (sagittarius control)
	    (clos user)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (match)
	    (encoding decoder)
	    (util list)
	    (util port)
	    (util file)
	    (rfc :5322)
	    (rfc uri)
	    (rfc mime)
	    (rfc base64)
	    (rfc tls))

  (define-condition-type &http-error &error
    make-http-error http-error?)
  
  (define (raise-http-error who msg . irritants)
    (raise (apply condition
		  (filter values
			  (list (make-http-error)
				(and who (make-who-condition who))
				(make-message-condition msg)
				(make-irritants-condition irritants))))))

  (define-class <http-connection> ()
    (;; server[:port]
     (server :init-keyword :server :init-value #f
	     :accessor http-connection-server)
     ;; A socket for persistent connection.
     (socket :init-keyword :socket :init-value #f
	     :accessor http-connection-socket)
     (secure-agent :init-keyword :secure-agent :init-value #f
		   :accessor http-connection-secure-agent)
     (proxy  :init-keyword :proxy :init-value #f
	     :accessor http-connection-proxy)
     (extra-headers :init-keyword :extra-headers :init-value '()
		    :accessor http-connection-extra-headers)
     (secure :init-keyword :secure :init-value #f
	     :accessor http-connection-secure)
     (auth-handler :init-keyword :auth-handler :init-value #f
		   :accessor http-connection-auth-handler)
     (auth-user :init-keyword :auth-user :init-value #f
		:accessor http-connection-auth-user)
     (auth-password :init-keyword :auth-password :init-value #f
		    :accessor http-connection-auth-password)))
  (define (make-http-connection server . args)
    (apply make <http-connection> :server server args))
  (define (http-connection? o) (is-a? o <http-connection>))


  (define http-user-agent (make-parameter (format "sagittarius.http/~a"
						  (sagittarius-version))))
  ;; redirect
  (define (redirect conn proto new-server)
    (let1 orig-server (http-connection-server conn)
      (unless (and (string=? orig-server new-server)
		   (eq? (http-connection-secure conn) (equal? proto "https")))
	(http-connection-server conn new-server)
	(http-connection-secure conn (equal? proto "https"))))
    conn)

  (define http-default-redirect-handler
    (make-parameter
     (lambda (method code headers body)
       (and-let* ((loc (rfc5322-header-ref headers "location")))
	 (case (string->number code)
	   ((300 301 305 307)
	    (case method ((GET HEAD) `(,method . ,loc)) (else #f)))
	   ((302 303)
	    (case method
	      ((GET HEAD) `(,method . ,loc))
	      (else `(GET . ,loc))))
	   (else #f))))))
  ;; 
  (define (http-request method server request-uri
			:key (host #f)
			     (redirect-handler #t)
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
			:allow-other-keys opts)
    (let1 conn (ensure-connection server auth-handler auth-user auth-password
				  proxy secure extra-headers)
      (let loop ((history '())
		 (host host)
		 (method method)
		 (request-uri (ensure-request-uri request-uri enc)))
	(receive (code headers body)
	    (request-response method conn host request-uri sender receiver
			      `(:user-agent ,user-agent ,@opts) enc)
	  (or (and-let* (( (not no-redirect) )
			 ( (string-prefix? "3" code) )
			 (h (case redirect-handler
			      ((#t) (http-default-redirect-handler))
			      ((#f) #f)
			      (else redirect-handler)))
			 (r (h method code headers body))
			 (method (car r))
			 (loc (cdr r)))
		(receive (uri proto new-server path*)
		    (canonical-uri conn loc (http-connection-server conn))
		  (when (or (member uri history)
			    (> (length history) 20))
		    (raise-http-error 'http-request
				      (format "redirection is looping via ~a"
					      uri)
				      (http-connection-server conn)))
		  (loop (cons uri history)
			(http-connection-server
			 (redirect conn proto new-server))
			method
			path*)))
	      (values code headers body))))))

  (define (server->socket server port make-socket)
    (cond ((matches #/([^:]+):(\d+)/ server)
	   => (lambda (m) 
		(make-socket (m 1) (m 2))))
	  (else (make-socket server port))))

  (define (invoke-auth-handler conn)
    (and-let* ((handler  (http-connection-auth-handler conn)))
      (let ((user     (http-connection-auth-user conn))
	    (password (http-connection-auth-password conn)))
	(handler user password conn))))

  (define (with-connection conn proc)
    (let* ((secure? (http-connection-secure conn))
	   (make-socket (if secure? make-client-tls-socket make-client-socket))
	   ;;(close-socket (if secure? tls-socket-close socket-close))
	   ;;(port-convert (if secure? tls-socket-port socket-port))
	   (port (if secure? "443" "80")))
      (let ((s (server->socket (or (http-connection-proxy conn)
				   (http-connection-server conn))
			       port make-socket))
	    (auth (invoke-auth-handler conn)))
	(unwind-protect
	 (proc (transcoded-port (socket-port s)
				(make-transcoder (utf-8-codec) 'lf))
	       auth)
	 (socket-close s)))))

  (define (request-response method conn host request-uri
			    sender receiver options enc)
    (define no-body-replies '("204" "304"))
    (receive (host uri)
	(consider-proxy conn (or host (http-connection-server conn))
			request-uri)
      (with-connection 
       conn
       (lambda (in/out auth-header)
	 (send-request in/out method host uri sender auth-header options enc)
	 (receive (code headers) (receive-header in/out)
	   (values code
		   headers
		   (and (not (eq? method 'HEAD))
			(not (member code no-body-replies))
			(receive-body in/out code headers receiver))))))))

  (define (canonical-uri conn uri host)
    (let*-values (((scheme specific) (uri-scheme&specific uri))
		  ((h p q f) (uri-decompose-hierarchical specific)))
      (let ((scheme (or scheme (if (http-connection-secure conn)
				   "https" "http")))
	    (host (or h host)))
	(values (uri-compose :scheme scheme :host host
			     :path p :query q :fragment f)
		scheme
		host
		;; drop "//"
		(string-drop (uri-compose :path p :query q :fragment f) 2)))))
  
  (define (consider-proxy conn host uri)
    (if (http-connection-proxy conn)
	(values host (uri-compose :scheme "http"
				  :host (http-connection-secure conn)
				  :path* uri))
	(values host uri)))

  ;; send
  (define (send-request out method host uri sender auth-headers options enc)
    ;; this is actually not so portable. display requires textual-port but
    ;; socket-port is binary-port. but hey!
    ;;(display out (standard-error-port))(newline)
    (format out "~a ~a HTTP/1.1\r\n" method uri)
    (case method
      ((POST PUT)
       (sender (options->request-headers `(:host ,host ,@options)) enc
	       (lambda (hdrs)
		 (send-headers hdrs out auth-headers)
		 (let ((chunked? (equal? (rfc5322-header-ref
					  hdrs "transfer-encoding")
					 "chunked"))
		       (first-time #t))
		   (lambda (size)
		     (when chunked?
		       (unless first-time (display "\r\n" out))
		       (format out "~x\r\n" size))
		     (flush-output-port out)
		     out)))))
      (else
       (send-headers (options->request-headers `(:host ,host ,@options)) out
		     auth-headers))))

  (define (send-headers hdrs out :optional (auth-header #f))
    (define (send hdrs)
      (for-each (lambda (hdr)
		  (format out "~a: ~a\r\n" (car hdr) (cadr hdr)))
		hdrs))
    (send hdrs)
    (and auth-header (send auth-header))
    (display "\r\n" out)
    (flush-output-port out))

  (define (options->request-headers options)
    (let loop ((options options) (r '()))
      (if (or (null? options) (null? (cdr options)))
	  (reverse r)
	  (loop (cddr options)
		`((,(format "~a" (car options)) 
		   ,(format "~a" (cadr options))) ,@r)))))


  ;; receive
  (define (receive-header remote)
    (receive (code reason) (parse-status-line (get-line remote))
      (values code (rfc5322-read-headers remote))))

  (define (parse-status-line line)
    (cond ((eof-object? line)
	   (raise-http-error 'parse-status-line
			     "http reply contains no data"))
	  ((looking-at #/[\w\/.]+\s+(\d\d\d)\s+(.*)/ line)
	   => (lambda (m) (values (m 1) (m 2))))
	  (else (raise-http-error 'parse-status-line
				  "bad reply from server"
				  line))))

  (define (receive-body remote code headers receiver)
    (let1 total (and-let* ((p (assoc "content-length" headers)))
		  (string->number (cadr p)))
      (cond ((assoc "transfer-encoding" headers)
	     => (lambda (p)
		  (if (equal? (cadr p) "chunked")
		      (receive-body-chunked remote code headers total receiver)
		      (raise-http-error 'receive-body
					"unsupported transfer-encoding"
					(cadr p)))))
	    (else (receive-body-once remote code headers total receiver)))))

  (define (receive-body-once remote code headers total receiver)
    (let1 rest total
      (define (callback)
	(if (equal? rest 0)
	    (values remote 0)
	    (begin (set! rest 0) (values remote total))))
      (receiver code headers total callback)))

  (define (receive-body-chunked remote code headers total receiver)
    (define chunk-size #f)
    (define condition #f)
    (define (callback)
      (if (equal? chunk-size 0)
	  (values remote 0)
	  ;; If we get an error during receiving from the server, we need
	  ;; to return -1 to give the chance to the receiver to clean up
	  ;; things. After the receiver returns we reraise the condition.
	  (guard (e (else (set! condition e) (values remote -1)))
	    ;; If we've already handled some chunks, we need to skip
	    ;; the tailing CRLF of the previous chunk
	    (when chunk-size (get-line remote))
	    (let1 line (get-line remote)
	      (when (eof-object? line)
		(raise-http-error 'receive-body-chunked
				  "chunked body ended prematurely"))
	      (cond ((#/^([0-9a-fA-F]+)/ line)
		     => (lambda (m)
			  (let1 digits (m 1)
			    (set! chunk-size (string->number digits 16))
			    (if (zero? chunk-size)
				(do ((line (get-line remote) (get-line remote)))
				    ((or (eof-object? line) (string-null? line))
				     (values remote 0)))
				(values remote chunk-size)))))
		    (else
		     ;; something is wrong
		     (raise-http-error 'receive-body-chunked
				       "bad line in chunked data"
				       line)))))))
    (begin0 (receiver code headers total callback)
      (when condition raise condition)))

	
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
	  (cond ((eqv? size 0)
		 (bytevector->string (get-output-bytevector sink)
				     (make-transcoder (lookup-encoding hdrs))))
		((or (not size) (> size 0))
		 (copy-binary-port sink remote :size size)
		 (loop sink)))))))

  (define (http-null-receiver)
    (lambda (code hdrs total retr)
      (let loop ((sink (open-file-output-port (null-device)
					      (file-options no-fail))))
	(receive (remote size) (retr)
	  (cond ((and size (<= size)) (close-output-port sink))
		(else (copy-binary-port remote sink :size size)
		      (loop sink)))))))

  ;; sink must be binary-port
  (define (http-oport-receiver sink flusher)
    (check-arg binary-port? sink 'http-oport-receiver)
    (lambda (code hdrs total retr)
      (let loop ()
	(receive (remote size) (retr)
	  (cond ((and size (<= size 0)) (flusher sink hdrs))
		(else
		 (copy-binary-port sink remote :size size)
		 (loop)))))))

  (define (http-file-receiver filename :key (temporary? #f))
    (lambda (code hdrs total retr)
      (receive (port tmpname) (make-temporary-file filename)
	(let loop ()
	  (receive (remote size) (retr)
	    (cond ((or (not size) (> size 0))
		   (copy-binary-port port remote :size size) (loop))
		  ((= size 0)
		   (close-output-port port)
		   (if temporary?
		       tmpname
		       (begin (rename-file tmpname filename) filename)))
		  (else (close-output-port port) (delete-file tmpname))))))))

  (define-syntax http-cond-receiver
    (syntax-rules (else =>)
      ((_) (http-null-receiver))
      ((_ (else . exprs)) (begin . exprs))
      ((_ (cc => proc) . rest)
       (lambda (code hdrs total retr)
	 ((if (match-status-code? cc code)
	      proc
	      (http-cond-receiver . rest))
	  code hdrs total retr)))
      ((_ (cc . exprs) . rest)
       (lambda (code hdrs total retr)
	 ((if (match-status-code? cc code '(cc . exprs))
	      (begin . exprs)
	      (http-cond-receiver . rest))
	  code hdrs total retr)))
      ((_ other . rest)
       (syntax-violation 'http-cond-receiver
			 "invalid clause in http-cond-receiver"
			 other))))

  (define (match-status-code? pattern code clause)
    (cond ((string? pattern)  (equal? pattern code))
	  ((regex-pattern? pattern) (pattern code))
	  (else (error 'match-status-code?
		       "invalid pattern in a clause of http-cond-receiver"
		       clause))))

  ;; query and request body composition
  (define (http-compose-query path params :optional (encoding 'utf-8))
    (define (esc s) (uri-encode-string (format "~a" s) :encoding encoding))
    (define (query-1 n&v)
      (match n&v
	((name value) (format "~a=~a" (esc name) (esc value)))
	(_ (assertion-violation 'http-compose-query
				"invalid request-uri form" params))))
    (define (query) (string-concatenate (intersperse "&" (map query-1 params))))
    (cond ((not path) (query))
	  ((null? params) path)
	  (else (format "~a?~a" path (query)))))

  ;; multipart/form-data composition [RFC2388]
  ;; <params> : (<params> ...)
  ;; <param>  : (<name> <value>)
  ;;          | (<name> <key> <value> <key2> <value2> ...)
  ;; <key>    : :value | :file | :content-type | :content-transfer-encoding
  ;;          | other keyword (used as a header name)
  (define (http-compose-form-data params port
				  :optional (encoding 'utf-8))
    (define (translate-param param)
      (match param
	((name value) (translate-param `(,name :value ,value)))
	((name . kvs)
	 (unless (even? (length kvs))
	   (assertion-violation
	    'http-compose-form-data
	    "invalid parameter format to create multipart/form-data") param)
	 (let-keywords kvs ((value "")
			    (file #f)
			    (content-type #f)
			    (content-transfer-encoding #f) . other-keys)
	   `(,(canonical-content-type (mime-parse-content-type content-type)
				      value file)
	     (("content-transfer-encoding" 
	       ,(or content-transfer-encoding "binary"))
	      ("content-disposition" ,(make-content-disposition name file))
	      ,@(map (lambda (x) (format "~a" x)) (slices other-keys 2)))
	     ,(if file `(file ,file) (format "~a" value)))))))
    (define (canonical-content-type ct value file)
      (match ct
	(#f (if (or file (bytevector? value))
		'("application" "octet-stream")
		`("text" "plain" ("charset" . ,(symbol->string encoding)))))
	((type subtype . options)
	 (if (assoc "charset" options)
	     ct
	     `(,type ,subtype ("charset" . 
			       ,(format "~a" encoding)) ,@options)))))
    (define (make-content-disposition name file)
      (call-with-string-output-port
	(lambda (out)
	  (display "form-data" out)
	  (mime-compose-parameters `(("name" . ,name)
				     ,@(cond-list 
					(file `("filename" . ,file))))
				   out))))
    (if (not port)
	(mime-compose-message-string (map translate-param params))
	(mime-compose-message (map translate-param params) port)))

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
		    (lambda (x)
		      (syntax-case x ()
			((k id)
			 (with-syntax ((name 
					(datum->syntax 
					 #'k
					 (string->symbol
					  (format "http-connection-~a"
						  (syntax->datum #'id))))))
			   #'(unless (undefined? id)
			       (name conn id))))))))
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
    (apply %http-request-adaptor 'POST server request-uri body options))

  (define (http-put server request-uri body . options)
    (apply %http-request-adaptor 'PUT server request-uri body options))

  (define (http-delete server request-uri . options)
    (apply %http-request-adaptor 'DELETE server request-uri #f options))

  (define (%http-request-adaptor method server request-uri body
				 :key receiver (sink #f) (flusher #f)
				 :allow-other-keys opts)
    (define recvr
      (if (or sink flusher)
	  (http-oport-receiver (or sink (open-output-bytevector))
			       (or flusher (lambda (s h) 
					     (get-output-bytevector s))))
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

  (define (http-string-sender str)
    (lambda (hdrs encoding header-sink)
      (let* ((encoder (make-transcoder (lookup-decoder encoding) 'none))
	     (body (string->bytevector str encoder))
	     (size (bytevector-length body))
	     (body-sink (header-sink `(("content-length" ,(number->string size))
				       ,@hdrs)))
	     (port (body-sink size)))
	(put-bytevector port body 0 size #t)
	(body-sink 0))))

  (define (http-blob-sender blob)
    (lambda (hdrs encoding header-sink)
      (let* ((data (if (string? blob) (string->utf8 blob) blob))
	     (size (bytevector-length data))
	     ;; TODO add "content-type: type/subtype; charset=utf-8" when
	     ;; blob was string
	     (body-sink (header-sink `(("content-length" ,(number->string size))
				       ,@hdrs)))
	     (port (body-sink size)))
	(put-bytevector port data 0 size #t)
	(body-sink 0))))

  (define (http-multipart-sender params)
    (lambda (hdrs encoding header-sink)
      (let-values (((body boundary) 
		    (http-compose-form-data params #f encoding)))
	(let* ((size (string-length body))
	       (hdrs `(("content-length" ,(number->string size))
		       ("mime-version" "1.0")
		       ("content-type" ,(string-append
					 "multipart/form-data; boundary=\""
					 boundary
					 "\""))
		       ,@(alist-delete "content-type" hdrs equal?)))
	       (body-sink (header-sink hdrs))
	       (port (body-sink size)))
	  (display body port)
	  (body-sink 0)))))

  ;; authentication handling
  ;; dummy

  (define (http-lookup-auth-handler headers)
    (and-let* ((hdr (rfc5322-header-ref headers "www-authenticate"))
	       (m   (looking-at #/^(\w+?)\s/ hdr))
	       (type (string-downcase (m 1))))
      (cond ((assoc type *supported-auth-handlers*) =>
	     (lambda (slot)
	       ((cdr slot) hdr)))
	    (else #f))))

  (define (http-basic-auth-handler-generator hdr)
    (lambda (user password _)
      (let ((msg (format "~a:~a" user password)))
	`(("authorization" ,(format "Basic ~a" 
				   (utf8->string
				    (base64-encode (string->utf8 msg)))))))))

  (define (http-digest-auth-handler-generator hdr)
    ;; need to get realm and so
    (lambda (user password conn)
      ;; TODO
      #f
      ))

  (define *supported-auth-handlers*
    `(("basic"  . ,http-basic-auth-handler-generator)
      ("digest" . ,http-digest-auth-handler-generator)))

  ;;(define (http-default-auth-handler . _) #f)

)
