;;; -*- Scheme -*-
;;;
;;; oauth.scm - OAuth 1.0 library.
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


;; based on cl-oauth
;; for now we don't support service provider.
#< (sagittarius regex) >
(library (net oauth)
    (export *protocol-version*

	    ;; tokens
	    <token>
	    token-key token-secret token-user-data token-consumer

	    register-token unregister-token

	    <consumer-token>
	    make-consumer-token

	    <request-token>
	    make-request-token
	    request-token-authorized?
	    request-token-authorized-set!
	    request-token-callback-uri
	    request-token-verification-code

	    <access-token>
	    make-access-token
	    access-token-session-handle
	    access-token-expires
	    access-token-authorization-expires
	    access-token-expired?

	    ;; consumer functions
	    obtain-access-token
	    authorize-request-token
	    ;; authorize-request-token-from-request
	    make-authorization-uri
	    obtain-request-token
	    access-protected-resource

	    ;; utility
	    oauth-compose-query
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius regex)
	    (sagittarius mop validator)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :19 time)
	    (srfi :26 cut)
	    (srfi :27 random-bits)
	    (math)
	    (rfc :5322)
	    (rfc base64)
	    (rfc http)
	    (rfc hmac)
	    (rfc uri))

  (define *protocol-version* :1.0)

  ;; TODO maybe it's better to separate token's definitions.
  ;; (expt 36 25)
  (define-constant random-n 808281277464764060643139600456536293376)

  ;; try not to create the same key
  (define random-source 
    (let ((s (make-random-source)))
      (random-source-randomize! s)
      (random-source-make-integers s)))
  
  (define (random-key)
    (number->string (random-source random-n) 36))
  (define (random-secret)
    (number->string (random-source random-n) 36))
  (define (random-verification-code)
    (number->string (random-source random-n) 36))

  (define-class <token> ()
    ((key    	:init-keyword :key :init-form (random-key))
     (secret 	:init-keyword :secret :init-form (random-secret))
     ;; Application-specific data associated
     (user-data :init-keyword :user-data :init-value #f
		:accessor token-user-data)))
  (define-method token-key ((t <token>)) (slot-ref t 'key))
  (define-method token-secret ((t <token>)) (slot-ref t 'secret))

  (define-class <consumer-token> (<token>)
    (;; The Consumer that originally requested this token.
     (last-timestamp :init-value 0 :accessor consumer-token-last-timestamp)))
  (define (make-consumer-token . args)
    (apply make <consumer-token> args))

  (define-class <consumer-ref-mixin> ()
    ((consumer :init-keyword :consumer :accessor token-consumer)))

  ;; request tokens
  (define-class <request-token> (<token> <consumer-ref-mixin>)
    (;; Callback URI for this request token, #f means oob
     (callback-uri :init-keyword :callback-uri :init-value #f)
     ;; Might be #f for OAuth 1.0
     (verification-code :init-keyword :verification-code
			:accessor request-token-verification-code
			:init-form (random-verification-code))
     (authorized? :init-value #f)))
  (define-method request-token-authorized? ((t <request-token>))
    (slot-ref t 'authorized?))
  (define-method request-token-authorized-set! ((t <request-token>)
						(b <boolean>))
    (slot-set! t 'authorized? b))

  (define (make-request-token . args)
    (apply make <request-token> args))

  (define (time-validator o v)
    (unless (or (not v) (time? v))
      (assertion-violation 'time-validator
			   "slot value must be #f or time object"))
    v)
  (define-class <access-token> (<token> <consumer-ref-mixin> <validator-mixin>)
    ((session-handle :init-keyword :session-handle :init-value #f)
     ;; Universal time when this token expires.
     (expires :init-keyword :expires :init-value #f
	      :validator time-validator)
     ;; Universal time when this token's session expires
     (authorization-expires :init-keyword :authorization-expires
			    :init-value #f :validator time-validator)
     ;; URI this access token has been obtained form. Needed for refresh.
     (origin-uri :init-keyword :origin-uri :init-value #f)))
  (define-method access-token-session-handle ((t <access-token>))
    (slot-ref t 'session-handle))
  (define-method access-token-expires ((t <access-token>))
    (slot-ref t 'expires))
  (define-method access-token-authorization-expires ((t <access-token>))
    (slot-ref t 'authorization-expires))
  (define-method access-token-origin-uri ((t <access-token>))
    (slot-ref t 'origin-uri))

  (define (make-access-token . args)
    (apply make <access-token> args))
  (define (access-token-expired? access-token)
    (unless (is-a? access-token <access-token>)
      (assertion-violation 'access-token-expired?
			   "access token required") access-token)
    (and (access-token-session-handle access-token)
	 (or (and-let* ((expires (access-token-expires access-token)))
	       (time>? (current-time) expires))
	     (and-let* ((expires (access-token-authorization-expires
				  access-token)))
	       (time>? (current-time) expires)))))

  ;; consumer functions
  ;; helper
  ;; OAuth expects upper case somehow...
  (define (%-fix str)
    (regex-replace-all #/%[\da-fA-F][\da-fA-F]/ str
		       (lambda (m p) 
			 (put-string p (string-upcase (regex-group m 0))))))
  (define (oauth-uri-encode str)
    (%-fix (uri-encode-string str)))
  (define (oauth-compose-query params)
    (%-fix (http-compose-query #f params)))

  (define (build-auth-string parameters)
    (format "OAuth ~a"
	    (%-fix
	     (string-join (map (lambda (p)
				 (string-append (oauth-uri-encode (car p))
						"="
						(oauth-uri-encode (cadr p))))
			       parameters) ", "))))
  (define (oauth-http-request uri
			      :key (auth-location :header)
				   (method 'GET)
				   (auth-parameters '())
				   (parameters '())
				   (additional-headers '()))
    (receive (scheme user-info host port path query frag) (uri-parse uri)
      ;; TODO secure
      (let* ((body (oauth-compose-query (if (eq? auth-location :parameters)
					    (append parameters auth-parameters)
					    parameters)))
	     (sender (http-blob-sender body))
	     (headers (if (eq? auth-location :header)
			  (cons `("Authorization"
				  ,(build-auth-string auth-parameters))
				additional-headers)
			  additional-headers)))
	(http-request method host (if (eq? method 'GET)
				      (string-append path "?" body)
				      path)
		      :sender sender
		      :auth-handler (lambda _ headers)))))

  (define (generate-auth-parameters consumer signature-method timestamp version
				    :optional (token #f))
    (let ((parameters `(("oauth_consumer_key" ,(token-key consumer))
			("oauth_signature_method" ,(string-upcase
						    (format "~a"
							    signature-method)))
			("oauth_timestamp" ,(number->string timestamp))
			("oauth_nonce" ,(number->string
					 (random-integer (greatest-fixnum))))
			("oauth_version" ,(format "~a" version)))))
      (if token
	  (cons `("oauth_token" ,(uri-decode-string (token-key token)
						    'utf-8 #t))
		parameters)
	  parameters)))

  ;; Additional parameters will be stored in the user-data slot of the token.
  (define (obtain-request-token uri consumer-token
				:key (version :1.0)
				     (user-parameters '())
				     (timestamp (time-second (current-time)))
				     (auth-location :header)
				     (request-method 'POST)
				     (callback-uri #f)
				     (additional-headers '())
				     (signature-method :hmac-sha1))
    (let* ((callback-uri (or callback-uri "oob"))
	   (auth-parameters (cons `("oauth_callback" ,callback-uri)
				  (generate-auth-parameters consumer-token
							    signature-method
							    timestamp
							    version)))
	   (sbs (signature-base-string :uri uri :request-method request-method
				       :parameters (sort-parameters
						    (append user-parameters
							    auth-parameters))))
	   (signature (oauth-signature signature-method sbs
				       (token-secret consumer-token)))
	   (signed-parameters (cons `("oauth_signature" ,signature)
				    auth-parameters)))
      (receive (status header body)
	  (oauth-http-request uri
			      :method request-method
			      :auth-location auth-location
			      :auth-parameters signed-parameters
			      :parameters user-parameters
			      :additional-headers additional-headers)
	(unless (string=? status "200")
	  (assertion-violation 'obtain-request-token body))
	(let* ((response (query-string->alist body))
	       (key (cond ((assoc "oauth_token" response) => cadr)
			  (else #f)))
	       (secret (cond ((assoc "oauth_token_secret" response) => cadr)
			     (else #f)))
	       (user-data (lset-difference 
			   (lambda (e1 e2) (equal? (car e1) e2))
			   response '("oauth_token" "oauth_token_secret"))))
	  (make-request-token :consumer consumer-token :key key
			      :secret secret :callback-uri callback-uri
			      :user-data user-data)))))

  ;; Return the service provider's authorization URI. [6.2.1] in 1.0
  (define (make-authorization-uri uri request-token
				  :key (version :1.0) (callback-uri #f)
				       (user-parameters '()))
    (when (and request-token (request-token-authorized? request-token))
      (assertion-violation 'make-authorization-uri
			   "given request token is already ahtuorised"
			   request-token))
    (let* ((parameters (append user-parameters
			       (if request-token
				   `(("oauth_token" ,(token-key request-token)))
				   '())
			       (if callback-uri
				   `(("oauth_callback" ,callback-uri))
				   '()))))
      (if (null? parameters)
	  uri
	  (string-append uri "?" (alist->query-string parameters)))))

  ;; Authorize a request token explicitly. Returns the authorized token.
  (define (authorize-request-token request-token verificateion-code)
    (when (and verificateion-code (string? verificateion-code))
      (request-token-verification-code request-token verificateion-code)
      (request-token-authorized-set! request-token #t))
    request-token)

  ;; Additional parameters will be stored in the user-data slot of the
  ;; token. POST is recommended as request method. [6.3.1]
  (define (obtain-access-token uri token :key
			       (consumer-token
				(token-consumer token))
			       (request-method 'POST)
			       (auth-location :header)
			       (version :1.0)
			       (timestamp (time-second (current-time)))
			       (signature-method :hmac-sha1))
    (let1 refresh? (is-a? token <access-token>)
      (unless refresh?
	(or (request-token-authorized? token)
	    (assertion-violation 'obtain-access-token
				 "request token is not authorised.")))
      (let* ((parameters (append
			  (generate-auth-parameters consumer-token
						    signature-method
						    timestamp
						    version
						    token)
			  (cond (refresh?
				 `(("oauth_session_handle"
				    ,(access-token-session-handle token))))
				((request-token-verification-code token)
				 => (lambda (it) `(("oauth_verifier" ,it))))
				(else '()))))
	     (sbs (signature-base-string :uri uri :request-method request-method
					 :parameters (sort-parameters 
						      parameters)))
	     (signature (oauth-signature signature-method sbs
					 (token-secret consumer-token)
					 (uri-decode-string (token-secret token)
							    'utf-8 #t)))
	     (signed-parameters (cons `("oauth_signature" ,signature)
				      parameters)))
	(receive (status header body)
	    (oauth-http-request uri
				:method request-method
				:auth-location auth-location
				:auth-parameters signed-parameters)
	  (define (field name response)
	    (cond ((assoc name response) => cadr)
		  (else #f)))

	  (unless (string=? status "200")
	    (assertion-violation 'obtain-access-token body))
	  (let* ((response (query-string->alist body))
		 (key (field "oauth_token" response))
		 (secret (field "oauth_token_secret" response))
		 (session-handle (field "oauth_session_handle" response))
		 (expires (and-let* ((r (field "oauth_expires_in" response)))
			    (add-duration! (current-time)
					   (make-time 'time-duration 0
						      (string->number r)))))
		 (authorization-expires
		  (and-let* ((r (field "oauth_authorization_expires_in"
				       response)))
		    (add-duration! (current-time)
				   (make-time 'time-duration 0
					      (string->number r)))))
		 (user-data (remove-oauth-parameters response)))
	    (unless (and key secret)
	      (assertion-violation 
	       'obtain-access-token
	       "oauth_token or/and oauth_token_secret field(s) are not returned"))
	    (make-access-token :consumer consumer-token
			       :key (uri-decode-string key 'utf-8 #t)
			       :secret (uri-decode-string secret 'utf-8 #t)
			       :session-handle session-handle
			       :expires expires
			       :authorization-expires authorization-expires
			       :origin-uri uri
			       :user-data user-data))))))

  (define (refresh-access-token access-token)
    (obtain-access-token (access-token-origin-uri access-token) access-token))

  (define (maybe-refresh-access-token access-token :optional (on-refresh #f))
    (if (access-token-expired? access-token)
	(let ((new-token (refresh-access-token access-token)))
	  (when on-refresh
	    (on-refresh new-token))
	  new-token)
	access-token))

  (define (get-problem-report-from-headers headers)
    (and-let* ((auth-header (rfc5322-header-ref headers "www-authenticate"))
	       (len (string-length auth-header))
	       ( (>= len 5) )
	       (type (substring auth-header 0 5))
	       ( (string=? type "OAuth") )
	       ( (> len 5))
	       (parameters (map (lambda (token)
				  (string-split token "="))
				(string-split (substring auth-header 6 len)
					      #/\s/))))
      parameters))

  (define (get-problem-report headers body)
    (let ((from-headers (get-problem-report-from-headers headers)))
      from-headers))

  ;; Access the protected resource at URI using ACCESS-TOKEN.
  ;; If the token contains OAuth Session information it will be checked for
  ;; validity before the request is made. Should the server notify us that
  ;; it has prematurely expired the token will be refresh as well and the
  ;; request sent again using the new token. ON-REFRESH will be called
  ;; whenever the access token is renewed.
  (define (access-protected-resource uri access-token :rest kwargs :key
				     (consumer-token 
				      (token-consumer access-token))
				     (on-refresh #f)
				     (timestamp (time-second (current-time)))
				     (user-parameters '())
				     (additional-headers '())
				     (version :1.0)
				     (auth-location :header)
				     (request-method 'GET)
				     (signature-method :hmac-sha1))
    (set! access-token (maybe-refresh-access-token access-token on-refresh))
    (receive (normalized-uri query-string-parameters) (normalize-uri uri #t)
      (let* ((auth-parameters (generate-auth-parameters consumer-token
							signature-method
							timestamp
							version
							access-token))
	     (sbs (signature-base-string :uri normalized-uri
					 :request-method request-method
					 :parameters 
					 (sort-parameters
					  (append query-string-parameters
						  user-parameters
						  auth-parameters))))
	     (signature (oauth-signature signature-method sbs
					 (token-secret consumer-token)
					 (token-secret access-token)))
	     (signed-parameters (cons `("oauth_signature" ,signature)
				      auth-parameters)))
	(receive (status header body)
	    (oauth-http-request uri
				:method request-method
				:auth-location auth-location
				:auth-parameters signed-parameters
				:parameters user-parameters
				:additional-headers additional-headers)
	  (if (string=? status "200")
	      (values body status #f #f)
	      (let* ((problem-report (get-problem-report header body))
		     (problem-hint (and-let* ((r (assoc "oauth_problem"
							problem-report)))
				     (cdr r)))
		     (problem-advice (and-let* ((r (assoc "oauth_problem_advice"
							  problem-report)))
				       (cdr r))))
		(cond ((and (string=? status "401")
			    (equal? problem-hint "token_expired"))
		       (let ((new-token (refresh-access-token access-token)))
			 (when on-refresh
			   (on-refresh new-token))
			 (apply access-protected-resource uri new-token 
				kwargs)))
		      (else
		       (values body status problem-hint problem-advice)))))))))

  ;; parameters
  ;; Sort parameters according to the OAuth spec.
  (define (sort-parameters parameters)
    (when (assoc "oauth_signature" parameters)
      (assertion-violation 'sort-parameters
			   "oauth_signature must not be in parameters"
			   parameters))
    (list-sort (lambda (a b)
		 (string< (car a) (car b))) parameters))

  ;; Return #t if parameter start with "oauth_".
  (define (oauth-parameter? parameter)
    (string-prefix? "oauth_" parameter))

  (define (remove-oauth-parameters parameters)
    (remove (lambda (p) (oauth-parameter? (car p))) parameters))

  ;; query string
  ;; TODO move this to somewhere, this is too general only for OAuth
  (define (alist->query-string alist :key (url-encode #f))
    (string-join
     (map (cut string-join <> "=")
	  (if url-encode
	      (map (lambda (l) (list (oauth-uri-encode (car l))
				     (oauth-uri-encode (cadr l)))) alist)
	      alist))
     "&"))
  (define (query-string->alist qs)
    (let ((kv-pairs (string-split qs "&")))
      (map (lambda (kv-pair)
	     (string-split kv-pair "="))
	   kv-pairs)))

  ;; signature
  ;; for now we only support consumer so no default
  (define (signature-base-string :key (uri #f)
				      (request-method #f)
				      (parameters #f))
    (string-append (string-upcase (format "~a" request-method))
		   "&" (oauth-uri-encode (normalize-uri uri))
		   "&" (oauth-uri-encode
			(oauth-compose-query parameters))))

  ;; uri
  ;; 9.1.2
  (define (normalize-uri uri :optional (need-query? #f))
    (receive (scheme user-info host port path query frag) (uri-parse uri)
      (let ((uri (string-append
		  (string-downcase scheme)
		  "://"
		  (string-downcase host)
		  (cond ((not port) "")
			((and (string=? scheme "http") (= port 80)) "")
			((and (string=? scheme "https") (= port 443)) "")
			(else (string-append ":" (number->string port))))
		  path)))
	(if need-query?
	    (values uri (if query (query-string->alist query) '()))
	    uri))))

  ;; hash
  (define (oauth-signature method sbs consumer-secret
			   :optional (token-secret ""))
    (utf8->string
      (base64-encode
       (case method
	 ((:hmac-sha1)
	  (hash (hash-algorithm 
		 HMAC
		 :key (string->utf8 (string-append consumer-secret
						   "&"
						   token-secret)))
		(string->utf8 sbs)))))))
  )