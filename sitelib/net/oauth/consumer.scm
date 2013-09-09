;;; -*- Scheme -*-
;;;
;;; token.scm - OAuth 1.0 library.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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
#!read-macro=sagittarius/regex
(library (net oauth consumer)
    (export obtain-access-token
	    authorize-request-token
	    ;; authorize-request-token-from-request
	    make-authorization-uri
	    obtain-request-token
	    access-protected-resource

	    oauth-uri-encode
	    oauth-compose-query)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius regex)
	    (net oauth misc)
	    (net oauth token)
	    (net oauth signature)
	    (net oauth parameters)
	    (net oauth query-string)
	    (srfi :1 lists)
	    (srfi :19 time)
	    (rfc :5322)
	    (rfc uri)
	    (rfc http))

  ;;;;;;;;;;;;
  ;;; consumer functions

  ;; Message translator
  ;; if something wrong happens within oauth process, http response has
  ;; the error message, however we don't know which format it uses.
  ;; so let user handle it.
  (define (default-message-translator status header body) body)

  ;; helper
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
		      :secure (string=? scheme "https")
		      :sender sender
		      :extra-headers headers))))

  (define (generate-auth-parameters consumer signature-method timestamp version
				    :optional (token #f))
    (let ((parameters `(("oauth_consumer_key" ,(token-key consumer))
			("oauth_signature_method" ,(string-upcase
						    (format "~a"
							    signature-method)))
			("oauth_timestamp" ,(number->string timestamp))
			("oauth_nonce" ,(number->string
					 (random-source (greatest-fixnum))))
			("oauth_version" ,(format "~a" version)))))
      (if token
	  (cons `("oauth_token" ,(uri-decode-string (token-key token)
						    :cgi-decode #t))
		parameters)
	  parameters)))

  ;; Additional parameters will be stored in the user-data slot of the token.
  (define (obtain-request-token uri consumer-token :key
				(version :1.0)
				(user-parameters '())
				(timestamp (time-second (current-time)))
				(auth-location :header)
				(request-method 'POST)
				(callback-uri #f)
				(additional-headers '())
				(signature-method :hmac-sha1)
				(error-translator default-message-translator))
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
	  (assertion-violation 'obtain-request-token
			       (error-translator status header body)))
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
			       (signature-method :hmac-sha1)
			       (error-translator default-message-translator))
    (let1 refresh? (access-token? token)
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
							    :cgi-decode #t)))
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
	    (assertion-violation 'obtain-access-token
				 (error-translator status header body)))
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
			       :key (uri-decode-string key :cgi-decode #t)
			       :secret (uri-decode-string secret :cgi-decode #t)
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
    (or (and-let* ((auth-header (rfc5322-header-ref headers "www-authenticate"))
		   (len (string-length auth-header))
		   ( (>= len 5) )
		   (type (substring auth-header 0 5))
		   ( (string=? type "OAuth") )
		   ( (> len 5)))
	  (map (lambda (token)
		 (string-split token "="))
	       (string-split (substring auth-header 6 len)
			     #/\s/)))
	'()))

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
				     (signature-method :hmac-sha1)
				     (error-translator 
				      default-message-translator))
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
	      (values body header #f #f)
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
		       (values (error-translator status header body)
			       header
			       problem-hint problem-advice)))))))))


)
	    
