;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/oauth2.scm - OAuth2 client
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

;; reference
;;  RFC 6749: https://tools.ietf.org/html/rfc6749
;;  RFC 7009: https://tools.ietf.org/html/rfc7009 (revocation)
(library (rfc oauth2)
    (export oauth2-request-password-credentials-access-token
	    oauth2-request-client-credentials-access-token
	    oauth2-request-authorization-server
	    oauth2-compose-access-token-request-parameter

	    oauth2-authorization-code-grant-authorization-url
	    oauth2-request-authorization-code-grant-access-token
	    
	    oauth2-get-request
	    oauth2-post-request
	    oauth2-request

	    oauth2-revoke-access-token

	    make-oauth2-access-token
	    oauth2-access-token?
	    oauth2-access-token-expired?
	    oauth2-access-token-creation-time
	    oauth2-access-token-access-token
	    oauth2-access-token-token-type
	    oauth2-access-token-expires-in
	    oauth2-access-token-refresh-token
	    oauth2-access-token-scope
	    oauth2-access-token-parameters

	    oauth2-connection?
	    make-oauth2-http1-connection
	    make-oauth2-http2-connection
	    oauth2-connection-http-connection
	    open-oauth2-connection!
	    close-oauth2-connection!
	    oauth2-connection-attach-access-token!
	    oauth2-connection-access-token

	    json-string->access-token

	    &oauth2-error oauth2-error?
	    &oauth2-request-error oauth2-request-error?
	    oauth2-request-error-status oauth2-request-error-content

	    &oauth2-authorization-server-error
	    oauth2-authorization-server-error?
	    )
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (srfi :1)
	    (srfi :13)
	    (srfi :19)
	    (srfi :39)
	    (text json)
	    (text json object-builder)
	    (rfc uri)
	    (rfc base64)
	    (rfc http-connections))

  ;; condition
  (define-condition-type &oauth2-error &error
    make-oauth2-error oauth2-error?)
  (define-condition-type &oauth2-request-error &oauth2-error
    make-oauth2-request-error oauth2-request-error?
    (status oauth2-request-error-status)
    (content oauth2-request-error-content))
  (define-condition-type &oauth2-authorization-server-error &oauth2-request-error
    make-oauth2-authorization-server-error oauth2-authorization-server-error?)
;;; Section 7. Accessing Protected Resources
  (define (oauth2-access-token->authorization access-token)
    (let ((type (string->symbol
		 (string-downcase
		  (oauth2-access-token-token-type access-token))))
	  (token (oauth2-access-token-access-token access-token)))
      (eval `(oauth2-generate-authorization ,token)
	    (environment `(rfc oauth2 ,type)))))

  (define-record-type oauth2-connection
    (fields http-connection
	    (mutable access-token)
	    (mutable authorization)
	    (mutable query))
    (protocol
     (lambda (p)
       (case-lambda
	((conn) (p conn #f #f #f))
	((conn access-token)
	 (let-values (((header query)
		       (oauth2-access-token->authorization access-token)))
	   (p conn access-token header query)))))))
  ;; even though, servers (resource, authorization) can be separated,
  ;; authorization server and resource server are the same most of the
  ;; time. so this procedure is useful in practice (not in theory)
  (define (oauth2-connection-attach-access-token! conn access-token)
    (let-values (((header query)
		  (oauth2-access-token->authorization access-token)))
      (oauth2-connection-access-token-set! conn access-token)
      (oauth2-connection-authorization-set! conn header)
      (oauth2-connection-query-set! conn query)
      conn))

  (define-syntax define-oauth2-connection
    (syntax-rules ()
      ((_ name make-http-connection)
       (define name
	 (case-lambda
	  ((server) (make-oauth2-connection (make-http-connection server #t)))
	  ((server token)
	   (make-oauth2-connection (make-http-connection server #t) token)))))))
  
  (define-oauth2-connection make-oauth2-http2-connection make-http2-connection)
  (define-oauth2-connection make-oauth2-http1-connection make-http1-connection)
  (define (open-oauth2-connection! conn)
    (open-http-connection! (oauth2-connection-http-connection conn))
    conn)
  (define (close-oauth2-connection! conn)
    (close-http-connection! (oauth2-connection-http-connection conn))
    conn)
;;; Access token
  (define-record-type oauth2-access-token
    (fields creation-time
	    access-token
	    token-type
	    expires-in
	    refresh-token
	    scope
	    parameters)
    (protocol (lambda (p)
		(lambda (access-token token-type expires-in refresh-token
		         scope parameters)
		  (unless access-token
		    (assertion-violation 'make-oauth2-access-token
					 "access_token is required"))
		  (unless token-type
		    (assertion-violation 'make-oauth2-access-token
					 "token_type is required"))
		  (p (current-time)
		     access-token
		     token-type
		     (and expires-in
			  (make-time time-duration 0 expires-in))
		     refresh-token
		     scope
		     parameters)))))
  (define access-token-builder
    (json-object-builder
     (list
      "access_token"
      "token_type"
      (? "expires_in" #f)
      (? "refresh_token" #f)
      (? "scope" #f))))
  (define (json-string->access-token json)
    (define extra-parameters (make-equal-hashtable))
    (define (parameter-handler k v) (hashtable-set! extra-parameters k v))
    (define (post-object-build obj)
      (apply make-oauth2-access-token (append obj (list extra-parameters))))
    (parameterize ((*post-json-object-build* post-object-build))
      (json-string->object json access-token-builder parameter-handler)))

  (define (oauth2-access-token-expired? access-token)
    (define expires-in (oauth2-access-token-expires-in access-token))
    (define creation-time (oauth2-access-token-creation-time access-token))
    (and expires-in
	 (time<=? (add-duration creation-time expires-in) (current-time))))

  ;; section 4.1 Authorization Code Grant
  ;; TODO support PKCE: https://tools.ietf.org/html/rfc7636
  (define (oauth2-authorization-code-grant-authorization-url
	   base-url client-id :key (redirect-uri #f)
				   (scope #f)
				   (state #f))
    (define (compose-query query)
      (let ((request-parameters
	     (string-concatenate
	      (filter values
		      (list "response_type=code&client_id="
			    (uri-encode-string client-id)
			    (and redirect-uri
				 (string-append "&redirect_uri="
						(uri-encode-string redirect-uri)))
			    (and state
				 (string-append "&state="
						(uri-encode-string state)))
			    (and scope
				 (string-append "&scope="
						(uri-encode-string scope))))))))
			    
	(if query
	    (string-append query "&" request-parameters)
	    request-parameters)))
    (let-values (((scheme ui host port path query frag) (uri-parse base-url)))
      (uri-compose :scheme "https" :userinfo ui :host host :port port
		   :path path :query (compose-query query) :fragment frag)))

  (define (authorization-header u p)
    (string-append "Basic " (base64-encode-string (string-append u ":" p))))

  (define (oauth2-compose-access-token-request-parameter type . rest)
    (let-values (((out extract) (open-string-output-port)))
      (put-string out "grant_type=")
      (put-string out type)
      (let loop ((k&v rest))
	(cond ((null? k&v) (extract))
	      ((null? (cdr k&v))
	       (assertion-violation
		'oauth2-compose-access-token-request-parameter
		"keyword list is not even" rest))
	      (else
	       (let ((k (car k&v))
		     (v (cadr k&v)))
		 (unless (and (keyword? k) (or (string? v) (not v)))
		   (assertion-violation
		    'oauth2-compose-access-token-request-parameter
		    "value must be keyword followed by string" k v))
		 (when v
		   (put-string out "&")
		   (put-string out
		    (uri-encode-string (keyword->string k) :cgi-encode #t))
		   (put-string out "=")
		   (put-string out (uri-encode-string v :cgi-encode #t)))
		 (loop (cddr k&v))))))))
  
  ;; even though cliend_secret is an optional parameter in spec however
  ;; it's sort of required according to the following page:
  ;; https://aaronparecki.com/oauth-2-simplified/
  ;; since client_id and client_secret combination is the alternative
  ;; method of authentication, we must not send authorization header here.
  ;; (this method is optional to support according to the spec but in this
  ;;  case it's rather must support instead of may support. wtf?)
  (define (oauth2-request-authorization-code-grant-access-token
	   connection path client-id code redirect-uri :key (client-secret #f))
    (json-string->access-token
     (oauth2-request-authorization-server connection path
      (oauth2-compose-access-token-request-parameter
       "authorization_code"
       :code code
       :client_id client-id
       :client_secret client-secret
       :redirect_uri redirect-uri))))
  
  ;; section 4.3
  (define (oauth2-request-password-credentials-access-token 
	   connection path username password :key (scope #f))
    (json-string->access-token
     (oauth2-request-authorization-server connection path
      (oauth2-compose-access-token-request-parameter
       "password"
       :username username
       :password password
       :scope scope)
      :authorization (authorization-header username password))))

  ;; section 4.4
  (define (oauth2-request-client-credentials-access-token
	   connection path credential :key (scope #f))
    (json-string->access-token 
     (oauth2-request-authorization-server connection path
      (oauth2-compose-access-token-request-parameter
       "client_credentials"
       :scope scope)
      :authorization (string-append "Basic " credential))))

;;; OAuth 2.0 Token Revocation
  (define (oauth2-revoke-access-token connection path credential access-token)
    (define (do-revoke auth token type)
      (oauth2-request-authorization-server connection path
       (string-append "token=" token "&token_type_hint=" type)
       :authorization (string-append "Basic "credential)))
    (let ((token (oauth2-access-token-access-token access-token))
	  (refresh-token (oauth2-access-token-refresh-token access-token)))
      ;; TODO should we make this optional?
      (when refresh-token (do-revoke auth refresh-token "refresh_token"))
      (do-revoke auth token "access_token")))
  
  (define (oauth2-request-authorization-server connection path parameters
					       . rest)
    (define content-type "application/x-www-form-urlencoded")
    (define http-connection (oauth2-connection-http-connection connection))
    (let-values (((status header body)
		  (apply http-request
			 http-connection 'POST path
			 :sender (http-string-sender http-connection parameters)
			 :content-type content-type
			 ;; please return JSON...
			 :accept "application/json"
			 rest)))
      (if (string=? status "200")
	  (utf8->string body)
	  (raise
	   (condition (make-oauth2-authorization-server-error
		       status (and body (utf8->string body)))
		      (make-who-condition 'oauth2-request-authorization-server)
		      (make-message-condition
		       "Failed to access to authorization server"))))))
   
  (define (oauth2-get-request conn path) (oauth2-request conn 'GET path))
  (define (oauth2-post-request conn path body)
    (oauth2-request conn 'POST path
		    :sender (http-blob-sender
			     (oauth2-connection-http-connection conn))))

  (define (oauth2-request conn method path . options)
    (let ((access-token  (oauth2-connection-access-token conn))
	  (authorization (oauth2-connection-authorization conn)))
      (unless (oauth2-access-token? access-token)
	(assertion-violation 'oauth2-request
			     "connection doesn't have access token"))
      (when (oauth2-access-token-expired? access-token)
	(assertion-violation 'oauth2-request
			     "access-token is expired"))
      (apply http-request (oauth2-connection-http-connection conn) method path
	     :authorization authorization
	     options)))
  
)
