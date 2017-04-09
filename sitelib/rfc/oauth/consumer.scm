;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/oauth/consumer.scm - OAuth1 consumer procedures
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

;; reference: https://tools.ietf.org/html/rfc5849
#!read-macro=sagittarius/bv-string
(library (rfc oauth consumer)
    (export oauth-request-temporary-credential
	    oauth-temporary-credential oauth-temporary-credential?
	    oauth-temporary-credential-token
	    oauth-temporary-credential-token-secret

	    make-oauth-authorization-url
	    oauth-request-access-token
	    oauth-access-token oauth-access-token? make-oauth-access-token
	    oauth-access-token-token oauth-access-token-token-secret

	    oauth-request/header-authorization
	    oauth-request/query-string-authoerization
	    oauth-request
	    ;; for debug/test
	    oauth-authorization-header
	    oauth-compute-signature&authorization-parameter)
    (import (rnrs)
	    (srfi :13)
	    (srfi :19)
	    (srfi :27)
	    (rfc oauth signature)
	    (rfc oauth connection)
	    (rfc oauth conditions)
	    (rfc http-connections)
	    (rfc :5322)
	    (rfc uri)
	    (only (rfc http) list->request-headers)
	    (www cgi) ;; for split-query-string
	    (sagittarius)
	    (sagittarius control))

  (define (oauth-compute-signature&authorization-parameter conn method uri
	    :key (timestamp (time-second (current-time)))
	         (nonce (number->string (random-integer (greatest-fixnum))))
	    :allow-other-keys parameters)
    (define-values (auth path query frag) (uri-decompose-hierarchical uri))
    (define key (oauth-connection-consumer-key conn))
    (define signer (oauth-connection-signer conn))
    (define sbs (oauth-construct-base-string-uri
		 (oauth-connection-http-connection conn) path))
    (define access-token (oauth-connection-access-token conn))
    (define (re-encode slot)
      (list (utf8->string (oauth-encode-string (car slot)))
	    (utf8->string (oauth-encode-string (cadr slot)))))
    (define alist
      `(("oauth_consumer_key" ,key)
	("oauth_signature_method" ,(symbol->string
				    (oauth-signer-method signer)))
	("oauth_timestamp" ,(number->string timestamp))
	("oauth_nonce" ,nonce)
	("oauth_version" "1.0")
	,@(if access-token `(("oauth_token" ,access-token)) '())
	,@(if query (map re-encode (split-query-string query)) '())
	,@(list->request-headers parameters)))

    ;; sanity check
    (when (and access-token (memq :oauth_token parameters))
      (assertion-violation 'oauth-compute-signature&authorization-parameter
			   "2 access tokens are passed"))
    
    (oauth-signer-process! signer (string->utf8 (symbol->string method)))
    (oauth-signer-process! signer #*"&")
    (oauth-signer-process! signer (oauth-encode-string sbs))
    (oauth-signer-process! signer #*"&")
    (let loop ((alist (oauth-normalize-parameters alist)) (first? #t))
      (unless (null? alist)
	(unless first? (oauth-signer-process! signer #*"%26")) ;; &
	(let ((k&v (car alist)))
	  (oauth-signer-process! signer (car k&v))
	  (oauth-signer-process! signer #*"%3D") ;; =
	  (oauth-signer-process! signer (cdr k&v)))
	(loop (cdr alist) #f)))
    (values (oauth-signer-done! signer)
	    (filter (lambda (o) (string-prefix? "oauth_" (car o)))
		    alist)))
  
  (define (oauth-authorization-parameter conn method uri . parameters)
    (let-values (((out extract) (open-string-output-port))
		 ((signature alist)
		  (apply oauth-compute-signature&authorization-parameter
			 conn method uri parameters)))
      (for-each (lambda (o)
		  (put-string out (car o))
		  (put-string out "=")
		  (put-string out (cadr o))
		  (put-string out "&")) alist)
      (put-string out "oauth_signature")
      (put-string out "=")
      (put-string out (utf8->string (oauth-encode-string signature)))
      (extract)))
  
  (define (oauth-authorization-header conn method uri . parameters)
    (let-values (((out extract) (open-string-output-port))
		 ((signature alist)
		  (apply oauth-compute-signature&authorization-parameter
			 conn method uri parameters)))
      (put-string out "OAuth ")
      (for-each (lambda (o)
		  (put-string out (car o))
		  (put-string out "=\"")
		  (put-string out (cadr o))
		  (put-string out "\",")) alist)
      (put-string out "oauth_signature")
      (put-string out "=\"")
      (put-string out (utf8->string (oauth-encode-string signature)))
      (put-string out "\"")
      (extract)))

  (define (raise-request-error who msg status response)
    (raise
     (condition (make-oauth-request-error status (utf8->string response))
		(make-who-condition 'who)
		(make-message-condition msg))))
  (define-record-type oauth-temporary-credential
    (fields token token-secret))

  ;; NB: we don't check content-type since one of the big OAuth provider
  ;;     (c.f. Twitter) doesn't return content-type x-www-form-urlencoded
  (define (oauth-request-temporary-credential conn uri :key (callback "oob"))
    (define (parse-response response)
      (define (raise-check-error)
	(raise
	 (conditions (make-oauth-error)
		     (make-who-condition 'oauth-request-temporary-credential)
		     (make-message-condition
		      "oauth_callback_confirmed is not found or not true"))))
      (let ((alist (split-query-string response)))
	(cond ((assoc "oauth_callback_confirmed" alist) =>
	       (lambda (v)
		 (unless (string=? (cadr v) "true") (raise-check-error))))
	      (else (raise-check-error)))
	(make-oauth-temporary-credential
	 (cadr (assoc "oauth_token" alist))
	 (cadr (assoc "oauth_token_secret" alist)))))
    (unless (oauth-connection-secure? conn)
      (assertion-violation 'oauth-request-temporary-credential
			   "connection must be secure"))
    (let-values (((s h b)
		  (oauth-request conn 'POST uri
		   (oauth-authorization-header conn 'POST uri
					       :oauth_callback callback)
		   :sender (http-null-sender
			    (oauth-connection-http-connection conn)))))
      (if (string=? s "200")
	  (parse-response (utf8->string b))
	  (raise-request-error 'oauth-request-temporary-credential
			       "Failed to request temporary credential" s b))))

  (define-record-type oauth-access-token
    (fields token token-secret))
  (define (make-oauth-authorization-url base-url temporary-credential)
    (define (compose-query query)
      (define token (oauth-temporary-credential-token temporary-credential))
      (if query
	  (string-append query "&oauth_token=" (uri-encode-string token))
	  (string-append "oauth_token=" (uri-encode-string token))))
    (let-values (((scheme ui host port path query frag) (uri-parse base-url)))
      (uri-compose :scheme scheme :userinfo ui :host host :port port
		   :path path :query (compose-query query)
		   :fragment frag)))

  (define (oauth-request-access-token conn uri temporary-credential pin)
    (define (parse-response response)
      (let ((alist (split-query-string response)))
	(make-oauth-access-token
	 (cadr (assoc "oauth_token" alist))
	 (cadr (assoc "oauth_token_secret" alist)))))
    (unless (oauth-connection-secure? conn)
      (assertion-violation 'oauth-request-access-token
			   "connection must be secure"))
    (let ((token (oauth-temporary-credential-token temporary-credential)))
      (let-values (((s h b)
		    (oauth-request conn 'POST uri
		     (oauth-authorization-header conn 'POST uri
						 :oauth_token token
						 :oauth_verifier pin)
		     :sender (http-blob-sender
			      (oauth-connection-http-connection conn)
			      (string->utf8 pin)))))
	(if (string=? s "200")
	    (parse-response (utf8->string b))
	    (raise-request-error 'oauth-request-access-token
				 "Failed to request access token" s b)))))

  ;; 3.5.  Parameter Transmission
  ;; POST request is not considered with these procedures
  ;; 3.5.1
  (define (oauth-request/header-authorization conn method uri . others)
    (apply oauth-request conn method uri
	   :authorization (oauth-authorization-header conn method uri)
	   others))
  ;; 3.5.2 is not supported
  ;; it's rather trouble some to implement generic way to handle POST request
  ;; and 3.5.2 requires POST.
  
  ;; 3.5.3
  (define (oauth-request/query-string-authoerization conn method uri . others)
    (let ((auth (oauth-authorization-parameter conn method uri)))
      (let-values (((a path query frag) (uri-decompose-hierarchical uri)))
	(apply oauth-request conn method
	       (string-append path "?" auth
			      (if query (string-append "&" query) ""))
	       others))))

  ;; it's just an wrapper of http-request for convenience.
  (define (oauth-request conn method uri . others)
    (apply http-request (oauth-connection-http-connection conn) method uri
	   others))
)



