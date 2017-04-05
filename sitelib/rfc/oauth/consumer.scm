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
	    (www cgi) ;; for split-query-string
	    (only (rfc http) list->request-headers))

  (define (oauth-compute-signature&authorization-parameter conn method uri
	    :key (timestamp (time-second (current-time)))
		 (nonce (number->string (random-integer (greatest-fixnum))))
	    :allow-other-keys parameters)
    (define key (oauth-connection-consumer-key conn))
    (define signer (oauth-connection-signer conn))
    (define sbs (oauth-construct-base-string-uri
		 (oauth-connection-http-connection conn) uri))
    (define alist
      `(("oauth_consumer_key" ,key)
	("oauth_signature_method" ,(symbol->string
				    (oauth-signer-method signer)))
	("oauth_timestamp" ,(number->string timestamp))
	("oauth_nonce" ,nonce)
	("oauth_version" "1.0")
	,@(list->request-headers parameters)))
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
  
  (define (oauth-authorization-header conn method uri . parameters)
    (let-values (((signature alist)
		  (apply oauth-compute-signature&authorization-parameter
			 conn method uri parameters))
		 ((out extract) (open-string-output-port)))
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

  (define-record-type oauth-temporary-credential
    (fields token token-secret))
  (define (oauth-request-temporary-credential conn uri :key (callback "oob")
					      :allow-other-keys others)
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
		   (apply oauth-authorization-header conn 'POST uri
			  :oauth_callback callback
			  others)
		   :sender (http-null-sender
			    (oauth-connection-http-connection conn)))))
      (if (string=? s "200")
	  (parse-response (utf8->string b))
	  (raise
	   (condition (make-oauth-request-error s (utf8->string b))
		      (make-who-condition 'oauth-request-temporary-credential)
		      (make-message-condition
		       "Failed to request temporary credential"))))))

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

  (define (oauth-request conn method uri authorization . others)
    (apply http-request (oauth-connection-http-connection conn) method uri
	   :authorization authorization
	   others))
)



