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

(library (net oauth provider)
    (export *protocol-version*
	    check-version
	    check-nonce-and-timestamp
	    check-signature
	    check-verification-code

	    validate-request-token-request
	    request-token-response
	    get-supplied-request-token
	    finalize-callback-uri

	    validate-access-token-request
	    validate-access-token
	    make-response

	    ;; tokens
	    register-token
	    unregister-token

	    ;; condition
	    &oauth-error make-oauth-error oauth-error?
	    &bad-request make-bad-request bad-request?
	    &unauthorised make-unauthorised unauthorised?
	    raise-oauth-error
	    )
    (import (rnrs)
	    (sagittarius)
	    (net oauth misc)
	    (net oauth signature)
	    (net oauth parameters)
	    (net oauth request-adapter)
	    (net oauth query-string)
	    (clos user)
	    (rfc uri)
	    (net oauth token)
	    (srfi :39 parameters))
  (define *protocol-version* (make-parameter :1.0))

  (define-condition-type &oauth-error &error make-oauth-error oauth-error?)
  (define-condition-type &bad-request &oauth-error
    make-bad-request bad-request?)
  (define-condition-type &unauthorised &oauth-error make-unauthorised
    unauthorised?)
  
  (define (raise-oauth-error ctr who message . irr)
    (raise (apply condition
		  (filter values (list (ctr)
				       (and who (make-who-condition who))
				       (make-message-condition message)
				       (make-irritants-condition irr))))))

  (define (finalize-callback-uri request-token)
    (let ((uri (request-token-callback-uri request-token)))
      (receive (scheme user-info host port path query frag) (uri-parse uri)
	(let ((merged (uri-merge (uri-compose :scheme scheme :userinfo user-info
					      :host host :port port :path path)
				 (string-concatenate 
				  (or query "")
				  "oauth_token=" 
				  (oauth-uri-encode (token-key request-token))
				  "&oauth_verifier="
				  (oauth-uri-encode (request-token-verification-code request-token))))))
	  (if frag
	      (string-append merged "#" frag)
	      merged)))))

  ;; Consumer management
  (define *registered-consumers* (make-parameter (make-equal-hashtable)))
  
  (define-method register-token ((token <consumer-token>))
    (hashtable-set! (*registered-consumers*) (token-key token) token)
    token)

  (define-method unregister-token ((token <consumer-token>))
    (hashtable-delete! (*registered-consumers*) (token-key token)))

  (define (get-consumer-token key)
    (hashtable-ref (*registered-consumers*) key #f))

  ;; signature checking
  (define (check-signature)
    (unless (equal? (parameter "oauth_signature_method") "HMAC-SHA1")
      (raise-oauth-error 
       make-bad-request
       'check-signature
       "Signature method not passed or different from HMAC-SHA1"))
    (let* ((supplied-signature (weak-hashtable-ref (*signature-cache*)
						   (request) #f))
	   (consumer-sec (and-let* ((t (get-consumer-token 
					(parameter "oauth_consumer_key"))))
			   (token-secret t)))
	   (token-sec (and-let* ((t (or (get-supplied-request-token
					     :raise-error? #f)
					    (get-supplied-access-token
					     :raise-error? #f))))
			    (token-secret t))))
      (unless supplied-signature
	(raise-oauth-error make-bad-request 'check-signature
			   "This request is not signed"))
      (unless consumer-sec
	(raise-oauth-error make-bad-request 'check-signature
			   "Invalid consumer"))
      ;; now calculate the signature and check for match
      (let* ((signature-base-string (signature-base-string))
	     (signature
	      (oauth-signature :hmac-sha1 signature-base-string consumer-sec
			       token-sec)))
	(unless (string=? signature supplied-signature)
	  ;; TODO should we log?
	  (raise-oauth-error make-unauthorised 'check-signature
			     "Invalid signature"))
	#t)))

  ;; nonce and timestamp checking
  (define (check-nonce-and-timestamp consumer-token)
    (unless (parameter "oauth_timestamp")
      (raise-oauth-error make-bad-request 'check-nonce-and-timestamp
			 "Missing Timestamp"))
    (let ((timestamp (string->number (parameter "oauth_timestamp")))
	  (nonce (parameter "oauth_nonce")))
      (unless timestamp
	(raise-oauth-error make-unauthorised 'check-nonce-and-timestamp
			   "Malformed Timestamp"))
      (unless nonce
	(raise-oauth-error make-bad-request 'check-nonce-and-timestamp
			   "Missing nonce"))
      (unless (>= timestamp (consumer-token-last-timestamp consumer-token))
	(raise-oauth-error make-unauthorised 'check-nonce-and-timestamp
			   "Invalid timestamp")))
    #t)

  ;; version checking
  (define (check-version)
    (let ((version (parameter "oauth_version")))
      (unless (member version '("1.0"))
	(raise-oauth-error make-bad-request 'check-version
			   "Not prepared to handle OAuth version other than 1.0"
			   version))
      #t))
  ;; verification code checking
  (define (check-verification-code)
    (unless (equal? (parameter "oauth_verifier")
		    (request-token-verification-code
		     (get-supplied-request-token)))
      (raise-oauth-error make-unauthorised 'check-verification-code
			 "Invalid verification code"))
    #t)

  ;; misc
  (define (get-supplied-consumer-token)
    (let ((consumer-key (parameter "oauth_consumer_key")))
      (unless consumer-key
	(raise-oauth-error make-bad-request 'get-supplied-access-token
			   "Consumer key not supplied"))
      (let ((consumer-token (get-consumer-token consumer-key)))
	(unless consumer-token
	  (raise-oauth-error make-unauthorised 'get-supplied-access-token
			     "Can't identify Consumer"))
	consumer-token)))

  (define (get-supplied-callback-uri 
	   :key (allow-oob-callback? #f) 
	   (allow-none? (eq? (*protocol-version*) :1.0)))
    (let ((callback (parameter "oauth_callback")))
      (cond ((and (not allow-none?) (not callback))
	     (raise-oauth-error make-bad-request 'get-supplied-callback-uri
				"No callback supplied"))
	    ((and (not allow-oob-callback?) (equal? callback "oob"))
	     (raise-oauth-error make-bad-request 'get-supplied-callback-uri
				"Not prepared for an OOB callback setup!"))
	    (else callback))))

  ;; request token management
  (define *issued-request-tokens* (make-parameter (make-equal-hashtable)))

  (define-method register-token ((token <request-token>))
    (hashtable-set! (*issued-request-tokens*) (token-key token) token))
  (define-method unregister-token ((token <request-token>))
    (hashtable-delete! (*issued-request-tokens*) (token-key token)))
  (define (invalidate-request-token request-token)
    (hashtable-delete! (*issued-request-tokens*) (token-key request-token)))

  ;; [5.3]
  (define (make-response alist)
    (alist->query-string
     (map (lambda (p)
	    (cons (oauth-uri-encode (car p))
		  (oauth-uri-encode (cdr p)))) alist)
     :include-leading-ampersand #f))

  ;; Respond to a valid request token request. [6.1.2]
  (define (request-token-response request-token . additional-parameters)
    (assert (for-all (lambda (o) (not (oauth-parameter? o)))
		     additional-parameters))
    (make-response
     (append
      `(("oauth_token" . ,(token-key request-token))
        ("oauth_token_secret" . ,(token-secret request-token))
        ("oauth_callback_confirmed" . "true"))
      additional-parameters)))


  ;; Check whether REQUEST is a valid request token request.
  ;; Returns the supplied Consumer callback or #f if
  ;; the callback is supposed to be transferred oob. [6.1.1]
  (define (validate-request-token-request :key (request-token-ctor
						make-request-token)
					  (allow-oob-callback? #f))
    (unless (>= (length (normalized-parameters))
		(case (*protocol-version*)
		  ((:1.0) 4) (else 5)))
      (raise-oauth-error make-bad-request 'validate-request-token-request
			 "Failed protocol assertion"))
    (check-version)
    (check-signature)
    (let ((consumer-token (get-supplied-consumer-token)))
      (check-nonce-and-timestamp consumer-token)
      (let* ((callback-uri (get-supplied-callback-uri 
			    :allow-oob-callback? allow-oob-callback?))
	     (request-token (request-token-ctor
			     :consumer consumer-token
			     :callback-uri callback-uri
			     :user-data (remove-oauth-parameters
					 (normalized-parameters)))))
	(register-token request-token)
	request-token)))

  ;; Utility function that extracts the Consumer-supplied request token
  ;; from a list of normalized parameters. Guards against non-existing
  ;; and unknown tokens. Returns the request token on success.
  (define (get-supplied-request-token :key (check-verification-code? #f)
				      (raise-error? #t))
    (let ((request-token-key (parameter "oauth_token")))
      (unless (or request-token-key (not raise-error?))
	(raise-oauth-error make-bad-request 'get-supplied-request-token
			   "Missing request token"))
      (let ((request-token (hashtable-ref (*issued-request-tokens*)
					  request-token-key #f)))
	(unless (or request-token-key (not raise-error?))
	  (raise-oauth-error make-unauthorised 'get-supplied-request-token
			     "Invalid request token"))
	(when check-verification-code?
	  (check-verification-code))
	request-token)))

  (define *issued-access-tokens* (make-parameter (make-equal-hashtable)))

  (define-method register-token ((token <access-token>))
    (hashtable-set! (*issued-access-tokens*) (token-key token) token))
  (define-method unregister-token ((token <access-token>))
    (hashtable-delete! (*issued-access-tokens*) (token-key token)))

  (define (validate-access-token-request :key (access-token-ctor
					       make-access-token))
    (define (check-parameters param)
      (receive (low high) (case (*protocol-version*)
			    ((:1.0) (values 5 6))
			    (else (values 6 7)))
	(<= low (length param) high)))
    ;; no user-supplied parameters allowed here, and the
    ;; spec forbids duplicate oauth args per section 5.
    ;; moreover we don't count the oauth_signature parameter as it isn't
    ;; part of the normalized parameter list.
    (let ((param (normalized-parameters)))
      (unless (check-parameters param)
	(raise-oauth-error make-bad-request 'validate-access-token-request
			   "Failed protocol assertion"))
      (unless (null? (remove-oauth-parameters param))
	(raise-oauth-error make-bad-request 'validate-access-token-request
			   "Failed protocol assertion"))
      (check-version)
      (check-signature)
      (let* ((request-token (get-supplied-request-token
			     :check-verification-code?
			     (not (eq? (*protocol-version*) :1.0))))
	     (consumer (token-consumer request-token)))
	(check-nonce-and-timestamp consumer)
	(let ((access-token (access-token-ctor :consumer consumer)))
	  (register-token access-token)
	  (invalidate-request-token request-token)
	  access-token))))
  
  (define (access-token-response access-token :rest additional-parameters)
    (oauth-uri-encode
     (alist->query-string
      `(("oauth_token" . ,(token-key access-token))
	("oauth_token_secret" . ,(token-secret access-token))))))
  
  ;; protected resource access management [7]
  ;; Utility function that extracts the Consumer-supplied request token
  ;; from a list of normalized parameters. Guards against non-existing
  ;; and unknown tokens. Returns the request token on success.  
  (define (get-supplied-access-token :key (raise-error? #t))
    (let ((access-token-key (parameter "oauth_token")))
      (unless (or access-token-key (not raise-error?))
	(raise-oauth-error make-bad-request 'get-supplied-access-token
			   "Missing access token"))
      (let ((access-token (hashtable-ref (*issued-access-tokens*)
					 access-token-key #f)))
	(unless (or access-token-key (not raise-error?))
	  (raise-oauth-error make-unauthorised 'get-supplied-access-token
			     "Invalid access token"))
	access-token)))

  (define (validate-access-token)
    (unless (>= (length (normalized-parameters)) 6)
      (raise-oauth-error make-bad-request 'validate-access-token
			 "Failed protocol assertion"))
    (check-version)
    (check-signature)
    (let ((consumer-token (get-supplied-consumer-token)))
      (check-nonce-and-timestamp consumer-token)
      (let ((access-token (get-supplied-access-token)))
	(unless (eq? consumer-token (token-consumer access-token))
	  (raise-oauth-error make-unauthorised 'validate-access-token
			     "Given access token was not issued for consumer token"
			     access-token consumer-token))
	#t)))
  )