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

	    oauth2-get-request
	    oauth2-post-request
	    oauth2-access-protected-resource

	    oauth2-revoke-access-token

	    make-oauth2-access-token
	    oauth2-access-token?
	    oauth2-access-token-access-token
	    oauth2-access-token-token-type
	    oauth2-access-token-expires-in
	    oauth2-access-token-refresh-token
	    oauth2-access-token-scope

	    (rename (open-http-connection! open-oauth2-connection!)
		    (close-http-connection! close-oauth2-connection!))
	    make-oauth2-http1-connection
	    make-oauth2-http2-connection
	    )
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (srfi :1)
	    (srfi :13)
	    (srfi :39)
	    (text json)
	    (rfc uri)
	    (rfc base64)
	    (rfc http-connections))
;;; Access token

  ;; section 4.3
  (define (oauth2-request-password-credentials-access-token 
	   connection path username password :optional (scope #f))
    (json-string->access-token
     (oauth2-request-authorization-server connection path
      (base64-encode-string (string-append username ":" password))
      (string-join
       (cons* "grant_type=password"
	      (string-append "username=" (uri-encode-string username))
	      (string-append "password=" (uri-encode-string password))
	      (if scope
		  (list (string-append "scope=" (uri-encode-string scope)))
		  '()))
       "&"))))

  ;; section 4.4
  (define (oauth2-request-client-credentials-access-token
	   connection path credential :optional (scope #f))
    (json-string->access-token 
     (oauth2-request-authorization-server connection path
      credential
      (string-join
       (cons* "grant_type=client_credentials"
	      (if scope
		  (list (string-append "scope=" (uri-encode-string scope)))
		  '()))
       "&"))))

;;; OAuth 2.0 Token Revocation
  (define (oauth2-revoke-access-token connection path credential access-token)
    (define (do-revoke auth token type)
      (oauth2-request-authorization-server connection path credential
       (string-append "token=" token "&token_type_hint=" type)))
    (let ((token (oauth2-access-token-access-token access-token))
	  (refresh-token (oauth2-access-token-refresh-token access-token)))
      ;; TODO should we make this optional?
      (when refresh-token (do-revoke auth refresh-token "refresh_token"))
      (do-revoke auth token "access_token")))
  
  (define (oauth2-request-authorization-server
	   connection path credential parameters)
    (define content-type "application/x-www-form-urlencoded")
    (let-values (((status header body)
		  (http-request
		   connection 'POST path
		   :sender (http-string-sender connection parameters)
		   :content-type content-type
		   :authorization (string-append "Basic " credential))))
      (if (string=? status "200")
	  (utf8->string body)
	  (error 'oauth2-request-authorization-server
		 "Failed to access to authorization server"
		 `((status: ,status)
		   (body: ,(and body (utf8->string body))))))))
    
;;; Access token
  (define-record-type oauth2-access-token
    (fields access-token token-type expires-in refresh-token scope)
    (protocol (lambda (p)
		(lambda (access-token token-type expires-in refresh-token scope)
		  (define (->time v)
		    (let ((sec (if (string? v)
				   (string->number v)
				   v)))
		      (make-time time-duration 0 sec)))		  
		  (unless access-token
		    (assertion-violation 'make-oauth2-access-token
					 "access_token is required"))
		  (unless token-type
		    (assertion-violation 'make-oauth2-access-token
					 "token_type is required"))
		  (p access-token token-type
		     (and expires-in
			  (or (and (time? expires-in) expires-in)
			      (->time expires-in)))
		     refresh-token scope)))))
  
  (define (json-string->access-token json)
    (define (assoc-ref alist v)
      (cond ((assoc v alist) => cdr)
	    (else #f)))
    (parameterize ((*json-map-type* 'alist))
      (let ((json (json-read (open-string-input-port json))))
	(make-oauth2-access-token
	 (assoc-ref json "access_token")
	 (assoc-ref json "token_type")
	 (assoc-ref json "expires_in")
	 (assoc-ref json "refresh_token")
	 (assoc-ref json "scope")))))

;;; Section 7. Accessing Protected Resources
  (define (oauth2-access-token->authorization access-token)
    (let ((type (string->symbol
		 (string-downcase
		  (oauth2-access-token-token-type access-token))))
	  (token (oauth2-access-token-access-token access-token)))
      (eval `(oauth2-generate-authorization ,token)
	    (environment `(rfc oauth2 ,type)))))

  (define (oauth2-get-request conn access-token path)
    (oauth2-access-protected-resource conn 'GET access-token path))

  (define (oauth2-post-request conn access-token path body)
    (oauth2-access-protected-resource conn 'POST access-token path
				      :content body))

  (define (oauth2-access-protected-resource conn method access-token path
					    :key (content #f))
    (let-values (((header query)
		  (oauth2-access-token->authorization access-token)))
      (apply http-request conn method path
	     :sender (and content (http-blob-sender conn content))
	     header)))

  (define (make-oauth2-http2-connection server)
    (make-http2-connection server #t))
  (define (make-oauth2-http1-connection server)
    (make-http1-connection server #t))
  
)
