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
	    
	    oauth2-connection?
	    oauth2-http-connection?
	    oauth2-http2-connection?
	    make-http-oauth2-connection
	    make-http2-oauth2-connection
	    oauth2-connection-close!
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
	    (rfc http)
	    (rfc http2 client))
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
		  ((oauth2-connection-http-post connection)
		   connection path parameters
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
      (case method
	((GET)
	 (apply (oauth2-connection-http-get conn) conn path header))
	((POST)
	 (apply (oauth2-connection-http-post conn) conn path content header)))))
  
;;; Connection
  (define-record-type oauth2-connection
    (fields http-get http-post close))
  
  (define-record-type oauth2-http-connection
    (parent oauth2-connection)
    (fields server))

  (define-record-type oauth2-http2-connection
    (parent oauth2-connection)
    (fields http2-connection))

  (define (make-http-oauth2-connection server)
    (define (nothing conn) #t)
    (make-oauth2-http-connection oauth2-http-get oauth2-http-post
				 nothing server))

  (define (oauth2-connection-close! connection)
    ((oauth2-connection-close connection) connection))
  
  (define (oauth2-http-get connection path . headers)
    (apply http-get (oauth2-http-connection-server connection)
	   path
	   :receiver (http-binary-receiver)
	   :secure #t
	   headers))
  (define (oauth2-http-post connection path body . headers)
    (apply http-post (oauth2-http-connection-server connection)
	       path body
	       :receiver (http-binary-receiver)
	       :secure #t
	       headers))

;;; http2
  (define (make-http2-oauth2-connection server)
    (define (parse-port server)
      (cond ((string-index-right server #\:) =>
	     (lambda (p) (string-copy server (+ p 1))))
	    (else #f)))
    (let ((port (or (parse-port server) "443")))
      (make-oauth2-http2-connection oauth2-http2-get oauth2-http2-post
       oauth2-http2-close
       (make-http2-client-connection server port :secure? #t))))

  (define (oauth2-http2-close conn)
    (close-http2-client-connection!
     (oauth2-http2-connection-http2-connection conn)))
  
  (define (http2-response->http1-compatible header body)
    (define (bv-header->string-header n&b)
      (list (utf8->string (car n&b)) (utf8->string (cadr n&b))))
    (let ((headers (map bv-header->string-header header)))
      (values (cond ((assoc ":status" headers) => cadr))
	      headers
	      body)))

  (define (oauth2-http2-get connection path . headers)
    (let-values (((h b)
		  (apply http2-get
			 (oauth2-http2-connection-http2-connection connection)
			 path
			 headers)))
      (http2-response->http1-compatible h b)))

  (define (oauth2-http2-post connection path body . headers)
    (let-values (((h b)
		  (apply http2-post
			 (oauth2-http2-connection-http2-connection connection)
			 path
			 (string->utf8 body)
			 headers)))
      (http2-response->http1-compatible h b)))
)
