;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/oauth2.scm - OAuth2 client
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

;; reference
;;  RFC 6749: https://tools.ietf.org/html/rfc6749

(library (rfc oauth2)
    (export oauth2-request-password-credentials-access-token
	    oauth2-request-client-credentials-access-token
	    oauth2-request-access-token
	    oauth2-connection?
	    oauth2-http-connection?
	    oauth2-http2-connection?
	    make-http-oauth2-connection
	    make-http2-oauth2-connection)
    (import (rnrs)
	    (sagittarius)
	    (srfi :1)
	    (srfi :13)
	    (rfc uri)
	    (rfc base64)
	    (rfc http)
	    (rfc http2 client))
;;; Access token

  ;; section 4.3
  (define (oauth2-request-password-credentials-access-token 
	   connection path username password :optional (scope #f))
    (oauth2-request-access-token connection path
     (list (create-basic-authorization 
	    (base64-encode-string (string-append username ":" password))))
     (string-join
      (cons* "grant_type=password"
	     (string-append "username=" (uri-encode-string username))
	     (string-append "password=" (uri-encode-string password))
	     (if scope
		 (list (string-append "scope=" (uri-encode-string scope)))
		 '()))
      "&")))

  ;; section 4.4
  (define (oauth2-request-client-credentials-access-token
	   connection path credential :optional (scope #f))
    (oauth2-request-access-token connection path
     (list (create-basic-authorization credential))
     (string-join
      (cons* "grant_type=client_credentials"
	     (if scope
		 (list (string-append "scope=" (uri-encode-string scope)))
		 '()))
      "&")))


  (define (oauth2-request-access-token connection path headers parameters)
    ((oauth2-connection-http-post connection)
     connection path headers parameters))
  
  (define (create-basic-authorization value)
    (list "Authorization" (string-append "Basic " value)))

;;; Connection
  (define-record-type oauth2-connection
    (fields http-get http-post))
  
  (define-record-type oauth2-http-connection
    (parent oauth2-connection)
    (fields server))

  (define-record-type oauth2-http2-connection
    (parent oauth2-connection)
    (fields connection))

  (define (make-http-oauth2-connection server)
    (make-oauth2-http-connection oauth2-http-get oauth2-http-post server))
  (define (make-http2-oauth2-connection server)
    (define (parse-port server)
      (cond ((string-index-right server #\:) =>
	     (lambda (p) (string-copy server (+ p 1))))
	    (else #f)))
    (let ((port (or (parse-port server) "443")))
      (make-oauth2-http2-connection oauth2-http2-get oauth2-http2-post
	(make-http2-client-connection server port :secure? #t))))

  (define content-type '("Content-Type" "application/x-www-form-urlencoded"))
  (define (oauth2-http-get connection path headers parameters)
    (http-get (oauth2-http-connection-server connection)
	      (if (null? parameters)
		  path
		  (string-append path "?" parameters))
	      :secure #t
	      :extra-headers headers))
  (define (oauth2-http-post connection path headers parameters)
    (http-post (oauth2-http-connection-server connection)
	      path parameters
	      :secure #t
	      :extra-headers (cons content-type headers)))

  (define (oauth2-http2-get connection path headers parameters)
    (apply http2-get (oauth2-http2-connection-connection connection)
	   (if (null? parameters)
	       path
	       (string-append path "?" parameters))
	   (append-map (lambda (h&v)
			 (list (string->keyword (car h&v)) (cadr h&v)))
		       headers)))

  (define (oauth2-http2-post connection path headers parameters)
    (apply http2-post (oauth2-http2-connection-connection connection)
	   path
	   (string->utf8 parameters)
	   (append-map (lambda (h&v)
			 (list (string->keyword (car h&v)) (cadr h&v)))
		       headers)))
					   
	       
  
  )				    
