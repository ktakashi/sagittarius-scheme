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

	    ;; for debug/test
	    oauth-authorization-header)
    (import (rnrs)
	    (srfi :13)
	    (srfi :19)
	    (srfi :27)
	    (rfc oauth signature)
	    (rfc oauth connection)
	    (rfc http-connections)
	    (only (rfc http) list->request-headers))
  
  (define (oauth-authorization-header conn method uri 
	    :key (timestamp (time-second (current-time)))
		 (nonce (random-integer (greatest-fixnum)))
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
	("oauth_nonce" ,(number->string nonce))
	("oauth_version" "1.0")
	,@(list->request-headers parameters)))

    (oauth-signer-process! signer (string->utf8 (symbol->string method)))
    (oauth-signer-process! signer #*"&")
    (oauth-signer-process! signer (string->utf8 sbs))
    (oauth-signer-process! signer #*"&")
    (let loop ((alist (oauth-normalize-parameters alist)) (first? #t))
      (unless (null? alist)
	(unless first? (oauth-signer-process! signer #*"%26")) ;; &
	(let ((k&v (car alist)))
	  (oauth-signer-process! signer (car k&v))
	  (oauth-signer-process! signer #*"%3D") ;; =
	  (oauth-signer-process! signer (cdr k&v)))
	(loop (cdr alist) #f)))
    (let ((signature (oauth-signer-done! signer)))
      (let-values (((out extract) (open-string-output-port)))
	(put-string out "OAuth ")
	(for-each (lambda (o)
		    (put-string out (car o))
		    (put-string out "=\"")
		    (put-string out (cadr o))
		    (put-string out "\","))
		  (filter (lambda (o) (string-prefix? "oauth_" (car o)))
			  alist))
	(put-string out "oauth_signature")
	(put-string out "=\"")
	(put-string out (utf8->string (oauth-encode-string signature)))
	(put-string out "\"")
	(extract))))

  (define (oauth-request-temporary-credential conn uri :key (callback "oob")
					      :allow-other-keys others)
    (unless (oauth-connection-secure? conn)
      (assertion-violation 'oauth-request-temporary-credential
			   "connection must be secure"))
    (oauth-request conn 'POST uri
		   (apply oauth-authorization-header conn 'POST uri
			  :oauth_callback callback
			  others)))

  (define (oauth-request conn method uri authorization . others)
    (apply http-request (oauth-connection-http-connection conn) method uri
	   :authorization authorization
	   others))
)



