;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/oauth/signature.scm - OAuth1 signature procedures
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
(library (rfc oauth signature)
    (export oauth-signer? oauth-signer-process! oauth-signer-done!
	    oauth-signer-method
	    oauth-verifier? oauth-verifier-verify
	    make-oauth-hmac-sha1-signer make-oauth-hmac-sha1-verifier
	    make-oauth-rsa-sha1-signer make-oauth-rsa-sha1-verifier
	    make-oauth-plaintext-signer make-oauth-plaintext-verifier

	    oauth-construct-base-string-uri
	    oauth-encode-string

	    oauth-normalize-parameters
	    )
    (import (rnrs)
	    (rfc http-connections)
	    (rfc base64)
	    (rfc hmac)
	    (rfc uri)
	    (srfi :13)
	    (srfi :14)
	    (util bytevector)
	    (crypto)
	    (math))
  (define-record-type oauth-signer
    (fields process done method))
  (define (oauth-signer-process! signer msg)
    ((oauth-signer-process signer) msg))
  (define (oauth-signer-done! signer)
    ((oauth-signer-done signer)))
  
  (define-record-type oauth-verifier
    (fields %verify))
  (define (oauth-verifier-verify verify msg signature)
    ((oauth-verifier-%verify verify) msg signature))

  (define (->base64 bv) (utf8->string (base64-encode bv :line-width #f)))
  ;; 3.4.2 HMAC-SHA1
  (define (make-oauth-hmac-sha1-signer secret)
    (define hmac (hash-algorithm HMAC :key secret))
    (hash-init! hmac)
    (make-oauth-signer
     (lambda (msg) (hash-process! hmac msg))
     (lambda () (let ((out (make-bytevector (hash-size hmac))))
		  (hash-done! hmac out 0 (bytevector-length out))
		  (hash-init! hmac) ;; for next time if needed
		  (->base64 out)))
     'HMAC-SHA1))
  (define (make-oauth-hmac-sha1-verifier secret)
    (define hmac (hash-algorithm HMAC :key secret))
    (make-oauth-verifier
     (lambda (msg signature)
       (verify-mac hmac msg (base64-decode-string signature :transcoder #f)))))

  ;; 3.4.3 RSA-SHA1
  ;; private key must be provided before hand.
  (define (make-oauth-rsa-sha1-signer private-key)
    (define cipher (make-cipher RSA private-key))
    (let-values (((out extract) (open-bytevector-output-port)))
      (values
       (make-oauth-signer
	(lambda (msg) (put-bytevector out msg))
	(lambda ()
	  (->base64
	   (cipher-signature cipher (extract)
			     :encode pkcs1-emsa-v1.5-encode)))
	'RSA-SHA1))))
  (define (make-oauth-rsa-sha1-verifier public-key)
    (define cipher (make-cipher RSA public-key))
    (make-oauth-verifier
     (lambda (msg signature)
       (cipher-verify cipher msg (base64-decode-string signature :transcoder #f)
		      :verify pkcs1-emsa-v1.5-verify))))
  ;; 3.4.4 PLAINTEXT
  ;; secret must be the same as HMAC-SHA1
  (define (make-oauth-plaintext-signer secret)
    (let ((r (->base64 secret)))
      (make-oauth-signer (lambda (msg) #t) (lambda () r) 'PLAINTEXT)))
  (define (make-oauth-plaintext-verifier secret)
    (make-oauth-verifier
     (lambda (msg signature)
       (unless (bytevector=? msg
			     (base64-decode-string signature :transcoder #f))
	 (assertion-violation 'oauth-plaintext-verifier "inconsistent")))))

  ;; 3.4 Signature Base String
  ;; 3.4.1.2 Base String URI
  (define (oauth-construct-base-string-uri http-connection path)
    (define secure? (http-connection-secure? http-connection))
    (define (remove-query path)
      (cond ((string-index-right path #\?) =>
	     (lambda (p) (string-copy path 0 p)))
	    (else path)))
    (let-values (((server port) (http-connection-server&port http-connection)))
      (let ((scheme (if secure? "https://" "http://"))
	    (path   (remove-query path)))
	(string-append scheme (string-downcase server)
		       (if (not (or (string=? "443" port) (string=? "80" port)))
			   (string-append ":" port)
			   "")
		       path))))

  ;; 3.4.1.3.2 Parameters Normalization
  ;; input = ((str str) ...)
  ;; output = ((bv bv) ...)
  (define (oauth-normalize-parameters alist)
    (list-sort (lambda (a b)
		 (if (bytevector=? (car a) (car b))
		     (bytevector<? (cdr a) (cdr b))
		     (bytevector<? (car a) (car b))))
	       (map (lambda (s) (cons (oauth-encode-string (car s))
				      (oauth-encode-string (cadr s)))) alist)))
  
  ;; 3.6.  Percent Encoding
  ;; we return encoded value as bytevector for convenience.
  (define (oauth-encode-string s)
    (let-values (((out extract) (open-bytevector-output-port)))
      (uri-encode (open-bytevector-input-port (string->utf8 s)) out)
      (extract)))
)
