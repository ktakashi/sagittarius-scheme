;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/auth/public-key.scm - SSH2 publickey authentication
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

#!nounbound
(library (rfc ssh auth public-key)
    (export ssh-public-key-authentication
	    *ssh:auth-method-rsa-algorithms*)
    (import (rnrs)
	    (clos user)
	    (binary pack)
	    (sagittarius)
	    (sagittarius crypto keys)
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures)
	    (sagittarius object)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (rfc ssh crypto)
	    (rfc ssh auth api)
	    (srfi :39 parameters))
(define method-key (string->keyword +ssh-auth-method-public-key+))
(define-method ssh-authenticate-method ((m (eql method-key)))
  ssh-public-key-authentication)

(define *ssh:auth-method-rsa-algorithms*
  (make-parameter `(,+public-key-rsa-sha2-256+
		    ,+public-key-rsa-sha2-512+
      		    ,+public-key-ssh-rsa+)))

;; private-key can be #f to check if the server has the public-key.
;; in that case 2 values, failure result and <ssh-msg-userauth-pk-ok>,
;; will be returned.
;; TODO should we?
(define (ssh-public-key-authentication transport user-name private-key public-key
      				       :key (service-name +ssh-connection+))
  (define (sign private-key session-id request)
    (let ((signer (make-ssh-signer (string->keyword (~ request 'algorithm-name))
				   private-key))
	  (msg (bytevector-append 
      		(pack "!L" (bytevector-length session-id)) session-id
      		(ssh-message->bytevector request))))
      (ssh-message->bytevector
       (make <ssh-signature> :type (~ request 'algorithm-name)
	     :signature (signer-sign-message signer msg)))))
  (define blob (make-ssh-public-key public-key))
  ;; we don't check private-key type but public-key to detect the
  ;; algorithm name
  (let-values (((algorithm-name require-check?)
		(determine-algorithm-name transport (~ blob 'name))))
    (let ((m (make <ssh-msg-public-key-userauth-request>
      	       :user-name user-name
      	       :service-name service-name
      	       :method +ssh-auth-method-public-key+
      	       :has-signature? #t
      	       :algorithm-name algorithm-name
      	       :blob (ssh-message->bytevector blob)
	       :signature #f)))
      (when (and require-check? (rsa-public-key? public-key))
	;; do prefligt
	(check-rsa-algorithm! blob m transport))
      ;; signature
      (set! (~ m 'signature) (sign private-key (~ transport 'session-id) m))
      (ssh-write-ssh-message transport m)
      ;; read the responce
      (ssh-read-auth-response transport
	(lambda (rp)
	  (cond ((= (bytevector-u8-ref rp 0) +ssh-msg-userauth-pk-ok+)
      		 (values #f
      			 (read-message <ssh-msg-userauth-pk-ok>
      				       (open-bytevector-input-port rp))))
      		(else (error 'ssh-public-key-authentication
			     "unknown tag" rp))))))))


(define (determine-algorithm-name transport key-algorithm-name)
  (define (check server-algorithms)
    (if (string=? key-algorithm-name +public-key-ssh-rsa+)
	(cond ((member +public-key-rsa-sha2-512+ server-algorithms)
	       (values +public-key-rsa-sha2-512+ #f))
	      ((member +public-key-rsa-sha2-256+ server-algorithms)
	       (values +public-key-rsa-sha2-256+ #f))
	      ((member +public-key-ssh-rsa+ server-algorithms)
	       (values key-algorithm-name #t))
	      (else (values key-algorithm-name #t)))
	(cond ((member key-algorithm-name server-algorithms)
	       (values key-algorithm-name #f))
	      (else (values key-algorithm-name #t)))))
  (cond ((~ transport 'server-signature-algorithms) => check)
	;; okay, we don't know if the server supports ssh-rsa or not
	(else (values key-algorithm-name #t))))
	 

;; For RSA, we have multiple options, so let the server respond if the
;; ones we supports are supported.
(define (check-rsa-algorithm! pk request transport)
  (define (check-server transport req)
    (ssh-write-ssh-message transport req)
    (let-values (((r value)
		  (ssh-read-auth-response transport
		    (lambda (rp)
		      (values (= (bytevector-u8-ref rp 0) +ssh-msg-userauth-pk-ok+)
			      rp)))))
      r))
  (set! (~ request 'has-signature?) #f)
  (let loop ((algorithms (*ssh:auth-method-rsa-algorithms*)))
    (if (null? algorithms)
	(error 'make-ssh-auth-method-certificate
	       "No RSA algorithms are supported by the server"
	       (*ssh:auth-method-rsa-algorithms*))
	(let ((name (car algorithms)))
	  (set! (~ pk 'name) name)
	  (set! (~ request 'algorithm-name) name)
	  (or (and (check-server transport request)
		   (set! (~ request 'has-signature?) #t))
	      (loop (cdr algorithms)))))))

)
