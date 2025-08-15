;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/auth.scm - SSH2 protocol authentication protocol
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

#!read-macro=sagittarius/regex
#!nounbound
(library (rfc ssh auth)
    (export ssh-authenticate
	    register-auth-method
	    ;; for extension
	    (rename (read-auth-response ssh-read-auth-response)))
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius regex)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (binary pack)
	    (crypto))

  (define *auth-methods* (make-eq-hashtable))
  (define (register-auth-method name proc) (set! (~ *auth-methods* name) proc))

  (define-syntax u8 (identifier-syntax bytevector-u8-ref))
  (define (read-auth-response transport callback)
    (let1 payload (ssh-read-packet transport)
      (cond ((= (u8 payload 0) +ssh-msg-userauth-banner+)
	     (let1 bp (ssh-read-packet transport) ;; ignore success
	       (values
		(= (u8 bp 0) +ssh-msg-userauth-success+)
		(read-message <ssh-msg-userauth-banner>
			      (open-bytevector-input-port payload)))))
	    ((= (u8 payload 0) +ssh-msg-userauth-success+) (values #t #f))
	    ((= (u8 payload 0) +ssh-msg-userauth-failure+)
	     (values #f
		     (read-message <ssh-msg-userauth-failure>
				   (open-bytevector-input-port payload))))
	    (else (callback payload)))))

  ;; private-key can be #f to check if the server has the public-key.
  ;; in that case 2 values, failure result and <ssh-msg-userauth-pk-ok>,
  ;; will be returned.
  ;; TODO should we?
  (define (auth-pub-key transport user-name private-key public-key
			:key (service-name +ssh-connection+))
    (define (make-signer mark name options)
      (if private-key
	  (lambda (msg)
	    (let* ((c (cipher mark private-key))
		   (sig (apply sign c msg options)))
	      (ssh-message->bytevector
	       (make <ssh-signature> :type name :signature sig))))
	  (lambda (msg) #f)))
    (define (dsa->blob)
      (values
       "ssh-dss"
       (make <ssh-dss-certificate>
	 :name "ssh-dss"
	 :p (~ public-key 'p) :q (~ public-key 'q) :g (~ public-key 'g)
	 :y (~ public-key 'Y))
       (make-signer DSA "ssh-dss" '(:der-encode #f))))
    (define (rsa->blob)
      (values
       "ssh-rsa"
       (make <ssh-rsa-certificate>
	 :name "ssh-rsa"
	 :e (~ public-key 'exponent) :n (~ public-key 'modulus))
       (make-signer RSA "ssh-rsa" (list :encode pkcs1-emsa-v1.5-encode))))

    ;; we don't check private-key type but public-key to detect the
    ;; algorithm name
    (let-values (((name blob signer)
		  (let1 name (symbol->string (~ (class-of public-key) 'name))
		    (cond ((#/rsa/i name) (rsa->blob))
			  ((#/dsa/i name) (dsa->blob))
			  (else (error 'auth-pub-key "unknown key" 
				       public-key))))))
      (let1 m (make <ssh-msg-public-key-userauth-request>
		:user-name user-name
		:service-name service-name
		:method +ssh-auth-method-public-key+
		:has-signature? (and private-key #t)
		:algorithm-name name
		:blob (ssh-message->bytevector blob))
	;; signature
	(set! (~ m 'signature)
	      (and private-key
		   (let1 sid (~ transport 'session-id)
		     (signer (bytevector-append 
			      (pack "!L" (bytevector-length sid)) sid
			      (ssh-message->bytevector m))))))
	(ssh-write-ssh-message transport m)
	;; read the responce
	(read-auth-response transport
	 (lambda (rp)
	   (cond ((= (u8 rp 0) +ssh-msg-userauth-pk-ok+)
		  (values #f
			  (read-message <ssh-msg-userauth-pk-ok>
					(open-bytevector-input-port rp))))
		 (else (error 'auth-password "unknown tag" rp))))))))

  (define (auth-password transport user-name old-password 
			 :key (new-password #f)
			 (service-name +ssh-connection+))
    (let1 m (make <ssh-msg-password-userauth-request>
	      :user-name user-name
	      :method +ssh-auth-method-password+
	      :service-name service-name
	      :change-password? (and new-password #t)
	      :old-password old-password
	      :new-password new-password)
      (ssh-write-ssh-message transport m)
      ;; TODO better dispatch
      (read-auth-response transport
       (lambda (rp)
	 (cond ((= (u8 rp 0) +ssh-msg-userauth-passwd-changereq+)
		(values #f
			(read-message <ssh-msg-userauth-passwd-changereq>
				      (open-bytevector-input-port rp))))
	       (else (error 'auth-password "unknown tag" rp)))))))
  
  (define (ssh-authenticate transport method . options)
    (if (symbol? method)
	(cond ((~ *auth-methods* method)
	       => (lambda (proc) 
		    ;; request service (this must be supported so don't check
		    ;; the response. or should we?
		    (ssh-service-request transport +ssh-userauth+)
		    (apply proc transport options)))
	      (else (error 'ssh-authenticate "method not supported" method)))
	(apply ssh-authenticate transport 
	       (string->symbol method) options)))

  ;; register
  (register-auth-method (string->symbol +ssh-auth-method-public-key+)
			auth-pub-key)
  (register-auth-method (string->symbol +ssh-auth-method-password+) 
			auth-password)
)
