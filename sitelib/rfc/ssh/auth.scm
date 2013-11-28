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
(library (rfc ssh auth)
    (export authenticate-user
	    register-auth-method
	    ;; for extension
	    read-auth-response)
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
	    (crypto)
	    (asn.1))

  (define *auth-methods* (make-eq-hashtable))
  (define (register-auth-method name proc) (set! (~ *auth-methods* name) proc))

  (define-syntax u8 (identifier-syntax bytevector-u8-ref))
  (define (read-auth-response socket callback)
    (let1 payload (read-packet socket)
      (cond ((= (u8 payload 0) +ssh-msg-userauth-banner+)
	     (let1 bp (read-packet socket) ;; ignore success
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

  ;; TODO required method so do it
  (define (auth-pub-key socket user-name public-key
			:key (private-key #f)
			(service-name +ssh-connection+))
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

    ;; we don't check private-key type but public-key for detect the
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
	      (let1 sid (~ socket 'session-id)
		(signer (bytevector-append (pack "!L" (bytevector-length sid))
					   sid
					   (ssh-message->bytevector m)))))
	(write-packet socket (ssh-message->bytevector m))
	;; read the responce
	(read-auth-response socket
	 (lambda (rp)
	   (cond ((= (u8 rp 0) +ssh-msg-userauth-pk-ok+)
		  (values #f
			  (read-message <ssh-msg-userauth-pk-ok>
					(open-bytevector-input-port rp))))
		 (else (error 'auth-password "unknown tag" rp))))))))

  (define (auth-password socket user-name old-password 
			 :key (new-password #f)
			 (service-name +ssh-connection+))
    (let1 m (make <ssh-msg-password-userauth-request>
	      :user-name user-name
	      :method +ssh-auth-method-password+
	      :service-name service-name
	      :change-password? (and new-password #t)
	      :old-password old-password
	      :new-password new-password)
      (write-packet socket (ssh-message->bytevector m))
      ;; TODO better dispatch
      (read-auth-response socket
       (lambda (rp)
	 (cond ((= (u8 rp 0) +ssh-msg-userauth-passwd-changereq+)
		(values #f
			(read-message <ssh-msg-userauth-passwd-changereq>
				      (open-bytevector-input-port rp))))
	       (else (error 'auth-password "unknown tag" rp)))))))
  
  (define (authenticate-user socket method . options)
    (if (symbol? method)
	(cond ((~ *auth-methods* method)
	       => (lambda (proc) 
		    ;; request service (this must be supported so don't check
		    ;; the response. or should we?
		    (service-request socket +ssh-userauth+)
		    (apply proc socket options)))
	      (else (error 'authenticate-user "method not supported" method)))
	(apply authenticate-user socket (string->symbol method) options)))

  ;; register
  (register-auth-method (string->symbol +ssh-auth-method-public-key+)
			auth-pub-key)
  (register-auth-method (string->symbol +ssh-auth-method-password+) 
			auth-password)
)