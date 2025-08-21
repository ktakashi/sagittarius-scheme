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
	    ssh-read-auth-response
	    ssh-authenticate-method

	    *ssh:auth-method-rsa-algorithms* ;; for testing purpose
	    ssh-read-identity-file
	    ssh-read-identity
	    )
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius regex)
	    (sagittarius stty)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (rfc ssh auth api)
	    (rfc ssh auth identity)
	    (rfc ssh auth public-key))

;; nobody is using it I guess, but for backward compatibility
(define-syntax register-auth-method
  (syntax-rules ()
    ((_ name proc)
     (begin
       (define key (string->keyword (symbol->string name)))
       (define-method ssh-authenticate-method ((m (eql key))) proc)))))

(define-syntax u8 (identifier-syntax bytevector-u8-ref))


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
    (ssh-read-auth-response transport
      (lambda (rp)
	(cond ((= (u8 rp 0) +ssh-msg-userauth-passwd-changereq+)
      	       (values #f
      		       (read-message <ssh-msg-userauth-passwd-changereq>
      				     (open-bytevector-input-port rp))))
	      (else (error 'auth-password "unknown tag" rp)))))))

(define (auth-keyboard-interactive transport user-name 
      				   :key (service-name +ssh-connection+)
      				   :allow-other-keys others)
  (define (read-prompts bin n)
    (do ((i 0 (+ i 1))
         (r '() (cons (read-message <ssh-msg-userauth-prompt> bin) r)))
        ((= i n) (reverse! r))))
  (define (handle-info-request rp)
    (cond ((= (u8 rp 0) +ssh-msg-userauth-info-request+)
      	   (let* ((bin (open-bytevector-input-port rp))
      		  (req (read-message <ssh-msg-userauth-info-request> bin))
      		  (prompts (read-prompts bin (~ req 'num-prompts))))
	     (let ((responses (read-client-input prompts)))
	       (let-values (((out e) (open-bytevector-output-port)))
		 (let* ((msg (make <ssh-msg-userauth-info-response>
			       :num-response (length responses)))
			(bv (ssh-message->bytevector msg)))
		   (put-bytevector out bv))
		 (for-each (lambda (r) (write-message :utf8-string r out #f))
			   responses)
		 (ssh-write-packet transport (e)))
      	       (ssh-read-auth-response transport handle-info-request))))
	  (else
      	   (error 'auth-keyboard-interactive "unknown tag" rp))))
    
  (let1 m (apply make <ssh-msg-keyboard-interactive-userauth-request>
      		 :user-name user-name
      		 :method +ssh-auth-method-keyboard-interactive+
      		 :service-name service-name
      		 others)
    (ssh-write-ssh-message transport m)
    (ssh-read-auth-response transport handle-info-request)))

(define (ssh-authenticate transport method . options)
  (if (keyword? method)
      (cond ((ssh-authenticate-method method)
             => (lambda (proc) 
      		  (ssh-service-request transport +ssh-userauth+)
      		  (apply proc transport options)))
            (else (error 'ssh-authenticate "method not supported" method)))
      (apply ssh-authenticate transport 
             (string->keyword method) options)))

(define (read-client-input prompts)
  (define (do-read prompt)
    ;; handle echo by using stty
    (display (~ prompt 'prompt)) (display " ")
    (flush-output-port (current-output-port))
    (if (~ prompt 'echo)
	(get-line (current-input-port))
	(with-stty '((not echo) echonl)
		   (lambda () (get-line (current-input-port))))))
  (map do-read prompts))

(define-method ssh-authenticate-method (ignore) #f)

(define password-key (string->keyword +ssh-auth-method-password+))
(define-method ssh-authenticate-method ((m (eql password-key)))
  auth-password)

(define interactive-key (string->keyword +ssh-auth-method-keyboard-interactive+))
(define-method ssh-authenticate-method ((m (eql interactive-key)))
  auth-keyboard-interactive)

)
