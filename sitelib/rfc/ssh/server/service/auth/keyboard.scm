;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/service/auth/keyboard.scm - SSH2 server authentication API
;;;  
;;;   Copyright (c) 2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh server service auth keyboard)
    (export ssh-handle-userauth-request
	    *ssh:keybord-interactive-prompt-message*

	    <ssh-interactive-credential> ssh-interactive-credential?
	    ssh-credential-prompt-sender
	    ssh-credential-response-receiver

	    make-ssh-interactive-prompt ssh-interactive-prompt?
	    )
    (import (rnrs)
	    (clos user)
	    (srfi :39 parameters)
	    (sagittarius object)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (rfc ssh server service auth api))

(define-class <ssh-interactive-credential> (<ssh-credential>)
  ((prompt-sender :init-keyword :prompt-sender
		  :reader ssh-credential-prompt-sender)
   (response-receiver :init-keyword :response-receiver
		      :reader ssh-credential-response-receiver)))
(define-record-type ssh-interactive-prompt
  (fields message echo?))

(define (ssh-interactive-credential? o)
  (is-a? o <ssh-interactive-credential>))


(define *ssh:keybord-interactive-prompt-message*
  (make-parameter "Password for Sagittarius SSH server:"))

;; TODO implement multiple prompts authentication capability
(define-method ssh-handle-userauth-request 
  ((m (equal +ssh-auth-method-keyboard-interactive+)) transport packet)
  (define msg 
    (bytevector->ssh-message <ssh-msg-keyboard-interactive-userauth-request>
			     packet))
  (define state (vector #f))
  ;; let's not skip the prompt, 0 prompt is also fine I guess
  (define (check-state r) (and r (eq? (vector-ref state 0) 'received) r))
  (let ((prompt-sender (make-prompt-sender transport state))
	(response-receiver (make-response-receiver transport state)))
    (check-state
     (ssh-authenticate-user (~ msg 'service-name)
			    (make <ssh-interactive-credential>
			      :username (~ msg 'user-name)
			      :prompt-sender prompt-sender
			      :response-receiver response-receiver)))))

(define ((make-prompt-sender transport state) prompts)
  (let ((info-request (make <ssh-msg-userauth-info-request>
			:name ""
			:instruction ""
			:num-prompts (vector-length prompts))))
    (let-values (((out e) (open-bytevector-output-port)))
      (ssh-write-message info-request out)
      (vector-for-each (lambda (p)
			 (ssh-write-message
			  (make <ssh-msg-userauth-prompt>
			    :prompt (ssh-interactive-prompt-message p)
			    :echo (ssh-interactive-prompt-echo? p))
			  out)) prompts)
      (ssh-write-packet transport (e))
      (vector-set! state 0 'requested))))

(define ((make-response-receiver transport state))
  (let ((bv (ssh-read-packet transport)))
    (unless (= (bytevector-u8-ref bv 0) +ssh-msg-userauth-info-response+)
      (error 'keyboard-interactive "unexpected message"))
    (let* ((bin (open-bytevector-input-port bv))
	   (header (ssh-read-message <ssh-msg-userauth-info-response> bin))
	   (n (~ header 'num-response))
	   (vec (make-vector n)))
      (do ((i 0 (+ i 1)))
	  ((= i n) (vector-set! state 0 'received) vec)
	(vector-set! vec i (ssh-read-message :utf8-string bin #f))))))

)
