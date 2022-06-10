;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/amqp/security - AMQP v1.0 security
;;;  
;;;   Copyright (c) 2012  Takashi Kato  <ktakashi@ymail.com>
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

;; reference:
;; http://docs.oasis-open.org/amqp/core/v1.0/os/amqp-core-security-v1.0-os.html
#!nounbound
(library (net mq amqp security)
    (export amqp:sasl-negotiation

	    ;; Below are probably not needed
	    amqp-sasl-mechanisms?
	    amqp-sasl-init?
	    amqp-sasl-challenge?
	    amqp-sasl-response?
	    amqp-sasl-outcome?
	    amqp-sasl-code?
	    amqp-sasl-outcome?

	    +sasl-code-ok+      
	    +sasl-code-auth+    
	    +sasl-code-sys+     
	    +sasl-code-sys-perm+
	    +sasl-code-sys-temp+)
    (import (except (rnrs) fields)
	    (net mq amqp types)
	    (clos user)
	    (rfc sasl)
	    (sagittarius)
	    (sagittarius object))

(define (amqp:sasl-negotiation sasl-context hostname receiver sender)
  (define (make-init m s h)
    (make-amqp-sasl-init
     :mechanism (sasl-authentication-mechanism-name m)
     :initial-response (sasl-authentication-state-message s)
     :hostname h))
  (let ((mechanism (receiver)))
    (unless (amqp-sasl-mechanisms? mechanism)
      (assertion-violation 'amqp:sasl-negotiation "Unexpected frame"
			   mechanism))
    (let* ((mechanisms (~ mechanism 'sasl-server-mechanisms))
	   (mechanism (sasl-select-mechanism sasl-context mechanisms))
	   ;; For AMQP, server-challenge always comes after sasl-init
	   ;; so I don't think CRAM-MD5 or other server challenge
	   ;; required mechanism can't be implemented (or we can make
	   ;; those implementation flexible?)
	   (initial-state (sasl-start-client-authentication mechanism #f)))
      (sender (make-init mechanism initial-state hostname))
      (let loop ((frame (receiver)))
	(cond ((amqp-sasl-outcome? frame)
	       (unless (= (~ frame 'code) +sasl-code-ok+)
		 (assertion-violation 'amqp:sasl-negotiation
				      "Failed to authenticate"
				      (~ frame 'code))))
	      (else
	       (assertion-violation 'amqp:sasl-negotiation
				    "Not yet...")))))))


;; Messages
(define-composite-type sasl-mechanisms amqp:sasl-mechanism:list
  #x00000000 #x00000040
  ((sasl-server-mechanisms :type :symbol :mandatory #t :multiple #t))
  :provides (sasl-frame))

(define-method write-object ((t <amqp-sasl-mechanisms>) out)
  (format out "#<sasl-mechanisms ~a>" (~ t 'sasl-server-mechanisms)))

(define-composite-type sasl-init amqp:sasl-init:list
  #x00000000 #x00000041
  ((mechanism :type :symbol :mandatory #t)
   (initial-response :type :binary)
   (hostname :type :string))
  :provides (sasl-frame))
(define-method write-object ((t <amqp-sasl-init>) out)
  (format out "#<sasl-init ~a>" (~ t 'mechanism)))

(define-composite-type sasl-challenge amqp:sasl-challenge:list
  #x00000000 #x00000042
  ((challenge :type :binary :mandatory #t))
  :provides (sasl-frame))

(define-composite-type sasl-response amqp:sasl-response:list
  #x00000000 #x00000043
  ((response :type :binary :mandatory #t))
  :provides (sasl-frame))

(define-restricted-type sasl-code :ubyte)
(define-constant +sasl-code-ok+       0)
(define-constant +sasl-code-auth+     1)
(define-constant +sasl-code-sys+      2)
(define-constant +sasl-code-sys-perm+ 3)
(define-constant +sasl-code-sys-temp+ 4)

(define-composite-type sasl-outcome amqp:sasl-outcome:list
  #x00000000 #x00000044
  ((code :type sasl-code :mandatory #t)
   (additional-data :type :binary))
  :provides (sasl-frame))
(define-method write-object ((t <amqp-sasl-outcome>) out)
  (format out "#<sasl-outcome ~a>" (~ t 'code)))
  
)
