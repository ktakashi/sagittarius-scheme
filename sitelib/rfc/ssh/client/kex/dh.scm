;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/client/kex/dh.scm - SSH2 protocol DH key exchange
;;;  
;;;   Copyright (c) 2010-2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh client kex dh)
    (export ssh-client-exchange-kex-message)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius crypto digests) 
	    (sagittarius crypto random)
	    (srfi :13 strings)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (rfc ssh transport kex dh) ;; dh specificx
	    (rfc ssh client kex api))

(define-method ssh-client-exchange-kex-message
  ((m (member ssh-dh-group-exchanges)) transport client-packet server-packet)
  (define (generate-e&x transport)
    (let ((gex-req (make <ssh-msg-kex-dh-gex-request>)))
      (ssh-write-ssh-message transport gex-req)
      (let* ((reply (ssh-read-packet transport))
	     (gex-group (read-message <ssh-msg-kex-dh-gex-group>
				      (open-bytevector-input-port reply)))
	     (p (~ gex-group 'p))
	     (g (~ gex-group 'g)))
	(let ((x (generate-x (~ transport 'prng) (div (- p 1) 2))))
	  (values p g x (mod-expt g x p) gex-req)))))
  (let-values (((p g x e req) (generate-e&x transport)))
    (ssh-kex-send/receive transport (make-init-class <ssh-msg-kex-dh-gex-init> e)
			  <ssh-msg-kex-dh-gex-reply>
			  (compute-k p x
			   (lambda (K-S f K)
			     (make <SSH-GEX-H> 
			       :V-C (~ transport 'client-version)
			       :V-S (~ transport 'server-version)
			       :I-C client-packet
			       :I-S server-packet
			       :K-S K-S
			       :min (~ req 'min) :n (~ req 'n) :max (~ req 'max)
			       :p p :g g :e e :f f :K K))))))

(define-method ssh-client-exchange-kex-message
  ((m (member ssh-dh-groups)) transport client-packet server-packet)
  (define (group-n kex)
    (cond ((#/group(\d+)/ kex) =>
	   (lambda (m) (string->number (m 1))))
	  (else (error 'ssh-exchange-kex-message "must not happen"))))
  (define (generate-e&x transport)
    (let-values (((p g) (case (group-n m)
			  ((1)  (values +dh-group1-p+ +dh-group1-g+))
			  ((14) (values +dh-group14-p+ +dh-group14-g+))
			  ((15) (values +dh-group15-p+ +dh-group15-g+))
			  ((16) (values +dh-group16-p+ +dh-group16-g+))
			  ((17) (values +dh-group17-p+ +dh-group17-g+))
			  ((18) (values +dh-group18-p+ +dh-group18-g+))
			  (else (error 'ssh-exchange-kex-message "unknown group"
				       (~ transport 'kex))))))
      (let ((x (generate-x (~ transport 'prng) p)))
	(values p g x (mod-expt g x p) #f))))
  (let-values (((p g x e req) (generate-e&x transport)))
    (ssh-kex-send/receive transport (make-init-class <ssh-msg-kexdh-init> e)
			  <ssh-msg-kexdh-reply>
			  (compute-k p x
			   (lambda (K-S f K)
			     (make <SSH-DH-H> 
			       :V-C (~ transport 'client-version)
			       :V-S (~ transport 'server-version)
			       :I-C client-packet
			       :I-S server-packet
			       :K-S K-S
			       :e e :f f :K K))))))

;; private
(define ((make-init-class init-class e))
  (make init-class :e e))

(define ((compute-k p x make-verify-message) transport kex-reply)
  (let* ((K-S (~ kex-reply 'K-S))
	 (f   (~ kex-reply 'f))
	 (K   (mod-expt f x p)))
    ;; verify signature
    (let* ((msg (make-verify-message K-S f K))
	   (h (ssh-verify-signature transport msg K-S (~ kex-reply 'signature))))
      (values K h))))

(define (generate-x prng r)
  (define size (div (bitwise-length r) 8))
  (let loop ()
    (let ((v (bytevector->integer
	      (random-generator-read-random-bytes prng size))))
      (or (and (< 1 v) v)
	  (loop)))))
)
