;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/key-manager.scm - Client private key manager
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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
(library (net http-client key-manager)
    (export make-key-manager
	    (rename (make-key-manager list->key-manager)
		    (%key-manager key-manager))
	    key-manager? key-manager->certificate-callback
	    key-provider? (rename (key-provider <key-provider>))
	    make-keystore-key-provider keystore-key-provider?
	    keystore-key-proider-add-key-retriever!)
    (import (rnrs)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (security keystore))

(define-record-type key-manager
  (fields key-providers)
  (protocol (lambda (p)
	      (lambda (providers)
		(unless (for-all key-provider? providers)
		  (assertion-violation 'make-key-manager
				       "List of key-provider is required"
				       providers))
		(p providers)))))
(define (%key-manager . providers) (make-key-manager providers))

(define (key-manager->certificate-callback key-manager)
  (define providers (key-manager-key-providers key-manager))
  (define key-retrievers
    (append-map list-queue-list (map key-provider-key-retrievers providers)))
  (lambda (socket)
    (exists (lambda (r) (r socket)) key-retrievers)))

(define-record-type key-provider
  (fields key-retrievers)
  (protocol (lambda (p)
	      (lambda (get-key)
		(let ((key-retrievers (list-queue)))
		  (list-queue-add-front! key-retrievers get-key)
		  (p key-retrievers))))))

(define (->keystore-get-key keystore password alias-provider)
  ;; for now passing raw tls-socket
  (lambda (socket)
    (cond ((alias-provider socket) =>
	   (lambda (alias)
	     (cons* (keystore-get-key keystore alias password)
		    (keystore-get-certificate keystore alias)
		    (or (keystore-get-certificate-chain keystore alias) '()))))
	  (else #f))))
(define-record-type keystore-key-provider
  (parent key-provider)
  (fields keystore)
  (protocol (lambda (n)
	      (lambda (keystore password alias-provider)
		((n (->keystore-get-key keystore password alias-provider))
		 keystore)))))

(define (keystore-key-proider-add-key-retriever! kp password alias-provider)
  (let ((ks (keystore-key-provider-keystore kp)))
    (list-queue-add-front! (key-provider-key-retrievers key-provider)
			   (->keystore-get-key ks password alias-provider))
    key-provider))
)
