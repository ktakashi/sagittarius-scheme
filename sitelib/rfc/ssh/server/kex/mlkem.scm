;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/kex/mlkem.scm - SSH ML-KEM/X25519 hybrid key exchange
;;;
;;;   Copyright (c) 2026  Sagittarius contributors.
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
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;;   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;;   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

#!nounbound
(library (rfc ssh server kex mlkem)
    (export ssh-server-exchange-kex-message)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius crypto keys)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport)
	    (rfc ssh transport kex hybrid)
	    (rfc ssh server kex api))

(define-method ssh-server-exchange-kex-message
  ((name (member ssh-hybrid-kex-names)) transport server-packet client-packet)
  (define (compute-k K-S init)
    (let ((C-INIT (~ init 'C-INIT)))
      (let-values (((C-PK2 C-PK1)
		    (ssh-hybrid-client-init-components C-INIT)))
	(let* ((C-PUB (import-public-key *key:ml-kem-768* C-PK2))
	       (C-X25519-PUB
		(import-public-key *key:x25519* C-PK1
				   (public-key-format raw)))
	       (x25519-key-pair (generate-key-pair *key:x25519*)))
	  (let-values (((S-CT2 K-PQ)
			(encapsulate *key:ml-kem-768* C-PUB)))
	    (let* ((S-PK1 (export-public-key
			    (key-pair-public x25519-key-pair)
			    (public-key-format raw)))
		   (K-CL (calculate-key-agreement *key:x25519*
			  (key-pair-private x25519-key-pair) C-X25519-PUB)))
	      (when (bytevector=? K-CL (make-bytevector 32 0))
		(error 'ssh-server-exchange-kex-message
		       "Invalid X25519 shared secret"))
	      (let* ((S-REPLY (bytevector-append S-CT2 S-PK1))
		     (K (ssh-hybrid-combine-secrets K-PQ K-CL))
		     (hash-message
		      (make <SSH-HYBRID-H>
			:V-C (~ transport 'client-version)
			:V-S (~ transport 'server-version)
			:I-C client-packet :I-S server-packet
			:K-S K-S :C-INIT C-INIT
			:S-REPLY S-REPLY :K K)))
		(values K hash-message S-REPLY))))))))
  (define (make-reply K-S signature S-REPLY)
    (make <ssh-msg-kex-hybrid-reply>
      :K-S K-S
      :S-REPLY S-REPLY
      :signature (ssh-message->bytevector signature)))
  (ssh-kex-receive/send
   transport <ssh-msg-kex-hybrid-init> compute-k make-reply
   :validate-init
   (lambda (packet message)
     (unless (and (ssh-hybrid-valid-packet?
		   packet message +ssh-msg-kex-hybrid-init+)
		  (= (bytevector-length (~ message 'C-INIT)) (+ 1184 32)))
       (error 'ssh-server-exchange-kex-message
	      "Malformed hybrid key exchange initialization")))
   :shared-secret-encoding :string))

)
