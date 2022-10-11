;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/signatures/eddsa.scm - EDDSA signature
;;;
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/bv-string
#!nounbound
(library (sagittarius crypto signatures eddsa)
    (export *signature:ed25519*
	    *signature:ed25519ctx*
	    *signature:ed25519ph*
	    *signature:ed448*
	    *signature:ed448ph*

	    make-signer-state signer-state->signature
	    make-verifier-state verifier-state-verify-message)
    (import (rnrs)
	    (core misc)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto signatures types)
	    (sagittarius crypto keys)
	    (sagittarius crypto math modular)
	    (sagittarius crypto math prime)
	    (sagittarius crypto math ed)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (sagittarius crypto secure)
	    (util bytevector))

(define *signature:ed25519*    *key:ed25519*)
(define *signature:ed25519ctx* :ed25519ctx)
(define *signature:ed25519ph*  :ed25519ph)
(define *signature:ed448*      *key:ed448*)
(define *signature:ed448ph*    :ed448ph)

;; For convenience
(define-method generate-public-key ((m (eql *signature:ed25519ctx*)) . opts)
  (apply generate-public-key *key:ed25519* opts))
(define-method generate-public-key ((m (eql *signature:ed25519ph*)) . opts)
  (apply generate-public-key *key:ed25519* opts))
(define-method generate-private-key ((m (eql *signature:ed25519ctx*)) . opts)
  (apply generate-private-key *key:ed25519* opts))
(define-method generate-private-key ((m (eql *signature:ed25519ph*)) . opts)
  (apply generate-private-key *key:ed25519* opts))
(define-method generate-key-pair ((m (eql *signature:ed25519ctx*)) . opts)
  (apply generate-key-pair *key:ed25519* opts))
(define-method generate-key-pair ((m (eql *signature:ed25519ph*)) . opts)
  (apply generate-key-pair *key:ed25519* opts))
(define-method import-public-key ((m (eql *signature:ed25519ctx*)) . opts)
  (apply import-public-key *key:ed25519* opts))
(define-method import-public-key ((m (eql *signature:ed25519ph*)) . opts)
  (apply import-public-key *key:ed25519* opts))
(define-method export-public-key ((m (eql *signature:ed25519ctx*)) . opts)
  (apply export-public-key *key:ed25519* opts))
(define-method export-public-key ((m (eql *signature:ed25519ph*)) . opts)
  (apply export-public-key *key:ed25519* opts))
(define-method import-private-key ((m (eql *signature:ed25519ctx*)) . opts)
  (apply import-private-key *key:ed25519* opts))
(define-method import-private-key ((m (eql *signature:ed25519ph*)) . opts)
  (apply import-private-key *key:ed25519* opts))
(define-method export-private-key ((m (eql *signature:ed25519ctx*)) . opts)
  (apply import-private-key *key:ed25519* opts))
(define-method export-private-key ((m (eql *signature:ed25519ph*)) . opts)
  (apply import-private-key *key:ed25519* opts))

(define-method generate-public-key ((m (eql *signature:ed448ph*)) . opts)
  (apply generate-public-key *key:ed448* opts))
(define-method generate-private-key ((m (eql *signature:ed448ph*)) . opts)
  (apply generate-private-key *key:ed448* opts))
(define-method generate-key-pair ((m (eql *signature:ed448ph*)) . opts)
  (apply generate-key-pair *key:ed448* opts))
(define-method import-public-key ((m (eql *signature:ed448ph*)) . opts)
  (apply import-public-key *key:ed448* opts))
(define-method export-public-key ((m (eql *signature:ed448ph*)) . opts)
  (apply export-public-key *key:ed448* opts))
(define-method import-private-key ((m (eql *signature:ed448ph*)) . opts)
  (apply import-private-key *key:ed448* opts))
(define-method export-private-key ((m (eql *signature:ed448ph*)) . opts)
  (apply import-private-key *key:ed448* opts))

(define-class <eddsa-state> (<buffered-signature-state>)
  ((scheme :init-keyword :scheme :reader eddsa-state-scheme)
   (context :init-keyword :context :reader eddsa-state-context)))
(define-class <eddsa-signer-state> (<eddsa-state> <signer-state>) ())
(define-class <eddsa-verifier-state> (<eddsa-state> <verifier-state>) ())

(define-method make-signer-state ((m (eql *signature:ed25519*))
				  (key <eddsa-private-key>)
				  :allow-other-keys opts)
  (make <eddsa-signer-state> :key key :scheme ed25519-scheme :context #vu8()))
(define-method make-signer-state ((m (eql *signature:ed25519ctx*))
				  (key <eddsa-private-key>)
				  :key ((context bytevector?) #vu8())
				  :allow-other-keys opts)
  (make <eddsa-signer-state> :key key :scheme ed25519ctx-scheme
	:context context))
(define-method make-signer-state ((m (eql *signature:ed25519ph*))
				  (key <eddsa-private-key>)
				  :key ((context bytevector?) #vu8())
				  :allow-other-keys opts)
  (make <eddsa-signer-state> :key key :scheme ed25519ph-scheme
	:context context))
(define-method make-signer-state ((m (eql *signature:ed448*))
				  (key <eddsa-private-key>)
				  :key ((context bytevector?) #vu8())
				  :allow-other-keys opts)
  (make <eddsa-signer-state> :key key :scheme ed448-scheme :context context))
(define-method make-signer-state ((m (eql *signature:ed448ph*))
				  (key <eddsa-private-key>)
				  :key ((context bytevector?) #vu8())
				  :allow-other-keys opts)
  (make <eddsa-signer-state> :key key :scheme ed448ph-scheme :context context))

(define-method signer-state->signature ((state <eddsa-signer-state>))
  (define key (signature-state-key state))
  (define scheme (eddsa-state-scheme state))
  (define ctx (eddsa-state-context state))
  (define prehash (eddsa-scheme-prehash scheme))
  (define inithash (eddsa-scheme-inithash scheme))
  (define parameter (eddsa-scheme-parameter scheme))
  (define pub-key (eddsa-public-key-data (eddsa-private-key-public-key key)))

  (define msg
    (let ((bv (buffered-signature-state-signing-message! state)))
      (or (and prehash (prehash bv ctx)) bv)))
  (define l (eddsa-parameter-L parameter))
  (define c (eddsa-parameter-c parameter))
  (define n (eddsa-parameter-n parameter))
  (define b (eddsa-parameter-b parameter))
  (define B (eddsa-parameter-B parameter))
  (let* ((khash (inithash (eddsa-private-key-random key) #f prehash))
	 (a (bytevector->integer/endian
	     (eddsa-clamp! (bytevector-copy khash 0 (div b 8)) c n b)
	     (endianness little)))
	 (seed (bytevector-copy khash (div b 8)))
	 (r (bytevector->integer/endian
	     (inithash (bytevector-append seed msg) ctx prehash)
	     (endianness little)))
	 (R (ed-point-encode-base parameter (ed-point-mul parameter B r) b))
	 (h (mod (bytevector->integer/endian
		  (inithash (bytevector-append R pub-key msg) ctx prehash)
		  (endianness little))
		 l))
	 (S (integer->bytevector/endian (mod (+ r (* h a)) l)
					(endianness little)
					(div b 8))))
    ;; It seems this can also be DER format, though RFC 8032 doesn't specify it
    (bytevector-append R S)))

(define-method make-verifier-state ((m (eql *signature:ed25519*))
				    (key <eddsa-public-key>)
				    :allow-other-keys opts)
  (make <eddsa-verifier-state> :key key :scheme ed25519-scheme :context #vu8()))
(define-method make-verifier-state ((m (eql *signature:ed25519ctx*))
				    (key <eddsa-public-key>)
				    :key ((context bytevector?) #vu8())
				    :allow-other-keys opts)
  (make <eddsa-verifier-state> :key key :scheme ed25519ctx-scheme
	:context context))
(define-method make-verifier-state ((m (eql *signature:ed25519ph*))
				    (key <eddsa-public-key>)
				    :key ((context bytevector?) #vu8())
				    :allow-other-keys opts)
  (make <eddsa-verifier-state> :key key :scheme ed25519ph-scheme
	:context context))
(define-method make-verifier-state ((m (eql *signature:ed448*))
				    (key <eddsa-public-key>)
				    :key ((context bytevector?) #vu8())
				    :allow-other-keys opts)
  (make <eddsa-verifier-state> :key key :scheme ed448-scheme :context context))
(define-method make-verifier-state ((m (eql *signature:ed448ph*))
				    (key <eddsa-public-key>)
				    :key ((context bytevector?) #vu8())
				    :allow-other-keys opts)
  (make <eddsa-verifier-state> :key key :scheme ed448ph-scheme
	:context context))

(define-method verifier-state-verify-message ((state <eddsa-verifier-state>)
					      (signature <bytevector>))
  (define key (signature-state-key state))
  (define scheme (eddsa-state-scheme state))
  (define ctx (eddsa-state-context state))
  (define prehash (eddsa-scheme-prehash scheme))
  (define inithash (eddsa-scheme-inithash scheme))
  (define parameter (eddsa-scheme-parameter scheme))
  (define pub-key (eddsa-public-key-data key))
  
  (define msg
    (let ((bv (buffered-signature-state-signing-message! state)))
      (or (and prehash (prehash bv ctx)) bv)))
  (define l (eddsa-parameter-L parameter))
  (define c (eddsa-parameter-c parameter))
  (define n (eddsa-parameter-n parameter))
  (define b (eddsa-parameter-b parameter))
  (define B (eddsa-parameter-B parameter))
  (unless (and (= (bytevector-length signature) (div b 4))
	       (= (bytevector-length pub-key) (div b 8)))
    (assertion-violation 'verifier-state-verify-message
			 "Invalid EdDSA signature"))
  (let* ((r-raw (bytevector-copy signature 0 (div b 8)))
	 (s-raw (bytevector-copy signature (div b 8)))
	 (R (ed-point-decode-base parameter r-raw b))
	 (S (bytevector->integer/endian s-raw (endianness little)))
	 (A (ed-point-decode-base parameter pub-key b))
	 (h (mod (bytevector->integer/endian
		  (inithash (bytevector-append r-raw pub-key msg) ctx prehash)
		  (endianness little))
		 l)))
    (and R A (< S l)
	 (let loop ((i 0)
		    (lhs (ed-point-mul parameter B S))
		    (rhs (ed-point-add parameter R
				       (ed-point-mul parameter A h))))
	   (if (= i c)
	       (ed-point=? parameter lhs rhs)
	       (loop (+ i 1)
		     (ed-point-double parameter lhs)
		     (ed-point-double parameter rhs)))))))

(define-vector-type eddsa-scheme
  (make-eddsa-scheme parameter prehash inithash) eddsa-scheme?
  (parameter eddsa-scheme-parameter)
  (prehash eddsa-scheme-prehash)
  (inithash eddsa-scheme-inithash))

(define (sha-512 bv) (digest-message (make-message-digest *digest:sha-512*) bv))
(define ed25519-scheme
  (make-eddsa-scheme ed25519-parameter
    #f (lambda (data ctx hflag)
	 (when (or hflag (and ctx (> (bytevector-length ctx) 0)))
	   (assertion-violation 'ed25519 "Context/hashes not supported"))
	 (sha-512 data))))

(define (ed25519ctx-inithash data ctx hflag)
  (let ((dom-prefix (if ctx
			(if (< (bytevector-length ctx) 256)
			    (bytevector-append
			     #*"SigEd25519 no Ed25519 collisions"
			     (if hflag #vu8(1) #vu8(0))
			     (make-bytevector 1 (bytevector-length ctx))
			     ctx)
			    (assertion-violation 'Ed25519 "Context too big"))
			#vu8())))
    (sha-512 (bytevector-append dom-prefix data))))

(define ed25519ctx-scheme
  (make-eddsa-scheme ed25519-parameter #f ed25519ctx-inithash))

(define ed25519ph-scheme
  (make-eddsa-scheme ed25519-parameter (lambda (x y) (sha-512 x))
		     ed25519ctx-inithash))

(define ((shake-256 size) bv)
  (digest-message (make-message-digest *digest:shake-256*) bv size))
(define shake-256-114 (shake-256 114))
(define shake-256-64 (shake-256 64))
(define (ed448-inithash data ctx hflag)
  (let ((dom-prefix (if ctx
			(if (< (bytevector-length ctx) 256)
			    (bytevector-append
			     #*"SigEd448"
			     (if hflag #vu8(1) #vu8(0))
			     (make-bytevector 1 (bytevector-length ctx))
			     ctx)
			    (assertion-violation 'Ed448
						 "Context too big"))
			#vu8())))
    (shake-256-114 (bytevector-append dom-prefix data))))
(define ed448-scheme
  (make-eddsa-scheme ed448-parameter #f ed448-inithash))
(define ed448ph-scheme
  (make-eddsa-scheme ed448-parameter (lambda (data ctx) (shake-256-64 data))
		     ed448-inithash))
)
