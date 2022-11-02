;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/signatures.scm - X.509 signature related operations
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

#!nounbound
(library (sagittarius crypto pkix signatures)
    (export x509-algorithm-identifier->signer
	    x509-algorithm-identifier->verifier
	    
	    x509-rsassa-pss-params? <x509-rsassa-pss-params>
	    make-x509-rsassa-pss-param
	    x509-rsassa-pss-params-digest
	    x509-rsassa-pss-params-mgf
	    x509-rsassa-pss-params-mgf-digest
	    x509-rsassa-pss-params-salt-length
	    x509-rsassa-pss-params-trailer-field)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (sagittarius crypto asn1)
	    (sagittarius crypto digests)
	    (sagittarius crypto keys)
	    (sagittarius crypto signatures)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto random)
	    (sagittarius mop immutable)
	    (sagittarius combinators))

(define (x509-algorithm-identifier->signer (sa x509-algorithm-identifier?) 
					   (private-key private-key?)
					   . opts)
  (let ((oid (x509-algorithm-identifier-oid sa))
	(param (x509-algorithm-identifier-parameters sa)))
    (apply (oid->signer-maker oid) private-key
	   (append! (x509-algorithm-parameters->keyword-parameters param)
		    opts))))

(define (x509-algorithm-identifier->verifier (sa x509-algorithm-identifier?) 
					     (public-key public-key?)
					     . opts)
  (let ((oid (x509-algorithm-identifier-oid sa))
	(param (x509-algorithm-identifier-parameters sa)))
    (apply (oid->verifier-maker oid) public-key
	   (append! (x509-algorithm-parameters->keyword-parameters param)
		    opts))))

;; This shouldn't be called X509, though it's defined in X.509 thing...
(define-class <x509-rsassa-pss-params> (<x509-algorithm-parameters> <immutable>)
  ((digest :init-keyword :digest :init-value *digest:sha-1*
	   :reader x509-rsassa-pss-params-digest)
   (mgf :init-keyword :mgf :init-value mgf-1
	:reader x509-rsassa-pss-params-mgf)
   (mgf-digest :init-keyword :mgf-digest :init-value *digest:sha-1*
	       :reader x509-rsassa-pss-params-mgf-digest)
   (salt-length :init-keyword :salt-length :init-value 20
		:reader x509-rsassa-pss-params-salt-length)
   (trailer-field :init-keyword :trailer-field :init-value 1
		  :reader x509-rsassa-pss-params-trailer-field)
   ;; to generate salt
   (prng :init-keyword :prng
	 :init-value (secure-random-generator *prng:chacha20*)
	 :reader x509-rsassa-pss-params-prng)))
(define-method write-object ((o <x509-rsassa-pss-params>) p)
  (format p "#<x509-rsassa-pss-params digest=~a mgf=~a mgf-digest=~a salt-length=~a>"
	  (x509-rsassa-pss-params-digest o)
	  (x509-rsassa-pss-params-mgf o)
	  (x509-rsassa-pss-params-mgf-digest o)
	  (x509-rsassa-pss-params-salt-length o)))

(define (x509-rsassa-pss-params? o) (is-a? o <x509-rsassa-pss-params>))
(define (make-x509-rsassa-pss-param
	 :key digest mgf mgf-digest salt-length trailer-field prng)
  (make <x509-rsassa-pss-params>
    :digest digest :mgf mgf :mgf-digest mgf-digest
    :salt-length salt-length :trailer-field trailer-field :prng prng))

(define-method x509-algorithm-parameters->algorithm-parameters
  ((p <x509-rsassa-pss-params>))
  (define (digest->aid digest)
    (make <algorithm-identifier>
      :algorithm (oid-string->der-object-identifier
		  (digest-descriptor-oid digest))))
  (define (mgf-aid p)
    (make <algorithm-identifier>
      ;; FIXME, not sure how we can do...
      :algorithm (oid-string->der-object-identifier *oid:mgf1*)
      :parameters (digest->aid (x509-rsassa-pss-params-mgf-digest p))))
  (define i->di integer->der-integer)
  (make <rsassa-pss-params>
    :hash-algorithm (digest->aid (x509-rsassa-pss-params-digest p))
    :mask-gen-algorithm (mgf-aid p)
    :salt-length (i->di (x509-rsassa-pss-params-salt-length p))
    :trailer-field (i->di (x509-rsassa-pss-params-trailer-field p))))

(define-method algorithm-parameters->x509-algorithm-parameters
  ((oid (equal *signature-algorithm:rsa-ssa-pss*)) parameters)
  (define aid->oid (.$ der-object-identifier->oid-string
		       algorithm-identifier-algorithm))
  (define (->mgf-hash p)
    (if (algorithm-identifier? p)
	p
	(asn1-object->asn1-encodable <algorithm-identifier> p)))
  (let* ((p (cond ((rsassa-pss-params? parameters) parameters)
		  ((not parameters)
		   (assertion-violation
		    'algorithm-parameters->signature-parameters
		    "RSA PSS-SSA requires parameters"))
		  (else
		   (asn1-object->asn1-encodable <rsassa-pss-params>
						parameters))))
	 (hash (rsassa-pss-params-hash-algorithm p))
	 (mgf (rsassa-pss-params-mask-gen-algorithm p))
	 (mgf-hash (and mgf (->mgf-hash (algorithm-identifier-parameters mgf))))
	 (salt-length (rsassa-pss-params-salt-length p))
	 (trailer-field (rsassa-pss-params-trailer-field p)))
    (make <x509-rsassa-pss-params>
      :digest (or (and hash (oid->digest-descriptor (aid->oid hash)))
		  *digest:sha-1*)
      :mgf (or (and mgf (oid->mgf (aid->oid mgf))) mgf-1)
      :mgf-digest (or (and hash (oid->digest-descriptor (aid->oid mgf-hash)))
		      *digest:sha-1*)
      :salt-length (or (and salt-length (der-integer->integer salt-length)) 20)
      :trailer-field (or (and trailer-field
			      (der-integer->integer trailer-field))
			 1))))

(define-method x509-algorithm-parameters->keyword-parameters
  ((p <x509-rsassa-pss-params>))
  (let ((salt-len (x509-rsassa-pss-params-salt-length p))
	(prng (x509-rsassa-pss-params-prng p)))
    (list :digest (x509-rsassa-pss-params-digest p)
	  :mgf (x509-rsassa-pss-params-mgf p)
	  :mgf-digest (x509-rsassa-pss-params-mgf-digest p)
	  :salt (random-generator-read-random-bytes prng salt-len)
	  :salt-length salt-len
	  :trailer-field (x509-rsassa-pss-params-trailer-field p))))

)
