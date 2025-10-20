;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/x509 - X.509 certificate utility library.
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

#!nounbound
(library (rfc x509)
    (export make-x509-certificate
	    x509-certificate?
	    bytevector->x509-certificate read-x509-certificate
	    x509-certificate->bytevector write-x509-certificate

	    x509-certificate->bytevector <x509-certificate>

	    x509-certificate-expired?
	    x509-certificate-issuer-dn
	    x509-certificate-subject-dn
	    x509-certificate-public-key
	    x509-certificate-validity
	    x509-certificate-serial-number
	    x509-certificate-version
	    x509-certificate-signature
	    x509-certificate-signature-algorithm
	    x509-certificate-extensions

	    validate-x509-certificate
	    x509-certificate-signature-validator
	    x509-certificate-validity-validator

	    x509-name? <x509-name> x509-name
	    x509-name->string
	    list->x509-name

	    x509-validity? <x509-validity>
	    make-x509-validity
	    x509-validity-not-before
	    x509-validity-not-after
	    
	    x509-certificate-template?
	    x509-certificate-template-builder
	    sign-x509-certificate-template

	    ;; Below are only for backward compatibilities
	    (rename (x509-certificate-version x509-certificate-get-version)
		    (x509-certificate-serial-number
		     x509-certificate-get-serial-number)
		    (x509-certificate-issuer-dn x509-certificate-get-issuer-dn)
		    (x509-certificate-subject-dn
		     x509-certificate-get-subject-dn))
	    x509-certificate-get-not-before
	    x509-certificate-get-not-after
	    x509-certificate-get-signature-algorithm
	    (rename (x509-certificate-signature x509-certificate-get-signature)
		    (x509-certificate-public-key
		     x509-certificate-get-public-key))
	    
	    x509:verify
	    x509:check-validity
	    x509:verify-certificate
	    ;; for backward compatibility
	    (rename (x509:verify verify)
		    (x509:check-validity check-validity))

	    ;; certificate generation
	    make-x509-issuer
	    make-x509-validity
	    (rename (make-x509-validity make-validity))
	    make-x509-simple-certificate
	    make-x509-self-signed-certificate
	    ;; for backward compatibility
	    (rename (make-x509-self-signed-certificate
		     make-x509-basic-certificate)))
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys)
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix certificate)	    
	    (sagittarius combinators)
	    (srfi :19 time))

(define-generic make-x509-certificate)
(define-method  make-x509-certificate ((bv <bytevector>))
  (bytevector->x509-certificate bv))
(define-method  make-x509-certificate ((p <port>))
  (read-x509-certificate p))
(define-method  make-x509-certificate ((o <asn1-object>))
  (asn1-object->x509-certificate o))

(define x509-certificate-get-not-before
  (.$ x509-validity-not-before x509-certificate-validity))
(define x509-certificate-get-not-after
  (.$ x509-validity-not-after x509-certificate-validity))
(define x509-certificate-get-signature-algorithm
  (.$ x509-algorithm-identifier-oid x509-certificate-signature-algorithm))

;; **DEPRECATED** It's a wrong idea to use certificate to verify a signature
;;                use (sagittarius crypto signatures) instead
(define (x509:verify cert message signature . ignore)
  (define oid (x509-certificate-signature-algorithm cert))
  (define verifier ((oid->verifier-maker oid)
		    (x509-certificate-public-key cert)
		    :der-encode #f))
  (verifier-verify-signature verifier message signature))

(define (x509:verify-certificate cert public-key)
  (validate-x509-certificate cert
    (x509-certificate-signature-validator public-key)))
  
(define (x509:check-validity cert :optional (date (current-date)))
  (validate-x509-certificate cert
    (x509-certificate-validity-validator date)))

(define +sha256-with-rsa-encryption+
  *signature-algorithm:rsa-pkcs-v1.5-sha256*)
(define +sha256-with-ecdsa-encryption+ *signature-algorithm:ecdsa-sha256*)
(define +eddsa-ed25519-encryption+ *signature-algorithm:ed25519*)
(define +eddsa-ed448-encryption+ *signature-algorithm:ed448*)

(define (make-x509-issuer lis)
  (apply x509-name (map (lambda (c) (list (car c) (cdr c))) lis)))

(define (make-x509-self-signed-certificate keypair serial-number
					   issuer validity subject)
  (define private-key (key-pair-private keypair))
  (define oid
    (cond ((rsa-private-key? private-key) +sha256-with-rsa-encryption+)
	  ((ecdsa-private-key? private-key) +sha256-with-ecdsa-encryption+)
	  ((eddsa-private-key? private-key)
	   (if (ed25519-key? private-key)
	       +eddsa-ed25519-encryption+
	       +eddsa-ed448-encryption+))
	  (else (assertion-violation 'make-x509-self-signed-certificate
				     "Keypair not supported" keypair))))
  (sign-x509-certificate-template
   (x509-certificate-template-builder
    (issuer-dn issuer)
    (subject-dn subject)
    (serial-number serial-number)
    (not-before (x509-validity-not-before validity))
    (not-after (x509-validity-not-after validity))
    (public-key (key-pair-public keypair)))
   oid private-key))

(define (make-x509-simple-certificate public-key
				      serial-number subject validity
				      issuer-cert issuer-private-key)
  (define private-key issuer-private-key)
  (define oid
    (cond ((rsa-private-key? private-key) +sha256-with-rsa-encryption+)
	  ((ecdsa-private-key? private-key) +sha256-with-ecdsa-encryption+)
	  ((eddsa-private-key? private-key)
	   (if (ed25519-key? private-key)
	       +eddsa-ed25519-encryption+
	       +eddsa-ed448-encryption+))
	  (else (assertion-violation 'make-x509-simple-certificate
				     "Private key not supported"
				     private-key))))
  (sign-x509-certificate-template
   (x509-certificate-template-builder
    (issuer-dn (x509-certificate-subject-dn issuer-cert))
    (subject-dn subject)
    (serial-number serial-number)
    (not-before (x509-validity-not-before validity))
    (not-after (x509-validity-not-after validity))
    (public-key public-key))
   oid private-key))
)
