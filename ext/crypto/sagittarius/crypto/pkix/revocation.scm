;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/revocation.scm - X.509 Certificate Revocation List
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
(library (sagittarius crypto pkix revocation)
    (export x509-revoked-certificate? <x509-revoked-certificate>
	    make-x509-revoked-certificate
	    x509-revoked-certificate-serial-number
	    x509-revoked-certificate-revocation-date
	    x509-revoked-certificate-crl-entry-extensions

	    x509-certificate-revocation-list? <x509-certificate-revocation-list>
	    x509-certificate-revocation-list-issuer
	    x509-certificate-revocation-list-this-update
	    x509-certificate-revocation-list-next-update
	    x509-certificate-revocation-list-revoked-certificates
	    x509-certificate-revocation-list-crl-extensions
	    x509-certificate-revocation-list-signature-algorithm
	    x509-certificate-revocation-list-signature
	    asn1-object->x509-certificate-revocation-list
	    read-x509-certificate-revocation-list
	    bytevector->x509-certificate-revocation-list
	    x509-certificate-revocation-list->asn1-object
	    x509-certificate-revocation-list->bytevector
	    write-x509-certificate-revocation-list

	    validate-x509-certificate-revocation-list
	    x509-certificate-revocation-list-signature-validator
	    x509-certificate-revocation-list-issuer-validator
	    x509-certificate-revoked?

	    x509-certificate-revocation-list-template?
	    x509-certificate-revocation-list-template-builder
	    sign-x509-certificate-revocation-list-template
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix dn)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix certificate)
	    (sagittarius crypto pkix signatures)
	    (sagittarius crypto pkix extensions)
	    (sagittarius crypto keys)
	    (sagittarius crypto signatures)
	    (sagittarius combinators)
	    (sagittarius mop allocation)
	    (sagittarius mop immutable)
	    (util bytevector)
	    (record builder)
	    (srfi :19 time))
;; useful utility :)
(define ((list-of pred) l) (for-all pred l))
(define (oid? o) (or (der-object-identifier? o) (object-identifier-string? o)))

(define (x509-revoked-certificate-c o) (slot-ref o 'c))
(define-class <x509-revoked-certificate> (<immutable> <cached-allocation>)
  ((c :init-keyword :c)
   (serial-number :allocation :virtual :cached #t
		  :slot-ref ($. x509-revoked-certificate-c
				revoked-certificate-user-certificate
				der-integer->integer)
		  :reader x509-revoked-certificate-serial-number)
   (revocation-date :allocation :virtual :cached #t
		    :slot-ref ($. x509-revoked-certificate-c
				  revoked-certificate-revocation-date
				  asn1-time->date)
		    :reader x509-revoked-certificate-revocation-date)
   (crl-entry-extensions :allocation :virtual :cached #t
    :slot-ref ($. x509-revoked-certificate-c
		  revoked-certificate-crl-entry-extensions
		  (lambda (e)
		    (and e (extensions->x509-extension-list e))))
    :reader x509-revoked-certificate-crl-entry-extensions)))
(define (x509-revoked-certificate? o) (is-a? o <x509-revoked-certificate>))
(define (make-x509-revoked-certificate
	 (serial-number integer?)
	 (revocation-date date?)
	 :optional ((extensions (or #f (list-of x509-extension?))) #f))
  (make <x509-revoked-certificate>
    :c (make-revoked-certificate serial-number revocation-date extensions)))

(define (revoked-certificate->x509-revoked-certificate
	 (revoked-certificate revoked-certificate?))
  (make <x509-revoked-certificate> :c revoked-certificate))
(define (x509-revoked-certificate->revoked-certificate
	 (revoked-certificate x509-revoked-certificate?))
  (define rc revoked-certificate)
  (let ((serial-number (x509-revoked-certificate-serial-number rc))
	(revocation-date (x509-revoked-certificate-revocation-date rc))
	(e* (x509-revoked-certificate-crl-entry-extensions rc)))
    (make-revoked-certificate serial-number revocation-date e*)))
(define (make-revoked-certificate sn date e*)
  (make <revoked-certificate>
    :user-certificate (integer->der-integer sn)
    :revocation-date (date->der-generalized-time date)
    :crl-entry-extensions (and e* (x509-extension-list->extensions e*))))

(define (x509-certificate-revocation-list-c o) (slot-ref o 'c))
(define tbs-cert-list (.$ certificate-list-c
			  x509-certificate-revocation-list-c))
(define-class <x509-certificate-revocation-list> (<x509-signed-object>)
  ((encoded :init-keyword :encoded :init-value #f :mutable #t
	    :reader x509-certificate-revocation-list-encoded
	    :writer x509-certificate-revocation-list-encoded-set!)
   (issuer :allocation :virtual :cached #t
	   :slot-ref ($. tbs-cert-list tbs-cert-list-issuer name->x509-name)
	   :reader x509-certificate-revocation-list-issuer)
   (this-update :allocation :virtual :cached #t
		:slot-ref ($. tbs-cert-list tbs-cert-list-this-update
			      asn1-time->date)
		:reader x509-certificate-revocation-list-this-update)
   (next-update :allocation :virtual :cached #t
		:slot-ref ($. tbs-cert-list tbs-cert-list-next-update
			      (lambda (e) (and e (asn1-time->date e))))
		:reader x509-certificate-revocation-list-next-update)
   (revoked-certificates :allocation :virtual :cached #t
    :slot-ref ($. tbs-cert-list tbs-cert-list-revoked-certificates
		  (lambda (l)
		    (map revoked-certificate->x509-revoked-certificate l)))
    :reader x509-certificate-revocation-list-revoked-certificates)
   (crl-extensions :allocation :virtual :cached #t
    :slot-ref ($. tbs-cert-list tbs-cert-list-crl-extensions
		  (lambda (e) (and e (extensions->x509-extension-list e))))
    :reader x509-certificate-revocation-list-crl-extensions)))
(define (x509-certificate-revocation-list? o)
  (is-a? o <x509-certificate-revocation-list>))
(define (x509-certificate-revocation-list-signature-algorithm
	 (crl x509-certificate-revocation-list?))
  (x509-signed-object-algorithm crl))
(define (x509-certificate-revocation-list-signature
	 (crl x509-certificate-revocation-list?))
  (x509-signed-object-signature crl))

(define (asn1-object->x509-certificate-revocation-list
	 (asn1-object asn1-object?))
  (make <x509-certificate-revocation-list>
    :c (asn1-object->asn1-encodable <certificate-list> asn1-object)))

(define (read-x509-certificate-revocation-list
	 (input (and input-port? binary-port?)))
  (asn1-object->x509-certificate-revocation-list (read-asn1-object input)))

(define (bytevector->x509-certificate-revocation-list (bv bytevector?))
  (let ((r (read-x509-certificate-revocation-list
	    (open-bytevector-input-port bv))))
    (x509-certificate-revocation-list-encoded-set! r bv)
    r))

(define (x509-certificate-revocation-list->asn1-object
	 (crl x509-certificate-revocation-list?))
  (asn1-encodable->asn1-object (x509-certificate-revocation-list-c crl)))

(define (x509-certificate-revocation-list->bytevector
	 (crl x509-certificate-revocation-list?))
  (cond ((x509-certificate-revocation-list-encoded crl))
	(else 
	 (let ((bv (asn1-encodable->bytevector
		    (x509-certificate-revocation-list->asn1-object crl))))
	   (x509-certificate-revocation-list-encoded-set! crl bv)
	   bv))))
(define (write-x509-certificate-revocation-list
	 (crl x509-certificate-revocation-list?)
	 :optional (out (current-output-port)))
  (let ((bv (x509-certificate-revocation-list->bytevector crl)))
    (put-bytevector out bv)))

(define (validate-x509-certificate-revocation-list
	 (crl x509-certificate-revocation-list?)
	 . validators)
  (for-all (lambda (v) (v crl)) validators))

(define ((x509-certificate-revocation-list-signature-validator
	  (public-key public-key?))
	 (crl x509-certificate-revocation-list?))
  (unless (verify-x509-signed-object crl public-key)
    (assertion-violation 'x509-certificate-revocation-list-signature-validator
			 "Invalid signature" crl)))
(define ((x509-certificate-revocation-list-issuer-validator (issuer x509-name?))
	 (crl x509-certificate-revocation-list?))
  (unless (equal? (x509-certificate-revocation-list-issuer crl) issuer)
    (assertion-violation 'x509-certificate-revocation-list-issuer-validator
			 "Invalid issuer" crl)))

(define (x509-certificate-revoked? (certificate x509-certificate?)
				   (crl x509-certificate-revocation-list?)
				   :optional (date (current-date)))
  (define (in-effect? revoked)
    (time<? (date->time-utc (x509-revoked-certificate-revocation-date revoked))
	    (date->time-utc date)))
  (define ((cert=? sn) revoked)
    (and (= (x509-revoked-certificate-serial-number revoked) sn)
	 (in-effect? revoked)))
       
  (and (equal? (x509-certificate-revocation-list-issuer crl)
	       (x509-certificate-issuer-dn certificate))
       (exists (cert=? (x509-certificate-serial-number certificate))
	       (x509-certificate-revocation-list-revoked-certificates crl))))

(define-record-type x509-certificate-revocation-list-template
  (fields issuer-dn
	  this-update
	  next-update
	  revoked-certificates
	  crl-extensions))

(define ((required who pred conv) o)
  (unless (pred o)
    (assertion-violation (list 'x509-certificate-revocation-list-template who)
			 "Invalid value"
			 o `(must satisfy ,pred)))
  (conv o))
(define ((optional who pred conv) o)
  (unless (or (not o) (pred o))
    (assertion-violation (list 'x509-certificate-revocation-list-template who)
			 "Invalid value"
			 o `(must satisfy ,pred)))
  (and o (conv o)))
(define check-issuer (required 'issuer-dn x509-name? x509-name->name))
(define check-this-update
  (required 'this-update date? date->der-generalized-time))
(define check-next-update
  (optional 'next-update date? date->der-generalized-time))
(define check-revoked-certificates
  (required 'revoked-certificates (list-of x509-revoked-certificate?)
	    (lambda (l)
	      (map x509-revoked-certificate->revoked-certificate l))))
(define check-extensions
  (optional 'crl-extensions (list-of x509-extension?)
	    x509-extension-list->extensions))
(define-syntax x509-certificate-revocation-list-template-builder
  (make-record-builder x509-certificate-revocation-list-template
   ((issuer-dn #f check-issuer)
    (this-update #f check-this-update)
    (next-update #f check-next-update)
    (revoked-certificates '() check-revoked-certificates)
    (crl-extensions #f check-extensions))))

(define (x509-certificate-revocation-list-template->tbs-cert-list tmpl algo)
  (define issuer (x509-certificate-revocation-list-template-issuer-dn tmpl))
  (define tupd (x509-certificate-revocation-list-template-this-update tmpl))
  (define nupd (x509-certificate-revocation-list-template-next-update tmpl))
  (define rc*
    (x509-certificate-revocation-list-template-revoked-certificates tmpl))
  (define extensions
    (x509-certificate-revocation-list-template-crl-extensions tmpl))
  (define version
    (and (or extensions (exists revoked-certificate-crl-entry-extensions rc*))
	 ;; v2
	 (integer->der-integer 1)))
  (make <tbs-cert-list>
    :version version
    :signature algo
    :issuer issuer
    :this-update tupd
    :next-update nupd
    :revoked-certificates rc*
    :crl-extensions extensions))

(define (sign-x509-certificate-revocation-list-template
	 (template x509-certificate-revocation-list-template?)
	 (aid (or string? x509-algorithm-identifier?))
	 (private-key private-key?))
  (define x509-aid (if (string? aid)
		       (make-x509-algorithm-identifier aid)
		       aid))
  (define signer (x509-algorithm-identifier->signer x509-aid private-key))
  (let* ((algorithm (x509-algorithm-identifier->algorithm-identifier x509-aid))
	 (tbs-list (x509-certificate-revocation-list-template->tbs-cert-list
		    template algorithm))
	 (signature (signer-sign-message signer
		     (asn1-encodable->bytevector tbs-list)))
	 (cr (make <certificate-list>
	       :c tbs-list
	       :algorithm algorithm
	       :signature (bytevector->der-bit-string signature))))
    (make <x509-certificate-revocation-list> :c cr)))

)
    
