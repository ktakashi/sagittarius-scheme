;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/certificate.scm - X.509 certificate
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
(library (sagittarius crypto pkix certificate)
    (export x509-name? <x509-name> x509-name
	    x509-name->string
	    list->x509-name

	    x509-validity? <x509-validity>
	    make-x509-validity
	    x509-validity-not-before
	    x509-validity-not-after
	    
	    x509-certificate? <x509-certificate>
	    bytevector->x509-certificate read-x509-certificate
	    asn1-object->x509-certificate
	    
	    x509-certificate->bytevector write-x509-certificate
	    x509-certificate->asn1-object

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
	    
	    x509-certificate-template?
	    x509-certificate-template-builder
	    sign-x509-certificate-template
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix dn)
	    (sagittarius crypto pkix keys)
	    (sagittarius crypto pkix signatures)
	    (sagittarius crypto pkix extensions)
	    (sagittarius crypto keys)
	    (sagittarius crypto signatures)
	    (sagittarius combinators)
	    (sagittarius mop allocation)
	    (sagittarius mop immutable)
	    (srfi :19 time)
	    (util bytevector)
	    (record builder))

;; useful utility :)
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
(define ((list-of pred) list) (for-all pred list))

(define-class <x509-validity> (<immutable> <cached-allocation>)
  ((validity :init-keyword :validity)
   (not-before :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ validity-not-before (lambda (o) (slot-ref o 'validity)))
	       asn1-time->date)
    :reader x509-validity-not-before)
   (not-after :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ validity-not-after (lambda (o) (slot-ref o 'validity)))
	       asn1-time->date)
    :reader x509-validity-not-after)))
(define (x509-validity? o) (is-a? o <x509-validity>))
(define (validity->x509-validity (validity validity?))
  (make <x509-validity> :validity validity))
(define (make-x509-validity (not-before date?) (not-after date?))
  (make <x509-validity>
    :validity (make <validity>
		:not-before (date->der-generalized-time not-before)
		:not-after (date->der-generalized-time not-after))))

(define (x509-certificate? o) (is-a? o <x509-certificate>))
(define (x509-certificate-c (o x509-certificate?)) (slot-ref o 'c))
(define tbs-cert (.$ certificate-c x509-certificate-c))
(define-class <x509-certificate> (<cached-allocation> <immutable>)
  ((c :init-keyword :c)
   (encoded :init-keyword :encoded :init-value #f :mutable #t
	    :reader x509-certificate-encoded
	    :writer x509-certificate-encoded-set!)
   (issuer-dn :allocation :virtual :cached #t
    :slot-ref (make-slot-ref 
	       (.$ tbs-certificate-issuer tbs-cert)
	       name->x509-name)
    :reader x509-certificate-issuer-dn)
   (subject-dn :allocation :virtual :cached #t
    :slot-ref (make-slot-ref 
	       (.$ tbs-certificate-subject tbs-cert)
	       name->x509-name)
    :reader x509-certificate-subject-dn)
   (public-key :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-subject-public-key-info tbs-cert)
	       subject-public-key-info->public-key)
    :reader x509-certificate-public-key)
   (validity :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-validity tbs-cert)
	       validity->x509-validity)
    :reader x509-certificate-validity)
   (serial-number :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-serial-number tbs-cert)
	       der-integer->integer)
    :reader x509-certificate-serial-number)
   (version :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-version tbs-cert)
	       (lambda (v) (+ (or (and v (der-integer->integer v)) 0) 1)))
    :reader x509-certificate-version)
   (signature :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ certificate-signature x509-certificate-c)
	       der-bit-string->bytevector)
    :reader x509-certificate-signature)
   (signature-algorithm :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ algorithm-identifier-algorithm certificate-algorithm x509-certificate-c)
	       der-object-identifier->oid-string)
    :reader x509-certificate-signature-algorithm)
   (raw-signature-algorithm :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ certificate-algorithm x509-certificate-c)
	       values)
    :reader x509-certificate-raw-signature-algorithm)
   (extensions :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ tbs-certificate-extensions tbs-cert)
	       (lambda (e)
		 (or (and e (extensions->x509-extension-list e)) '())))
    :reader x509-certificate-extensions)))

(define-method write-object ((o <x509-certificate>) p)
  (define signature-hex
    (bytevector->hex-string (x509-certificate-signature o)))
  (define extensions (x509-certificate-extensions o))
  (let-values (((out e) (open-string-output-port)))
    (format out "#<x509-certificate~%")
    (format out "  [0]          Version: ~a~%" (x509-certificate-version o))
    (format out "          SerialNumber: ~a~%" (x509-certificate-serial-number o))
    (format out "              IssuerDN: ~a~%" (x509-name->string (x509-certificate-issuer-dn o)))
    (format out "            Start Date: ~a~%" (x509-validity-not-before (x509-certificate-validity o)))
    (format out "            Final Date: ~a~%" (x509-validity-not-after (x509-certificate-validity o)))
    (format out "             SubjectDN: ~a~%" (x509-name->string (x509-certificate-subject-dn o)))
    (format out "            Public Key: ~a~%" (x509-certificate-public-key o))
    (format out "   Signature Algorithm: ~a~%" (x509-certificate-signature-algorithm o))
    (let ((p (algorithm-identifier-parameters
	      (x509-certificate-raw-signature-algorithm o))))
      (when (and p (not (der-null? p)))
	(format out "  Signature Parameters: ~a~%" p)))
    (cond ((< (string-length signature-hex) 40)
	   (format out "             Signature: ~a~%" signature-hex))
	  (else
	   (format out "             Signature: ~a~%" (substring signature-hex 0 40))
	   (do ((i 40 (+ i 40)) (len (string-length signature-hex))) ((>= i len))
	     (if (< i (- len 40))
		 (format out "                        ~a~%" (substring signature-hex i (+ i 40)))
		 (format out "                        ~a~%" (substring signature-hex i len))))))
    (unless (null? extensions)
      (format out "            Extensions: ~%")
      (for-each (lambda (e) (format out "              ~a~%" e)) extensions))
    (display ">" out)
    (display (e) p)))

(define (bytevector->x509-certificate (bv bytevector?))
  (make <x509-certificate>
    :c (asn1-object->asn1-encodable <certificate>
				    (bytevector->asn1-object bv))
    :encoded bv))
(define (read-x509-certificate (port (and input-port? binary-port?)))
  (asn1-object->x509-certificate (read-asn1-object port)))

(define (asn1-object->x509-certificate (asn1-object asn1-object?))
  (make <x509-certificate>
    :c (asn1-object->asn1-encodable <certificate> asn1-object)))

(define (x509-certificate->bytevector (x509-certificate x509-certificate?))
  (cond ((x509-certificate-encoded x509-certificate))
	(else
	 (let ((r (asn1-encodable->bytevector
		   (x509-certificate-c x509-certificate))))
	   (x509-certificate-encoded-set! x509-certificate r)
	   r))))

(define (write-x509-certificate (x509-certificate x509-certificate?)
				:optional (out (current-output-port)))
  (put-bytevector out (x509-certificate->bytevector x509-certificate)))

(define (x509-certificate->asn1-object (x509-certificate x509-certificate?))
  (bytevector->asn1-object (x509-certificate->bytevector x509-certificate)))

(define (x509-certificate-expired? (x509-certificate x509-certificate?)
				   :optional (when (current-date)))
  (let* ((validity (x509-certificate-validity x509-certificate))
	 (utc (date->time-utc when))
	 (not-after (date->time-utc (x509-validity-not-after validity))))
    (time<? not-after utc)))

(define (validate-x509-certificate (x509-certificate x509-certificate?)
				   . validators)
  (for-all (lambda (v) (v x509-certificate)) validators))

(define ((x509-certificate-signature-validator (public-key public-key?))
	 (x509-certificate x509-certificate?))
  (let-values (((oid parameters)
		(signature-algorithm->oid&parameters
		 (x509-certificate-raw-signature-algorithm x509-certificate))))
    (let ((verifier (apply (oid->verifier-maker oid) public-key
			   :der-encode #f
			   (signature-parameters->keyword-parameters parameters))))
      (unless (verifier-verify-signature verifier
		(asn1-encodable->bytevector (tbs-cert x509-certificate))
		(x509-certificate-signature x509-certificate))
	(assertion-violation 'x509-certificate-signature
			     "Invalid signature" x509-certificate)))))
(define ((x509-certificate-validity-validator :optional (when (current-date)))
	 (x509-certificate x509-certificate?))
  (let* ((validity (x509-certificate-validity x509-certificate))
	 (utc (date->time-utc when))
	 (not-before (date->time-utc (x509-validity-not-before validity)))
	 (not-after (date->time-utc (x509-validity-not-after validity))))
    (unless (and (time<=? not-before utc) (time<=? utc not-after))
      (assertion-violation 'x509-certificate-validity
			   "Certificate is either expired or not in effect"
			   x509-certificate))))

(define-record-type x509-certificate-template
  (fields issuer-dn
	  subject-dn
	  serial-number
	  not-before
	  not-after
	  public-key
	  issuer-unique-id
	  subject-unique-id
	  extensions))
(define ((required who pred conv) o)
  (unless (pred o)
    (assertion-violation (list 'x509-certificate-template who)
			 "Invalid value"
			 o `(must satisfy ,pred)))
  (conv o))
(define ((optional who pred conv) o)
  (unless (or (not o) (pred o))
    (assertion-violation (list 'x509-certificate-template who)
			 "Invalid value"
			 o `(must satisfy ,pred)))
  (and o (conv o)))

(define check-issuer (required 'issuer-dn x509-name? x509-name->name))
(define check-subject (required 'subject-dn x509-name? x509-name->name))
(define check-serial-number
  (required 'serial-number integer? integer->der-integer))
(define check-not-before
  (required 'not-before date? date->der-generalized-time))
(define check-not-after
  (required 'not-after date? date->der-generalized-time))
(define check-public-key
  (required 'public-key public-key? values))
(define check-issuer-unique-id
  (optional 'issuer-unique-id bytevector? bytevector->der-bit-string))
(define check-subject-unique-id
  (optional 'subject-unique-id bytevector? bytevector->der-bit-string))
(define check-extensions
  (optional 'extensions (list-of x509-extension?)
	    x509-extension-list->extensions))

(define-syntax x509-certificate-template-builder
  (make-record-builder x509-certificate-template
    ((issuer-dn #f check-issuer)
     (subject-dn #f check-subject)
     (serial-number #f check-serial-number)
     (not-before #f check-not-before)
     (not-after #f check-not-after)
     (public-key #f check-public-key)
     (issuer-unique-id #f check-issuer-unique-id)
     (subject-unique-id #f check-subject-unique-id)
     (extensions #f check-extensions))))

(define (x509-certificate-template->tbs-certificate template
						    signature-algorithm)
  (define (version template)
    (cond ((x509-certificate-template-extensions template)
	   (integer->der-integer 2)) ;; v3
	  ((or (x509-certificate-template-issuer-unique-id template)
	       (x509-certificate-template-subject-unique-id template))
	   (integer->der-integer 1)) ;; v2
	  (else #f)))		     ;; v1 (we don't set)
  (define public-key (x509-certificate-template-public-key template))
  (make <tbs-certificate>
    :version (version template)
    :serial-number (x509-certificate-template-serial-number template)
    :signature signature-algorithm
    :issuer (x509-certificate-template-issuer-dn template)
    :validity (make <validity>
		:not-before (x509-certificate-template-not-before template)
		:not-after (x509-certificate-template-not-after template))
    :subject-public-key-info (public-key->subject-public-key-info public-key)
    :subject (x509-certificate-template-subject-dn template)
    :issuer-unique-id (x509-certificate-template-issuer-unique-id template)
    :subject-unique-id (x509-certificate-template-subject-unique-id template)
    :extensions (x509-certificate-template-extensions template)))

(define (sign-x509-certificate-template
	 (template x509-certificate-template?)
	 oid
	 (private-key private-key?)
	 :optional ((parameters (or #f signature-parameters?)) #f))

  (define signer (apply (oid->signer-maker oid) private-key
			:der-encode #f
			(signature-parameters->keyword-parameters parameters)))
  (let* ((algorithm (make <algorithm-identifier>
		      :algorithm (oid-string->der-object-identifier oid)
		      :parameters (signature-parameters->algorithm-parameters parameters)))
	 (tbs-certificate (x509-certificate-template->tbs-certificate
			   template algorithm))
	 (signature (signer-sign-message signer
		     (asn1-encodable->bytevector tbs-certificate)))
	 (certificate (make <certificate>
			:c tbs-certificate
			:algorithm algorithm
			:signature (bytevector->der-bit-string signature))))
    (make <x509-certificate> :c certificate)))
)
