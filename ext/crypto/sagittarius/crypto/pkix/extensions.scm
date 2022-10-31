;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/extensions.scm - X.509 certificate / CRL extensions
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
(library (sagittarius crypto pkix extensions)
    (export x509-extension? <x509-extension>
	    x509-extension-id
	    x509-extension-critical?
	    x509-extension-value

	    *extension:authority-key-identifier*
	    *extension:subject-key-identifier*
	    *extension:key-usage*
	    *extension:private-key-usage-period*
	    *extension:certificate-policies*
	    *extension:policy-mappings*
	    *extension:subject-alt-name*
	    *extension:issuer-alt-name*
	    *extension:subject-directory-attributes*
	    *extension:basic-constraints*
	    *extension:name-constraints*
	    *extension:policy-constraints*
	    *extension:ext-key-usage*
	    *extension:crl-distribution-points*
	    *extension:inhibit-any-policy*
	    *extension:freshest-crl*
	    *extension:authority-info-access*
	    *extension:subject-info-access*

	    *extension:crl-number*
	    *extension:delta-crl-indicator*
	    *extension:issuing-distribution-point*
	    
	    *extension:crl-reason*
	    *extension:certificate-issuer*
	    *extension:hold-instruction-code*
	    *extension:invalidity-date*

	    x509-extensions? <x509-extensions>
	    x509-extensions->list
	    list->x509-extensions
	    x509-extensions

	    find-x509-extension
	    x509-extension-by-id
	    
	    ;; these 2 are only internal use
	    extensions->x509-extensions
	    x509-extensions->extensions
	    
	    x509-general-name? <x509-general-name>
	    rfc822-name->general-name
	    dns-name->general-name
	    ip-address->general-name
	    uri->general-name
	    directory-name->general-name
	    registered-id->general-name

	    x509-general-names? <x509-general-names>
	    list->x509-general-names
	    x509-general-names
	    ;; these are only for testing
	    x509-general-name->general-name
	    x509-general-names->general-names

	    ;; altName
	    make-x509-subject-alternative-name-extension
	    x509-subject-alternative-name-extension->x509-general-names
	    make-x509-issuer-alternative-name-extension
	    x509-issuer-alternative-name-extension->x509-general-names

	    ;; authorityKeyIdentifier
	    x509-authority-key-identifier? <x509-authority-key-identifier>
	    make-x509-authority-key-identifier
	    x509-authority-key-identifier-key-identifier
	    x509-authority-key-identifier-authority-cert-issuer
	    x509-authority-key-identifier-authority-cert-serial-number

	    make-x509-authority-key-identifier-extension
	    x509-authority-key-identifier-extension->x509-authority-key-identifier
	    ;; for testing
	    x509-authority-key-identifier->authority-key-identifier
	    
	    make-x509-subject-key-identifier-extension
	    x509-subject-key-identifier-extension->subject-key-identifier
	    
	    x509-key-usages x509-key-usages?
	    make-x509-key-usage-extension
	    x509-key-usage-extension->x509-key-usages

	    x509-private-key-usage-period? <x509-private-key-usage-period>
	    make-x509-private-key-usage-period
	    x509-private-key-usage-period-not-before
	    x509-private-key-usage-period-not-after
	    
	    make-x509-private-key-usage-period-extension
	    x509-private-key-usage-period-extension->x509-private-key-usage-period
	    ;; certificate policies
	    x509-policy-qualifier-info? <x509-policy-qualifier-info>
	    x509-policy-qualifier-info-id
	    x509-policy-qualifier-info-qualifier
	    make-x509-policy-qualifier-info
	    x509-policy-qualifier-info->policy-qualifier-info
	    x509-policy-qualifier->asn1-encodable ;; just in case?
	    asn1-encodable->x509-policy-qualifier

	    x509-notice-reference? <x509-notice-reference>
	    x509-notice-reference-organization
	    x509-notice-reference-notice-numbers
	    make-x509-notice-reference

	    x509-user-notice? <x509-user-notice>
	    x509-user-notice-ref
	    x509-user-notice-explicit-text
	    make-x509-user-notice

	    x509-policy-information? <x509-policy-information>
	    x509-policy-information-identifier
	    x509-policy-information-qualifiers
	    make-x509-policy-information
	    *policy-qualifier-type:cps*
	    *policy-qualifier-type:unotice*
	    
	    make-x509-certificate-policies-extension
	    x509-certificate-policies-extension->x509-policy-informations
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix dn)
	    (sagittarius crypto pkix extensions alt-names)
	    (sagittarius crypto pkix extensions cps)
	    (sagittarius crypto pkix extensions key-usage)
	    (sagittarius mop immutable)
	    (sagittarius mop allocation)
	    (sagittarius combinators)
	    (srfi :19 time))

;; useful utility :)
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
(define ((list-of pred) list) (for-all pred list))
(define (oid? o) (or (der-object-identifier? o) (object-identifier-string? o)))

(define (x509-extension-source o) (slot-ref o 'extension))
(define-class <x509-extension> (<immutable> <cached-allocation>)
  ((extension :init-keyword :extension)
   (id :allocation :virtual :slot-ref (make-slot-ref
				       (.$ extension-id x509-extension-source)
				       der-object-identifier->oid-string)
       :reader x509-extension-id)
   (critical? :allocation :virtual :cached #t
	      :slot-ref (make-slot-ref
			 (.$ extension-critical x509-extension-source)
			 (lambda (o) (and o (der-boolean->boolean o))))
	      :reader x509-extension-critical?)
   (value :allocation :virtual :cached #t
	  :slot-ref (make-slot-ref
		     (.$ extension-value x509-extension-source)
		     (.$ bytevector->asn1-object der-octet-string->bytevector))
	  :reader x509-extension-value)))
(define (x509-extension? o) (is-a? o <x509-extension>))
(define (x509-extension-of (oid/string oid?))
  (let ((oid (if (der-object-identifier? oid/string)
		 (der-object-identifier->oid-string oid/string)
		 oid/string)))
    (lambda (extension)
      (equal? oid (x509-extension-id extension)))))

(define (extension->x509-extension (e extension?))
  (make <x509-extension> :extension e))

(define (list->x509-extension-list l)
  (map extension->x509-extension l))
(define-method write-object ((o <x509-extension>) p)
  (format p "#<x509-extension id=~a critical=~a value=~a>"
	  (x509-extension-id o)
	  (x509-extension-critical? o)
	  (x509-extension-value o)))

(define (make-x509-extension (id der-object-identifier?) (critical boolean?)
			     (value asn1-encodable?))
  (let ((bv (asn1-encodable->bytevector value)))
    (make <x509-extension>
      :extension (make <extension>
		   :id id
		   :critical (boolean->der-boolean critical)
		   :value (bytevector->der-octet-string bv)))))

(define (make-x509-subject-alternative-name-extension
	 (general-names x509-general-names?)
	 :optional (critical? #f))
  (make-x509-extension *extension:subject-alt-name* critical?
		       (x509-general-names->general-names general-names)))

(define (x509-subject-alternative-name-extension->x509-general-names
	 (x509-extension (and x509-extension?
			      (x509-extension-of *extension:subject-alt-name*))))
  (let ((v (x509-extension-value x509-extension)))
    (general-names->x509-general-names
     (asn1-object->asn1-encodable <general-names> v))))

(define (make-x509-issuer-alternative-name-extension
	 (general-names x509-general-names?)
	 :optional (critical? #f))
  (make-x509-extension *extension:issuer-alt-name* critical?
		       (x509-general-names->general-names general-names)))
(define (x509-issuer-alternative-name-extension->x509-general-names
	 (x509-extension (and x509-extension?
			      (x509-extension-of *extension:issuer-alt-name*))))
  (let ((v (x509-extension-value x509-extension)))
    (general-names->x509-general-names
     (asn1-object->asn1-encodable <general-names> v))))


(define (x509-authority-key-identifier-extension->x509-authority-key-identifier
	 (x509-extension (and x509-extension?
			      (x509-extension-of *extension:authority-key-identifier*))))
  (authority-key-identifier->x509-authority-key-identifier
   (asn1-object->asn1-encodable <authority-key-identifier> (x509-extension-value x509-extension))))

(define (make-x509-authority-key-identifier-extension
	 (authority-key-identifier x509-authority-key-identifier?)
	 :optional (critical? #f))
  (make-x509-extension *extension:authority-key-identifier* critical?
		       (x509-authority-key-identifier->authority-key-identifier
			authority-key-identifier)))

(define (make-x509-subject-key-identifier-extension
	 (key-identifier bytevector?)
	 :optional (critical? #f))
  (make-x509-extension *extension:subject-key-identifier* critical?
		       (bytevector->der-octet-string key-identifier)))
(define (x509-subject-key-identifier-extension->subject-key-identifier
	 (x509-extension (and x509-extension?
			      (x509-extension-of *extension:subject-key-identifier*))))
  (der-octet-string->bytevector (x509-extension-value x509-extension)))


(define (x509-key-usage-extension->x509-key-usages
	 (x509-extension (and x509-extension?
			      (x509-extension-of *extension:key-usage*))))
  (let ((bits (bytevector->uinteger
	       (der-bit-string->bytevector
		(x509-extension-value x509-extension)))))
    (bits->x509-key-usages bits)))

(define (make-x509-key-usage-extension (key-usages x509-key-usages?)
				       :optional (critical? #f))
  (make-x509-extension *extension:key-usage* critical?
		       (bytevector->der-bit-string
			(integer->bytevector
			 (x509-key-usages->bits key-usages)))))

;; Private key usage period
(define (make-x509-private-key-usage-period-extension
	 (private-key-usage-period x509-private-key-usage-period?)
	 :optional (critical? #f))
  (make-x509-extension *extension:private-key-usage-period* critical?
   (x509-private-key-usage-period->private-key-usage-period
    private-key-usage-period)))
(define (x509-private-key-usage-period-extension->x509-private-key-usage-period
	 (x509-extension (and x509-extension? (x509-extension-of *extension:private-key-usage-period*))))
  (define (maybe v conv) (and v (conv v)))
  (let ((p (asn1-object->asn1-encodable <private-key-usage-period>
					(x509-extension-value x509-extension))))
    (make-x509-private-key-usage-period
     :not-before (maybe (private-key-usage-period-not-before p)
			asn1-time->date)
     :not-after (maybe (private-key-usage-period-not-after p)
		       asn1-time->date))))

;; Certificate Policies
(define (make-x509-certificate-policies-extension (critical? boolean?)
	 . policy-informations)
  (unless (for-all x509-policy-information? policy-informations)
    (assertion-violation 'make-x509-certificate-policies-extension
			 "<x509-policy-information> is required"
			 policy-informations))
  (make-x509-extension *extension:certificate-policies* critical?
		       (make-der-sequence
			(map x509-policy-information->policy-information
			     policy-informations))))
(define (x509-certificate-policies-extension->x509-policy-informations
	 (x509-extension (and x509-extension?
			      (x509-extension-of *extension:certificate-policies*))))
  (let ((v (x509-extension-value x509-extension)))
    (map policy-information->x509-policy-infomation
	 (map (lambda (o)
		(asn1-object->asn1-encodable <policy-information> o))
	      (asn1-collection->list v)))))

;; X509 extensions
(define (x509-extensions->extensions o) (slot-ref o 'extensions))
(define-class <x509-extensions> (<immutable> <cached-allocation>)
  ((extensions :init-keyword :extensions)
   (elements :allocation :virtual :cached #t
	     :slot-ref (make-slot-ref
			(.$ extensions->list x509-extensions->extensions)
			list->x509-extension-list)
	     :reader x509-extensions->list)))
(define (x509-extensions? o) (is-a? o <x509-extensions>))

(define (list->x509-extensions (extensions (list-of x509-extension?)))
  (make <x509-extensions>
    :extensions (make <extensions>
		  :elements (map x509-extension-source extensions))))
(define (x509-extensions . extensions) (list->x509-extensions extensions))

(define (find-x509-extension (pred procedure?)
			     (x509-extensions x509-extensions?))
  (find pred (x509-extensions->list x509-extensions)))
(define x509-extension-by-id x509-extension-of)

;; internal
(define (extensions->x509-extensions (extensions extensions?))
  (make <x509-extensions> :extensions extensions))

)
    
