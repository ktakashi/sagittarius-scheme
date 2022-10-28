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
	    make-x509-issuer-alternative-name-extension

	    ;; authorityKeyIdentifier
	    x509-authority-key-identifier? <x509-authority-key-identifier>
	    make-x509-authority-key-identifier
	    x509-authority-key-identifier-key-identifier
	    x509-authority-key-identifier-authority-cert-issuer
	    x509-authority-key-identifier-authority-cert-serial-number

	    make-x509-authority-key-identifier-extension
	    ;; for testing
	    x509-authority-key-identifier->authority-key-identifier
	    
	    make-x509-subject-key-identifier-extension
	    
	    x509-key-usages x509-key-usages?
	    make-x509-key-usage-extension
	    x509-key-usage-extension->x509-key-usages
	    
	    )
    (import (rnrs)
	    (clos user)
	    (core enums) ;; for enum-set?...
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius mop immutable)
	    (sagittarius mop allocation)
	    (sagittarius combinators))

;; useful utility :)
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))

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

(define-class <x509-general-name> (<immutable>)
  ((type :init-keyword :type :reader x509-general-name-type)
   (value :init-keyword :value :reader x509-general-name-value)))
(define (x509-general-name? o) (is-a? o <x509-general-name>))
(define (x509-general-name->general-name (name x509-general-name?))
  (define (get-ctr type)
    (case type
      ((rfc822-name) string->der-ia5-string)
      ((dns-name) string->der-ia5-string)
      ((ip-address) bytevector->der-octet-string)
      ((uniform-resource-identifier) string->der-ia5-string)
      ((directory-name) string->der-utf8-string)
      ((registered-id) oid-string->der-object-identifier)
      (else
       (assertion-violation 'x509-general-name->general-name
			    "Unsupported type" type))))
  (let ((type (x509-general-name-type name)))
    (make <general-name>
      :type type :value ((get-ctr type) (x509-general-name-value name)))))

(define (rfc822-name->general-name (name string?))
  (make <x509-general-name> :type 'rfc822-name :value name))
(define (dns-name->general-name (name string?))
  (make <x509-general-name> :type 'dns-name :value name))
(define (ip-address->general-name (ip-address bytevector?))
  (make <x509-general-name> :type 'ip-address :value ip-address))
(define (uri->general-name (uri string?))
  (make <x509-general-name> :type 'uniform-resource-identifier :value uri))
(define (directory-name->general-name (name string?))
  (make <x509-general-name> :type 'directory-name :value name))
(define (registered-id->general-name (oid object-identifier-string?))
  (make <x509-general-name> :type 'registered-id :value oid))

(define-class <x509-general-names> (<immutable>)
  ((names :init-keyword :names :reader x509-general-names-names)))
(define (x509-general-names? o) (is-a? o <x509-general-names>))
(define (x509-general-names->general-names (general-names x509-general-names?))
  (make <general-names>
    :elements (map x509-general-name->general-name
		   (x509-general-names-names general-names))))
(define ((list-of pred) list) (for-all pred list))
(define list-of-x509-general-names? (list-of x509-general-name?))
(define (list->x509-general-names (names list-of-x509-general-names?))
  (make <x509-general-names> :names names))
(define (x509-general-names . names) (list->x509-general-names names))

(define (make-x509-subject-alternative-name-extension
	 (general-names x509-general-names?)
	 :optional (critical? #f))
  (make-x509-extension *extension:subject-alt-name* critical?
		       (x509-general-names->general-names general-names)))

(define (make-x509-issuer-alternative-name-extension
	 (general-names x509-general-names?)
	 :optional (critical? #f))
  (make-x509-extension *extension:issuer-alt-name* critical?
		       (x509-general-names->general-names general-names)))

(define-class <x509-authority-key-identifier> (<immutable>)
  ((key-identifier :init-keyword :key-identifier :init-value #f
		   :reader x509-authority-key-identifier-key-identifier)
   (authority-cert-issuer :init-keyword :authority-cert-issuer :init-value #f
    :reader x509-authority-key-identifier-authority-cert-issuer)
   (authority-cert-serial-number :init-keyword :authority-cert-serial-number
    :init-value #f
    :reader x509-authority-key-identifier-authority-cert-serial-number)))
(define (x509-authority-key-identifier? o)
  (is-a? o <x509-authority-key-identifier>))
(define (make-x509-authority-key-identifier
	 :key ((key-identifier (or #f bytevector?)) #f)
	      ((authority-cert-issuer (or #f x509-general-names?)) #f)
	      ((authority-cert-serial-number (or #f integer?)) #f))
  (unless (or (and authority-cert-issuer authority-cert-serial-number)
	      (and (not authority-cert-issuer)
		   (not authority-cert-serial-number)))
    (assertion-violation 'make-x509-authority-key-identifier
			 "authority-cert-issuer and authority-cert-serial-number must be either both present or both absent"))
  (make <x509-authority-key-identifier>
    :key-identifier key-identifier
    :authority-cert-issuer authority-cert-issuer
    :authority-cert-serial-number authority-cert-serial-number))

(define (x509-authority-key-identifier->authority-key-identifier key-identifier)
  (define (maybe v conv) (and v (conv v)))
  (make <authority-key-identifier>
    :key-identifier (maybe (x509-authority-key-identifier-key-identifier key-identifier)
			   bytevector->der-octet-string)
    :authority-cert-issuer (x509-authority-key-identifier-authority-cert-issuer key-identifier)
    :authority-cert-serial-number (maybe (x509-authority-key-identifier-authority-cert-serial-number key-identifier)
					 integer->der-integer)))

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

;; KeyUsage ::= BIT STRING {
;;      digitalSignature        (0),
;;      nonRepudiation          (1), --  recent editions of X.509 have
;;                                   --  renamed this bit to
;;                                   --  contentCommitment
;;      keyEncipherment         (2),
;;      dataEncipherment        (3),
;;      keyAgreement            (4),
;;      keyCertSign             (5),
;;      cRLSign                 (6),
;;      encipherOnly            (7),
;;      decipherOnly            (8)
;;  }
(define *x509-key-usage-list*
  '(digital-signature
    non-repudiation    ;;
    content-commitment ;; = non-repudiation
    key-encipherment
    data-encipherment
    key-agreement
    crl-sign
    key-cert-sign
    encipher-only
    decipher-only
    ))
(define x509-key-usage-enumeration (make-enumeration *x509-key-usage-list*))
(define make-x509-key-usages (enum-set-constructor x509-key-usage-enumeration))

(define-syntax x509-key-usages
  (syntax-rules ()
    ((_ symbol ...)
     (make-x509-key-usages '(symbol ...)))))

(define *x509-key-usages* (enum-set-universe (x509-key-usages)))
(define (x509-key-usages? o)
  (and (enum-set? o)
       (enum-set-subset? o *x509-key-usages*)))

(define (key-usage->bit-position key-usage)
  (case key-usage
    ((digital-signature)                  7)
    ((non-repudiation content-commitment) 6)
    ((key-encipherment)                   5)
    ((data-encipherment)                  4)
    ((key-agreement)                      3)
    ((crl-sign)                           2)
    ((key-cert-sign)                      1)
    ((encipher-only)                      0)
    ;; Where does this 15 come from?
    ((decipher-only)                      15)))

(define (key-usage->bit key-usage)
  (bitwise-arithmetic-shift  1(key-usage->bit-position key-usage)))

(define (x509-key-usage-extension->x509-key-usages
	 (x509-extension x509-extension?))
  (unless (equal? (x509-extension-id x509-extension)
		  (der-object-identifier->oid-string *extension:key-usage*))
    (assertion-violation 'x509-key-usage-extension->x509-key-usages
			 "X509 key usage extension is required"
			 x509-extension))
  (let ((bits (bytevector->uinteger
	       (der-bit-string->bytevector
		(x509-extension-value x509-extension)))))
    (let loop ((usages (enum-set->list *x509-key-usages*)) (r '()))
      (cond ((null? usages) (make-x509-key-usages r))
	    ((bitwise-bit-set? bits (key-usage->bit-position (car usages)))
	     (loop (cdr usages) (cons (car usages) r)))
	    (else (loop (cdr usages) r))))))

(define (make-x509-key-usage-extension (key-usages x509-key-usages?)
				       :optional (critical? #f))
  (let loop ((usages (enum-set->list key-usages)) (r 0))
    (if (null? usages)
	(make-x509-extension *extension:key-usage* critical?
			     (bytevector->der-bit-string
			      (integer->bytevector r)))
	(loop (cdr usages)
	      (bitwise-ior r (key-usage->bit (car usages)))))))

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
(define (x509-extension-by-id oid/string)
  (let ((oid (if (der-object-identifier? oid/string)
		 (der-object-identifier->oid-string oid/string)
		 oid/string)))
    (lambda (extension)
      (equal? oid (x509-extension-id extension)))))

;; internal
(define (extensions->x509-extensions (extensions extensions?))
  (make <x509-extensions> :extensions extensions))

)
    
