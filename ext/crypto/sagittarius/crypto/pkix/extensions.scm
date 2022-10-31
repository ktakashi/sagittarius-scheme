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

	    ;; For testing
	    x509-private-key-usage-period->private-key-usage-period
	    
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
	    x509-policy-information->policy-information
	    *policy-qualifier-type:cps*
	    *policy-qualifier-type:unotice*
	    
	    make-x509-certificate-policies-extension
	    x509-certificate-policies-extension->x509-policy-informations
	    )
    (import (rnrs)
	    (clos user)
	    (core enums) ;; for enum-set?...
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix dn)
	    (sagittarius mop immutable)
	    (sagittarius mop allocation)
	    (sagittarius combinators)
	    (srfi :19 time))

;; useful utility :)
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
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
      ((directory-name) x509-name->name)
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
(define (directory-name->general-name (name x509-name?))
  (make <x509-general-name> :type 'directory-name :value name))
(define (registered-id->general-name (oid object-identifier-string?))
  (make <x509-general-name> :type 'registered-id :value oid))
(define (general-name->x509-general-name (general-name general-name?))
  (let ((v (general-name-name general-name)))
    (make <x509-general-name>
      :type (general-name-type general-name)
      :value (cond ((asn1-string? v) (asn1-string->string v))
		   ((der-object-identifier? v) (der-object-identifier->oid-string v))
		   ((der-octet-string? v) (der-octet-string->bytevector v))
		   ;; sorry, not supported yet...
		   (else v)))))
  
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
(define (general-names->x509-names (general-names general-names?))
  (make <x509-general-names>
    :names (map general-name->x509-general-name
		(general-names-components general-names))))

(define (make-x509-subject-alternative-name-extension
	 (general-names x509-general-names?)
	 :optional (critical? #f))
  (make-x509-extension *extension:subject-alt-name* critical?
		       (x509-general-names->general-names general-names)))

(define (x509-subject-alternative-name-extension->x509-general-names
	 (x509-extension (and x509-extension?
			      (x509-extension-of *extension:subject-alt-name*))))
  (let ((v (x509-extension-value x509-extension)))
    (general-names->x509-names
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
    (general-names->x509-names
     (asn1-object->asn1-encodable <general-names> v))))

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
(define (authority-key-identifier->x509-authority-key-identifier key-identifier)
  (define (maybe v conv) (and v (conv v)))
  (make <x509-authority-key-identifier>
    :key-identifier (maybe (authority-key-identifier-key-identifier key-identifier)
			   der-octet-string->bytevector)
    :authority-cert-issuer (maybe (authority-key-identifier-authority-cert-issuer key-identifier)
				  general-names->x509-names)
    :authority-cert-serial-number (maybe (authority-key-identifier-authority-cert-serial-number key-identifier)
					 der-integer->integer)))

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
  (bitwise-arithmetic-shift 1 (key-usage->bit-position key-usage)))

(define (x509-key-usage-extension->x509-key-usages
	 (x509-extension (and x509-extension? (x509-extension-of *extension:key-usage*))))
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

;; Private key usage period
(define-class <x509-private-key-usage-period> (<immutable>)
  ((not-before :init-keyword :not-before
	       :reader x509-private-key-usage-period-not-before)
   (not-after :init-keyword :not-after
	      :reader x509-private-key-usage-period-not-after)))
(define (x509-private-key-usage-period? o)
  (is-a? o <x509-private-key-usage-period>))
(define (make-x509-private-key-usage-period
	 :key ((not-before (or #f date?)) #f) ((not-after (or #f date?)) #f))
  (unless (or not-before not-after)
    (assertion-violation 'make-x509-private-key-usage-period
			 "Either not-before or not-after must be present"))
  (make <x509-private-key-usage-period>
    :not-before not-before :not-after not-after))
(define (x509-private-key-usage-period->private-key-usage-period
	 (private-key-usage-period x509-private-key-usage-period?))
  (let ((not-before
	 (x509-private-key-usage-period-not-before private-key-usage-period))
	(not-after
	 (x509-private-key-usage-period-not-after private-key-usage-period)))
    (make <private-key-usage-period>
      :not-before (and not-before (date->der-generalized-time not-before))
      :not-after (and not-after (date->der-generalized-time not-after)))))

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

;; CertificatePolicies
(define (string->display-text s)
  (make <directory-string>
    :type 'utf8-string :value (string->der-utf8-string s)))
(define (display-text->string ds)
  (asn1-string->string (display-text-string ds)))

(define-class <x509-notice-reference> (<immutable>)
  ((organization :init-keyword :organization
		 :reader x509-notice-reference-organization)
   (notice-numbers :init-keyword :notice-numbers
		   :reader x509-notice-reference-notice-numbers)))
(define (x509-notice-reference? o) (is-a? o <x509-notice-reference>))
(define (make-x509-notice-reference (organization string?) . numbers)
  (unless (for-all integer? numbers)
    (assertion-violation 'make-x509-notice-reference
			 "numbers must integers" numbers))
  (make <x509-notice-reference> :organization organization
	:notice-numbers numbers))
(define (x509-notice-reference->notice-reference notice-refence)
  (make <notice-reference>
    :organization (string->display-text
		   (x509-notice-reference-organization notice-refence))
    :notice-numbers (make-der-sequence
		     (map integer->der-integer
			  (x509-notice-reference-notice-numbers notice-refence)))))
(define (notice-reference->x509-notice-reference notice-reference)
  (apply make-x509-notice-reference
	 (display-text->string (notice-reference-organization notice-reference))
	 (map der-integer->integer
	      (notice-reference-notice-numbers notice-reference))))

(define-class <x509-user-notice> (<immutable>)
  ((ref :init-keyword :ref :reader x509-user-notice-ref)
   (explicit-text :init-keyword :explicit-text
		  :reader x509-user-notice-explicit-text)))
(define (x509-user-notice? o) (is-a? o <x509-user-notice>))
(define (make-x509-user-notice
	 :key ((ref (or #f x509-notice-reference?)) #f)
	      ((explicit-text (or #f string?)) #f))
  (make <x509-user-notice> :ref ref :explicit-text explicit-text))
(define (x509-user-notice->user-notice (user-notice x509-user-notice?))  
  (define (maybe o conv) (and o (conv o)))
  (make <user-notice>
    :notice-ref (maybe (x509-user-notice-ref user-notice)
		       x509-notice-reference->notice-reference)
    :explicit-text (maybe (x509-user-notice-explicit-text user-notice)
			  string->display-text)))
(define (user-notice->x509-user-notice user-notice)
  (define (maybe o conv) (and o (conv o)))
  (make-x509-user-notice
   :ref (maybe (user-notice-notice-ref user-notice)
	       notice-reference->x509-notice-reference)
   :explicit-text (maybe (user-notice-explicit-text user-notice)
			 display-text->string)))

(define-class <x509-policy-qualifier-info> (<immutable>)
  ((id :init-keyword :id :reader x509-policy-qualifier-info-id)
   (qualifier :init-keyword :qualifier
	      :reader x509-policy-qualifier-info-qualifier)))
(define-method write-object ((o <x509-policy-qualifier-info>) p)
  (format p "#<x509-policy-qualifier-info id=~a qualifier=~a>"
	  (x509-policy-qualifier-info-id o)
	  (x509-policy-qualifier-info-qualifier o)))

(define (x509-policy-qualifier-info? o) (is-a? o <x509-policy-qualifier-info>))
(define (make-x509-policy-qualifier-info (id oid?) qualifier)
  (make <x509-policy-qualifier-info>
    :id (if (string? id) id (der-object-identifier->oid-string id))
    :qualifier qualifier))
(define-generic x509-policy-qualifier->asn1-encodable) ;; for future extension
(define-method x509-policy-qualifier->asn1-encodable (id (q <asn1-object>)) q)

(define-generic asn1-encodable->x509-policy-qualifier) ;; for future extension
(define-method asn1-encodable->x509-policy-qualifier (id (q <asn1-object>)) q)

(define *cps-id*
  (der-object-identifier->oid-string *policy-qualifier-type:cps*))
(define *unotice-id*
  (der-object-identifier->oid-string *policy-qualifier-type:unotice*))
(define (x509-policy-qualifier-info->policy-qualifier-info policy-qualifier-info)
  (define (qualifier->asn1-encodable id q)
    (cond ((string=? id *cps-id*) (string->der-ia5-string q))
	  ((string=? id *unotice-id*) (x509-user-notice->user-notice q))
	  (else (x509-policy-qualifier->asn1-encodable id q))))
  (let ((id (x509-policy-qualifier-info-id policy-qualifier-info))
	(q  (x509-policy-qualifier-info-qualifier policy-qualifier-info)))
    (make <policy-qualifier-info>
      :policy-qualifier-id (oid-string->der-object-identifier id)
      :qualifier (qualifier->asn1-encodable id q))))

(define (policy-qualifier-info->x509-policy-qualifier-info policy-qualifier-info)
  (let ((id (policy-qualifier-info-policy-qualifier-id policy-qualifier-info))
	(qualifier (policy-qualifier-info-qualifier policy-qualifier-info)))
    (make-x509-policy-qualifier-info id
     (cond ((equal? id *policy-qualifier-type:cps*) 
	    (der-ia5-string->string qualifier))
	   ((equal? id *policy-qualifier-type:unotice*)
	    (user-notice->x509-user-notice
	     (asn1-object->asn1-encodable <user-notice> qualifier)))
	   (else
	    (asn1-encodable->x509-policy-qualifier
	     (der-object-identifier->oid-string id) qualifier))))))
  
(define-class <x509-policy-information> (<immutable>)
  ((identifier :init-keyword :identifier
	       :reader x509-policy-information-identifier)
   (qualifiers :init-keyword :qualifiers
	       :reader x509-policy-information-qualifiers)))
(define-method write-object ((o <x509-policy-information>) p)
  (let ((q (x509-policy-information-qualifiers o)))
    (if (null? q)
	(format p "#<x509-policy-information id=~a>"
		(x509-policy-information-identifier o))
	(format p "#<x509-policy-information id=~a qualifier=~a>"
		(x509-policy-information-identifier o) q))))
(define (x509-policy-information? o) (is-a? o <x509-policy-information>))
(define (make-x509-policy-information (id oid?) . qualifiers)
  (unless (for-all x509-policy-qualifier-info? qualifiers)
    (assertion-violation 'make-x509-policy-information
			 "qualifiers must be x509-policy-qualifier-info"
			 qualifiers))
  (make <x509-policy-information>
    :identifier (if (string? id) id (der-object-identifier->oid-string id))
    :qualifiers qualifiers))
(define (x509-policy-information->policy-information policy-information)
  (let ((id (x509-policy-information-identifier policy-information))
	(q* (x509-policy-information-qualifiers policy-information)))
    (make <policy-information>
      :policy-identifier (oid-string->der-object-identifier id)
      :policy-qualifiers (and q* (make-der-sequence
				  (map x509-policy-qualifier-info->policy-qualifier-info q*))))))
(define (policy-information->x509-policy-infomation policy-information)
  (let ((q* (policy-information-policy-qualifiers policy-information)))
    (if q*
	(apply make-x509-policy-information
	       (policy-information-policy-identifier policy-information)
	       (map policy-qualifier-info->x509-policy-qualifier-info q*))
	(make-x509-policy-information
	 (policy-information-policy-identifier policy-information)))))

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
    
