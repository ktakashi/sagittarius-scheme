;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/extensions/alt-names.scm - Subjet / Issuer Alt names
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
(library (sagittarius crypto pkix extensions alt-names)
    (export x509-general-name? <x509-general-name>
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
	    general-names->x509-general-names
	    x509-general-names->general-names

	    ;; For now put it here
	    ;; authorityKeyIdentifier
	    x509-authority-key-identifier? <x509-authority-key-identifier>
	    make-x509-authority-key-identifier
	    x509-authority-key-identifier-key-identifier
	    x509-authority-key-identifier-authority-cert-issuer
	    x509-authority-key-identifier-authority-cert-serial-number

	    authority-key-identifier->x509-authority-key-identifier
	    ;; for testing
	    x509-authority-key-identifier->authority-key-identifier
	    
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix dn)
	    (sagittarius mop immutable))

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
(define (general-names->x509-general-names (general-names general-names?))
  (make <x509-general-names>
    :names (map general-name->x509-general-name
		(general-names-components general-names))))

;; Authoritykeyidentifier
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
				  general-names->x509-general-names)
    :authority-cert-serial-number (maybe (authority-key-identifier-authority-cert-serial-number key-identifier)
					 der-integer->integer)))

)
    
