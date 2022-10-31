;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/extensions/cps.scm - Certificate PolicieS extension
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
(library (sagittarius crypto pkix extensions cps)
    (export x509-policy-qualifier-info? <x509-policy-qualifier-info>
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
	    policy-information->x509-policy-infomation
	    *policy-qualifier-type:cps*
	    *policy-qualifier-type:unotice*
	    
	    ;; For testing
	    x509-policy-information->policy-information
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius mop immutable))

;; useful utility :)
(define (oid? o) (or (der-object-identifier? o) (object-identifier-string? o)))

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

)
    
