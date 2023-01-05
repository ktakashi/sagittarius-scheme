;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/cms.scm - CMS
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
(library (sagittarius crypto pkcs cms)
    (export cms-encrypted-content-info? <cms-encrypted-content-info>
	    cms-encrypted-content-info-content-type
	    cms-encrypted-content-info-content-encryption-algorithm
	    cms-encrypted-content-info-encrypted-content
	    encrypted-content-info->cms-encrypted-content-info
	    cms-encrypted-content-info->encrypted-content-info

	    cms-encrypted-data? <cms-encrypted-data>
	    make-cms-encrypted-data
	    cms-encrypted-data-version
	    cms-encrypted-data-encrypted-content-info
	    cms-encrypted-data-unprotected-attrs
	    encrypted-data->cms-encrypted-data

	    cms-content-info? <cms-content-info>
	    make-cms-data-content-info
	    make-cms-encrypted-data-content-info
	    cms-content-info-content-type
	    cms-content-info-content
	    cms-encrypted-content-info->cms-content-info
	    cms-content-info->cms-encrypted-content-info
	    cms-content-info->content-info
	    content-info->cms-content-info
	    bytevector->cms-content-info
	    cms-content-info->bytevector)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkcs algorithms)
	    (sagittarius crypto pkcs modules cms)
	    (sagittarius crypto pkix attributes)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius combinators))

(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
(define-class <cms-encrypted-content-info> (<asn1-encodable-container>)
  ((content-type :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ encrypted-content-info-content-type
		   asn1-encodable-container-c)
	       der-object-identifier->oid-string)
    :reader cms-encrypted-content-info-content-type)
   (content-encryption-algorithm :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ encrypted-content-info-content-encryption-algorithm
		   asn1-encodable-container-c)
	       algorithm-identifier->x509-algorithm-identifier)
    :reader cms-encrypted-content-info-content-encryption-algorithm)
   (encrypted-content :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ encrypted-content-info-encrypted-content
		   asn1-encodable-container-c)
	       der-octet-string->bytevector)
    :reader cms-encrypted-content-info-encrypted-content)))
(define (cms-encrypted-content-info? o) (is-a? o <cms-encrypted-content-info>))
(define (encrypted-content-info->cms-encrypted-content-info
	 (eci encrypted-content-info?))
  (make <cms-encrypted-content-info> :c eci))
(define (cms-encrypted-content-info->encrypted-content-info
	 (eci cms-encrypted-content-info?))
  (asn1-encodable-container-c eci))

(define-class <cms-encrypted-data> (<asn1-encodable-container>)
  ((version :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ encrypted-data-version asn1-encodable-container-c)
	       der-integer->integer)
    :reader cms-encrypted-data-version)
   (encrypted-content-info :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ encrypted-data-encrypted-content-info
		   asn1-encodable-container-c)
	       encrypted-content-info->cms-encrypted-content-info)
    :reader cms-encrypted-data-encrypted-content-info)
   (unprotected-attrs :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ encrypted-data-unprotected-attrs
		   asn1-encodable-container-c)
	       attributes->x509-attributes)
    :reader cms-encrypted-data-unprotected-attrs)))
(define (cms-encrypted-data? o) (is-a? o <cms-encrypted-data>))
(define (encrypted-data->cms-encrypted-data (ed encrypted-data?))
  (make <cms-encrypted-data> :c ed))
(define (cms-encrypted-data->encrypted-data (ed cms-encrypted-data?))
  (asn1-encodable-container-c ed))

(define (make-cms-encrypted-data (eci cms-encrypted-content-info?)
				 :optional (attributes #f))
  (let ((raw-eci (cms-encrypted-content-info->encrypted-content-info eci))
	(attrs (and attributes (x509-attributes->attributes attributes))))
    (make <cms-encrypted-data>
      :c (make <encrypted-data>
	   :version (integer->der-integer (if attributes 2 0))
	   :encrypted-content-info raw-eci
	   :unprotected-attrs attrs))))

(define-generic content->cms-content)
(define-class <cms-content-info> (<asn1-encodable-container>)
  ((content-type :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ content-info-content-type asn1-encodable-container-c)
	       der-object-identifier->oid-string)
    :reader cms-content-info-content-type)
   (content :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ content-info-content asn1-encodable-container-c)
	       content->cms-content)
    :reader cms-content-info-content)))
(define (cms-content-info? o) (is-a? o <cms-content-info>))
(define-method content->cms-content ((d <der-octet-string>))
  (der-octet-string->bytevector d))
(define (cms-content-info->content-info (ci cms-content-info?))
  (asn1-encodable-container-c ci))
(define (content-info->cms-content-info (ci content-info?))
  (make <cms-content-info> :c ci))
(define (bytevector->cms-content-info bv)
  (content-info->cms-content-info
   (bytevector->asn1-encodable <content-info> bv)))
(define (cms-content-info->bytevector (ci cms-content-info?))
  (asn1-encodable->bytevector (asn1-encodable-container-c ci)))

(define (make-cms-data-content-info (content bytevector?))
  (make <cms-content-info>
    :c (make <content-info>
	 :content-type *cms:data-content-type*
	 :content (bytevector->der-octet-string content))))
(define (make-cms-encrypted-data-content-info (data cms-encrypted-data?))
  (make <cms-content-info>
    :c (make <content-info>
	 :content-type *cms:encrypted-data-content-type*
	 :content (cms-encrypted-data->encrypted-data data))))
  

(define (cms-encrypted-content-info->cms-content-info eci key . opts)
  (let ((aid (cms-encrypted-content-info-content-encryption-algorithm eci))
	(ct (cms-encrypted-content-info-content-type eci))
	(c (cms-encrypted-content-info-encrypted-content eci)))
    (make <cms-content-info>
      :c (make <content-info>
	   :content-type (oid-string->der-object-identifier ct)
	   :content (bytevector->der-octet-string
		     (apply pkcs-decrypt-data aid key c opts))))))

(define (cms-content-info->cms-encrypted-content-info ci aid key . opts)
  (let ((ct (cms-content-info-content-type ci))
	(c (cms-content-info-content ci))
	(id (x509-algorithm-identifier->algorithm-identifier aid)))
    (make <cms-encrypted-content-info>
      :c (make <encrypted-content-info>
	   :content-type (oid-string->der-object-identifier ct)
	   :content-encryption-algorithm id
	   :encrypted-content (bytevector->der-octet-string
			       (apply pkcs-encrypt-data aid key c opts))))))
)

