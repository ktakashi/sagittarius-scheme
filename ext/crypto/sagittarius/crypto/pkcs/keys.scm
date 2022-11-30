;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkcs/keys.scm - PKCS#8 key operation
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
(library (sagittarius crypto pkcs keys)
    (export import-private-key export-private-key

	    pkcs-encrypted-private-key-info? <pkcs-encrypted-private-key-info>
	    pkcs-encrypted-private-key-info-encryption-algorithm
	    pkcs-encrypted-private-key-info-encrypted-data
	    read-pkcs-encrypted-private-key-info
	    bytevector->pkcs-encrypted-private-key-info
	    write-pkcs-encrypted-private-key-info
	    pkcs-encrypted-private-key-info->bytevector

	    pkcs-one-asymmetric-key? <pkcs-one-asymmetric-key>
	    pkcs-one-asymmetric-key-version
	    pkcs-one-asymmetric-key-private-key-algorithm
	    pkcs-one-asymmetric-key-private-key
	    pkcs-one-asymmetric-key-raw-key-value ;; for PKCS#12 secret key
	    pkcs-one-asymmetric-key-attributes
	    pkcs-one-asymmetric-key-public-key
	    pkcs-one-asymmetric-key->bytevector
	    private-key->pkcs-one-asymmetric-key
	    write-pkcs-one-asymmetric-key
	    pkcs-one-asymmetric-key->pkcs-encrypted-private-key-info
	    pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key

	    ;; For (rsa pkcs :8)...
	    one-asymmetric-key->private-key
	    ;; For keystore
	    one-asymmetric-key->pkcs-one-asymmetric-key
	    pkcs-one-asymmetric-key->one-asymmetric-key
	    encrypted-private-key-info->pkcs-encrypted-private-key-info
	    pkcs-encrypted-private-key-info->encrypted-private-key-info)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkcs modules akp)
	    (sagittarius crypto pkcs algorithms)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix attributes)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto keys)
	    (sagittarius combinators))
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))

(define (one-asymmetric-key->private-key (oakp one-asymmetric-key?))
  (import-private-key (asn1-encodable->asn1-object oakp)
		      (private-key-format private-key-info)))
(define (one-asymmetric-key->public-key (oakp one-asymmetric-key?))
  (let ((pk (one-asymmetric-key-public-key oakp)))
    (and pk
	 (import-public-key
	  (make <subject-public-key-info>
	    :algorithm (one-asymmetric-key-private-key-algorithm oakp)
	    :subject-public-key pk)))))

(define-class <pkcs-one-asymmetric-key> (<asn1-encodable-container>)
  ((version :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ one-asymmetric-key-vertsion
		   asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-one-asymmetric-key-version)
   (private-key-algorithm :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ one-asymmetric-key-private-key-algorithm
		   asn1-encodable-container-c)
	       algorithm-identifier->x509-algorithm-identifier)
    :reader pkcs-one-asymmetric-key-private-key-algorithm)
   (private-key :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ asn1-encodable-container-c)
	       one-asymmetric-key->private-key)
    :reader pkcs-one-asymmetric-key-private-key)
   (raw-key-value :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ one-asymmetric-key-private-key
		   asn1-encodable-container-c)
	       der-octet-string->bytevector)
    :reader pkcs-one-asymmetric-key-raw-key-value)
   (attributes :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ one-asymmetric-key-attributes
		   asn1-encodable-container-c)
	       attributes->x509-attributes)
    :reader pkcs-one-asymmetric-key-attributes)
   (public-key :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ asn1-encodable-container-c)
	       one-asymmetric-key->public-key)
    :reader pkcs-one-asymmetric-key-public-key)))
(define (pkcs-one-asymmetric-key? o) (is-a? o <pkcs-one-asymmetric-key>))

(define-method import-private-key ((key <pkcs-one-asymmetric-key>))
  (one-asymmetric-key->private-key (asn1-encodable-container-c key)))
(define-method export-private-key ((key <pkcs-one-asymmetric-key>))
  (pkcs-one-asymmetric-key->bytevector key))

(define-method import-private-key ((key <one-asymmetric-key>))
  (one-asymmetric-key->private-key key))
(define-method export-private-key ((key <one-asymmetric-key>))
  (asn1-encodable->bytevector key))

(define (private-key->one-asymmetric-key (private-key private-key?))
  (let ((bv (export-private-key private-key
				(private-key-format private-key-info))))
    (bytevector->asn1-encodable <one-asymmetric-key> bv)))

(define (private-key->pkcs-one-asymmetric-key (private-key private-key?))
  (let ((c (private-key->one-asymmetric-key private-key)))
    (make <pkcs-one-asymmetric-key> :c c)))

(define (pkcs-one-asymmetric-key->one-asymmetric-key oak)
  (asn1-encodable-container-c oak))
(define (pkcs-one-asymmetric-key->bytevector oak)
  (asn1-encodable->bytevector 
   (pkcs-one-asymmetric-key->one-asymmetric-key oak)))
(define (write-pkcs-one-asymmetric-key oak
	 :optional (out (current-output-port)))
  (put-bytevector out (pkcs-one-asymmetric-key->bytevector oak)))

(define (bytevector->pkcs-one-asymmetric-key bv)
  (read-pkcs-one-asymmetric-key (open-bytevector-input-port bv)))
(define (read-pkcs-one-asymmetric-key in)
  (one-asymmetric-key->pkcs-one-asymmetric-key
   (asn1-object->asn1-encodable <one-asymmetric-key> (read-asn1-object in))))
(define (one-asymmetric-key->pkcs-one-asymmetric-key epki)
  (make <pkcs-one-asymmetric-key> :c epki))

(define-class <pkcs-encrypted-private-key-info> (<asn1-encodable-container>)
  ((encryption-algorithm :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ encrypted-private-key-info-encryption-algorithm
		   asn1-encodable-container-c)
	       algorithm-identifier->x509-algorithm-identifier)
    :reader pkcs-encrypted-private-key-info-encryption-algorithm)
   (encrypted-date :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ encrypted-private-key-info-encrypted-data
		   asn1-encodable-container-c)
	       der-octet-string->bytevector)
    :reader pkcs-encrypted-private-key-info-encrypted-data)))
(define (pkcs-encrypted-private-key-info? o)
  (is-a? o <pkcs-encrypted-private-key-info>))
(define (make-pkcs-encrypted-private-key-info
	 (x509-aid x509-algorithm-identifier?) (bv bytevector?))
  (define aid (x509-algorithm-identifier->algorithm-identifier x509-aid))
  (make <pkcs-encrypted-private-key-info>
    :c (make <encrypted-private-key-info>
	 :encryption-algorithm aid
	 :encrypted-data (bytevector->der-octet-string bv))))

(define (bytevector->pkcs-encrypted-private-key-info bv)
  (read-pkcs-encrypted-private-key-info (open-bytevector-input-port bv)))
(define (read-pkcs-encrypted-private-key-info in)
  (encrypted-private-key-info->pkcs-encrypted-private-key-info
   (asn1-object->asn1-encodable <encrypted-private-key-info>
				(read-asn1-object in))))
(define (encrypted-private-key-info->pkcs-encrypted-private-key-info epki)
  (make <pkcs-encrypted-private-key-info> :c epki))

(define (pkcs-encrypted-private-key-info->encrypted-private-key-info epki)
  (asn1-encodable-container-c epki))
(define (write-pkcs-encrypted-private-key-info epki
	 :optional (out (current-output-port)))
  (put-bytevector out (pkcs-encrypted-private-key-info->bytevector epki)))
(define (pkcs-encrypted-private-key-info->bytevector epki)
  (asn1-encodable->bytevector 
   (pkcs-encrypted-private-key-info->encrypted-private-key-info epki)))

(define (pkcs-one-asymmetric-key->pkcs-encrypted-private-key-info
	 (oak pkcs-one-asymmetric-key?)
	 (aid x509-algorithm-identifier?) key . opts)
  (let ((data (pkcs-one-asymmetric-key->bytevector oak)))
    (make-pkcs-encrypted-private-key-info aid
     (apply pkcs-encrypt-data aid key data opts))))

(define (pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key
	 (epki pkcs-encrypted-private-key-info?) key . opts)
  (define aid (pkcs-encrypted-private-key-info-encryption-algorithm epki))
  (define data (pkcs-encrypted-private-key-info-encrypted-data epki))
  (bytevector->pkcs-one-asymmetric-key
   (apply pkcs-decrypt-data aid key data opts)))
     
)
