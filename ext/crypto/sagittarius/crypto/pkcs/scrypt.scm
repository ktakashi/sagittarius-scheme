;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/scrypt.scm - scrypt for PKCS 
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

;; ref
;; - https://datatracker.ietf.org/doc/html/rfc7914
#!nounbound
(library (sagittarius crypto pkcs scrypt)
    (export *pbes:scrypt*
	    pkcs-scrypt-params? <pkcs-scrypt-params>
	    make-pkcs-scrypt-params
	    pkcs-scrypt-params-salt
	    pkcs-scrypt-params-cost-parameter
	    pkcs-scrypt-params-block-size
	    pkcs-scrypt-params-parallelization-parameter
	    pkcs-scrypt-params-key-length
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto kdfs scrypt)
	    (sagittarius crypto pkcs modules scrypt)
	    (sagittarius crypto pkcs algorithms)
	    (sagittarius crypto pkcs modules pbes)
	    (sagittarius crypto pkcs pbes) ;; for <pkcs-pbes2-params>
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius combinators))
(define sid der-object-identifier->oid-string)
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
(define-class <pkcs-scrypt-params>
  (<asn1-encodable-container> <x509-algorithm-parameters>)
  ((salt :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ scrypt-params-salt asn1-encodable-container-c)
	       der-octet-string->bytevector)
    :reader pkcs-scrypt-params-salt)
   (cost-parameter :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ scrypt-params-cost-parameter asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-scrypt-params-cost-parameter)
   (block-size :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ scrypt-params-block-size asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-scrypt-params-block-size)
   (parallelization-parameter :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ scrypt-params-parallelization-parameter
		   asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-scrypt-params-parallelization-parameter)
   (key-length :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ scrypt-params-key-length
		   asn1-encodable-container-c)
	       (lambda (v) (and v (der-integer->integer v))))
    :reader pkcs-scrypt-params-key-length)))
(define (pkcs-scrypt-params? o) (is-a? o <pkcs-scrypt-params>))
(define (make-pkcs-scrypt-params (salt bytevector?)
				 (cost-parameter integer?)
				 (block-size integer?)
				 (parallelization-parameter integer?)
				 :key (key-length #f))
  (make <pkcs-scrypt-params>
    :c (make <scrypt-params>
	 :salt (bytevector->der-octet-string salt)
	 :cost-parameter (integer->der-integer cost-parameter)
	 :block-size (integer->der-integer block-size)
	 :parallelization-parameter (integer->der-integer parallelization-parameter)
	 :key-length (and key-length (integer->der-integer key-length)))))

(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:scrypt*))))
  (values <pkcs-scrypt-params> <scrypt-params>))
(define-method oid->kdf ((oid (equal (sid *pbes:scrypt*))) kdf-param pbes2-param)
  (define enc (pkcs-pbes2-params-encryption-scheme pbes2-param))
  (define dk-len (or (pkcs-scrypt-params-key-length kdf-param)
		     (encryption-scheme->key-length enc)))
  (lambda (key . ignore)
    (scrypt (string->utf8 key)
	    (pkcs-scrypt-params-salt kdf-param)
	    (pkcs-scrypt-params-cost-parameter kdf-param)
	    (pkcs-scrypt-params-block-size kdf-param)
	    (pkcs-scrypt-params-parallelization-parameter kdf-param)
	    dk-len)))
)
