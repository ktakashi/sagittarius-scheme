;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkcs/pbes.scm - PKCS#5 operation
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
(library (sagittarius crypto pkcs pbes)
    (export pkcs-pbe-parameter? <pkcs-pbe-parameter>
	    pkcs-pbe-parameter-salt
	    pkcs-pbe-parameter-iteration-count

	    pkcs-pbes2-params? <pkcs-pbes2-params>
	    pkcs-pbes2-params-key-derivation-fun
	    pkcs-pbes2-params-encryption-scheme

	    pkcs-pbkdf2-param? <pkcs-pbkdf2-params>
	    pkcs-pbkdf2-params-salt
	    pkcs-pbkdf2-params-iteration-count
	    pkcs-pbkdf2-params-key-length
	    pkcs-pbkdf2-params-prf

	    pkcs-rc2-cbc-parameter? <pkcs-rc2-cbc-parameter>
	    pkcs-rc2-cbc-parameter-version
	    pkcs-rc2-cbc-parameter-iv

	    pkcs-rc5-cbc-parameter? <pkcs-rc5-cbc-parameter>
	    pkcs-rc5-cbc-parameter-version
	    pkcs-rc5-cbc-parameter-rounds
	    pkcs-rc5-cbc-parameter-block-size-in-bits
	    pkcs-rc5-cbc-parameter-iv 
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkcs modules pbes)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto keys)
	    (sagittarius combinators))
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
(define-class <pkcs-pbe-parameter> (<asn1-encodable-container>)
  ((salt :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbe-parameter-salt asn1-encodable-container-c)
	       der-octet-string->bytevector)
    :reader pkcs-pbe-parameter-salt)
   (iteration-count :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbe-parameter-iteration-count asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-pbe-parameter-iteration-count)))
(define (pkcs-pbe-parameter? o) (is-a? o <pkcs-pbe-parameter>))
(define sid der-object-identifier->oid-string)

(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:pbe/md2-des-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:pbe/md2-rc2-cbd*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:pbe/md5-des-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:pbe/md5-rc2-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:pbe/sha1-des-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:pbe/sha1-rc2-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))

(define-class <pkcs-pbes2-params> (<asn1-encodable-container>)
  ((key-derivation-fun :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbes2-params-key-derivation-func
		   asn1-encodable-container-c)
	       algorithm-identifier->x509-algorithm-identifier)
    :reader pkcs-pbes2-params-key-derivation-fun)
   (encryption-scheme :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbes2-params-encryption-scheme
		   asn1-encodable-container-c)
	       algorithm-identifier->x509-algorithm-identifier)
    :reader pkcs-pbes2-params-encryption-scheme)))
(define (pkcs-pbes2-params? o) (is-a? o <pkcs-pbes2-params>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:pbes2*))))
  (values <pkcs-pbes2-params> <pbes2-params>))

(define-class <pkcs-pbkdf2-params> (<asn1-encodable-container>)
  ((salt :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbkdf2-salt-choice-value
		   pbkdf2-params-salt
		   asn1-encodable-container-c)
	       ;; we don't support other source for now
	       der-octet-string->bytevector)
    :reader pkcs-pbkdf2-params-salt)
   (iteration-count :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbkdf2-params-iteration-count
		   asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-pbkdf2-params-iteration-count)
   (key-length :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbkdf2-params-key-length
		   asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-pbkdf2-params-key-length)
   (prf :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbkdf2-params-key-length
		   asn1-encodable-container-c)
	       algorithm-identifier->x509-algorithm-identifier)
    :reader pkcs-pbkdf2-params-prf)))
(define (pkcs-pbkdf2-param? o) (is-a? o <pkcs-pbkdf2-params>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:pbkdf2*))))
  (values <pkcs-pbkdf2-params> <pbkdf2-params>))

(define-class <pkcs-rc2-cbc-parameter> (<asn1-encodable-container>)
  ((version :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ rc2-cbc-parameter-version
		   asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-rc2-cbc-parameter-version)
   (iv :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ rc2-cbc-parameter-iv
		   asn1-encodable-container-c)
	       der-octet-string->bytevector)
    :reader pkcs-rc2-cbc-parameter-iv)))
(define (pkcs-rc2-cbc-parameter? o) (is-a? o <pkcs-rc2-cbc-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:rc2-cbc*))))
  (values <pkcs-rc2-cbc-parameter> <rc2-cbc-parameter>))

(define-class <pkcs-rc5-cbc-parameter> (<asn1-encodable-container>)
  ((version :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ rc5-cbc-parameter-version
		   asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-rc5-cbc-parameter-version)
   (rounds :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ rc5-cbc-parameter-rounds
		   asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-rc5-cbc-parameter-rounds)
   (block-size-in-bits :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ rc5-cbc-parameter-block-size-in-bits
		   asn1-encodable-container-c)
	       der-integer->integer)
    :reader pkcs-rc5-cbc-parameter-block-size-in-bits)
   (iv :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ rc5-cbc-parameter-iv
		   asn1-encodable-container-c)
	       der-octet-string->bytevector)
    :reader pkcs-rc5-cbc-parameter-iv)))
(define (pkcs-rc5-cbc-parameter? o) (is-a? o <pkcs-rc5-cbc-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:rc5-cbc*))))
  (values <pkcs-rc5-cbc-parameter> <rc5-cbc-parameter>))

)
