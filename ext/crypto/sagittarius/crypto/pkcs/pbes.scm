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
	    pkcs-pbes2-params-key-derivation-func
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
	    (sagittarius crypto pkcs algorithms)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto digests)
	    (sagittarius crypto keys)
	    (sagittarius crypto kdfs)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto mac hmac)
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
  ((oid (equal (sid *pbes:pbe/md2-rc2-cbc*))))
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
  ((key-derivation-func :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbes2-params-key-derivation-func
		   asn1-encodable-container-c)
	       algorithm-identifier->x509-algorithm-identifier)
    :reader pkcs-pbes2-params-key-derivation-func)
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

(define *hmac/sha1-aid*
  (make-x509-algorithm-identifier (sid *pbes:hmac/sha1*)))
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
	       (lambda (v) (and v (der-integer->integer v))))
    :reader pkcs-pbkdf2-params-key-length)
   (prf :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ pbkdf2-params-prf
		   asn1-encodable-container-c)
	       (lambda (ai)
		 (or (and ai
			  (algorithm-identifier->x509-algorithm-identifier ai))
		     *hmac/sha1-aid*)))
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
  ((oid (equal (sid *pbes:rc5-cbc-pad*))))
  (values <pkcs-rc5-cbc-parameter> <rc5-cbc-parameter>))

;; PBES1
;; Don't use this in new application :)
;; The key derivation happens twice for better design, and it's obviously
;; slow. But supporting them is only for backward compatibility purpose
;; so shouldn't be an issue
(define-method oid->kdf ((oid (equal (sid *pbes:pbe/md2-des-cbc*)))  param)
  (pbe-parameter->pbkdf1 param *digest:md2*))
(define-method oid->kdf ((oid (equal (sid *pbes:pbe/md2-rc2-cbc*)))  param)
  (pbe-parameter->pbkdf1 param *digest:md2*))
(define-method oid->kdf ((oid (equal (sid *pbes:pbe/md5-des-cbc*)))  param)
  (pbe-parameter->pbkdf1 param *digest:md5*))
(define-method oid->kdf ((oid (equal (sid *pbes:pbe/md5-rc2-cbc*)))  param)
  (pbe-parameter->pbkdf1 param *digest:md5*))
(define-method oid->kdf ((oid (equal (sid *pbes:pbe/sha1-des-cbc*))) param)
  (pbe-parameter->pbkdf1 param *digest:sha-1*))
(define-method oid->kdf ((oid (equal (sid *pbes:pbe/sha1-rc2-cbc*))) param)
  (pbe-parameter->pbkdf1 param *digest:sha-1*))
(define (pbe-parameter->pbkdf1 param digest)
  (lambda (key . ignore)
    (let ((bv (pbkdf-1 key
		       (pkcs-pbe-parameter-salt param)
		       (pkcs-pbe-parameter-iteration-count param)
		       16
		       :digest digest)))
      (bytevector-copy bv 0 8))))

(define-method oid->cipher ((oid (equal (sid *pbes:pbe/md2-des-cbc*)))  param)
  (make-cipher param *scheme:des* *digest:md2*))
(define-method oid->cipher ((oid (equal (sid *pbes:pbe/md2-rc2-cbc*)))  param)
  (make-cipher param *scheme:rc2* *digest:md2*))
(define-method oid->cipher ((oid (equal (sid *pbes:pbe/md5-des-cbc*)))  param)
  (make-cipher param *scheme:des* *digest:md5*))
(define-method oid->cipher ((oid (equal (sid *pbes:pbe/md5-rc2-cbc*)))  param)
  (make-cipher param *scheme:rc2* *digest:md5*))
(define-method oid->cipher ((oid (equal (sid *pbes:pbe/sha1-des-cbc*))) param)
  (make-cipher param *scheme:des* *digest:sha-1*))
(define-method oid->cipher ((oid (equal (sid *pbes:pbe/sha1-rc2-cbc*))) param)
  (make-cipher param *scheme:rc2* *digest:sha-1*))
(define (make-cipher param scheme digest)
  (lambda (key)
    (let ((bv (pbkdf-1 key
		       (pkcs-pbe-parameter-salt param)
		       (pkcs-pbe-parameter-iteration-count param)
		       16
		       :digest digest)))
      (values (make-symmetric-cipher scheme *mode:cbc*)
	      (make-iv-parameter (bytevector-copy bv 8 16))))))

;; PBES2
(define-method oid->kdf ((oid (equal (sid *pbes:pbes2*))) param)
  (let ((kdf-aid (pkcs-pbes2-params-key-derivation-func param)))
    (oid->kdf (x509-algorithm-identifier-oid kdf-aid)
	      (x509-algorithm-identifier-parameters kdf-aid)
	      ;; fxxk
	      param)))
(define-method oid->kdf ((oid (equal (sid *pbes:pbkdf2*))) param oparam)
  (define prf-aid (pkcs-pbkdf2-params-prf param))
  (define prf-oid (x509-algorithm-identifier-oid prf-aid))
  ;; TODO support CMAC PRF
  (define digest (oid->digest-descriptor (hmac-oid->digest-oid prf-oid)))
  (define prf (mac->prf-provider *mac:hmac* :digest digest))
  (define enc (pkcs-pbes2-params-encryption-scheme oparam))
  (define dk-len (or (pkcs-pbkdf2-params-key-length param)
		     ;; TODO get key length from param for RC2
		     (check-key-length enc)))
  (lambda (key . ignroe)
    (pbkdf-2 key
	     (pkcs-pbkdf2-params-salt param)
	     (pkcs-pbkdf2-params-iteration-count param)
	     dk-len
	     :prf prf)))

(define (check-key-length aid)
  (let-values (((scheme ignore)
		(oid->encryption-scheme (x509-algorithm-identifier-oid aid))))
    (symmetric-cipher-descriptor-max-key-length scheme)))


(define-method oid->cipher ((oid (equal (sid *pbes:pbes2*))) param)
  (define enc (pkcs-pbes2-params-encryption-scheme param))
  (define iv (encryption-scheme->iv enc))
  (let-values (((scheme mode)
		(oid->encryption-scheme (x509-algorithm-identifier-oid enc))))
    (lambda (key)
      (values (make-symmetric-cipher scheme mode)
	      ;; TODO add more param such as rounds for RC5
	      (make-iv-parameter iv)))))

(define (encryption-scheme->iv aid)
  (oid->iv (x509-algorithm-identifier-oid aid)
	   (x509-algorithm-identifier-parameters aid)))
;; default
(define-method oid->iv (oid param) (der-octet-string->bytevector param))

(define-method oid->encryption-scheme
  ((oid (equal (sid *pbes:aes128-cbc-pad*))))
  (values *scheme:aes-128* *mode:cbc*))
(define-method oid->encryption-scheme
  ((oid (equal (sid *pbes:aes192-cbc-pad*))))
  (values *scheme:aes-192* *mode:cbc*))
(define-method oid->encryption-scheme
  ((oid (equal (sid *pbes:aes256-cbc-pad*))))
  (values *scheme:aes-256* *mode:cbc*))
(define-method oid->encryption-scheme ((oid (equal (sid *pbes:des-cbc*))))
  (values *scheme:des* *mode:cbc*))
(define-method oid->encryption-scheme ((oid (equal (sid *pbes:desede-cbc*))))
  (values *scheme:des3* *mode:cbc*))
(define-method oid->encryption-scheme ((oid (equal (sid *pbes:rc2-cbc*))))
  (values *scheme:rc2* *mode:cbc*))
(define-method oid->encryption-scheme ((oid (equal (sid *pbes:rc5-cbc-pad*))))
  (values *scheme:rc5* *mode:cbc*))
)
