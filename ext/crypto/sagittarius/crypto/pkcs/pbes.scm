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
	    make-pkcs-pbe-parameter
	    pkcs-pbe-parameter-salt
	    pkcs-pbe-parameter-iteration-count

	    pkcs-pbes2-params? <pkcs-pbes2-params>
	    make-pkcs-pbes2-params
	    pkcs-pbes2-params-key-derivation-func
	    pkcs-pbes2-params-encryption-scheme

	    pkcs-pbkdf2-param? <pkcs-pbkdf2-params>
	    make-pkcs-pbkdf2-params
	    pkcs-pbkdf2-params-salt
	    pkcs-pbkdf2-params-iteration-count
	    pkcs-pbkdf2-params-key-length
	    pkcs-pbkdf2-params-prf

	    pkcs-rc2-cbc-parameter? <pkcs-rc2-cbc-parameter>
	    make-pkcs-rc2-cbc-parameter
	    pkcs-rc2-cbc-parameter-version
	    pkcs-rc2-cbc-parameter-iv

	    pkcs-rc5-cbc-parameter? <pkcs-rc5-cbc-parameter>
	    make-pkcs-rc5-cbc-parameter
	    pkcs-rc5-cbc-parameter-version
	    pkcs-rc5-cbc-parameter-rounds
	    pkcs-rc5-cbc-parameter-block-size-in-bits
	    pkcs-rc5-cbc-parameter-iv

	    make-pbe-md2-des-cbc-x509-algorithm-identifier
	    make-pbe-md2-rc2-cbc-x509-algorithm-identifier
	    make-pbe-md5-des-cbc-x509-algorithm-identifier
	    make-pbe-md5-rc2-cbc-x509-algorithm-identifier
	    make-pbe-sha1-des-cbc-x509-algorithm-identifier
	    make-pbe-sha1-rc2-cbc-x509-algorithm-identifier
	    make-pbes2-x509-algorithm-identifier
	    make-pbkdf2-x509-algorithm-identifier
	    ;; PRF OID
	    *pbes:hmac/sha1*
	    *pbes:hmac/sha224*
	    *pbes:hmac/sha256*
	    *pbes:hmac/sha384*
	    *pbes:hmac/sha512*
	    *pbes:hmac/sha512/224*
	    *pbes:hmac/sha512/256*
	    *pbes:hmac/sha3-224*
	    *pbes:hmac/sha3-256*
	    *pbes:hmac/sha3-384*
	    *pbes:hmac/sha3-512*
	    make-aes128-encryption-x509-algorithm-identifier
	    make-aes192-encryption-x509-algorithm-identifier
	    make-aes256-encryption-x509-algorithm-identifier
	    make-des3-encryption-x509-algorithm-identifier
	    make-des-encryption-x509-algorithm-identifier
	    make-rc2-encryption-x509-algorithm-identifier
	    make-rc5-encryption-x509-algorithm-identifier
	    
	    encryption-scheme->cipher-parameters
	    parameter->key-length ;; for default key length
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkcs algorithms)
	    (sagittarius crypto pkcs modules pbes)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto digests)
	    (sagittarius crypto keys)
	    (sagittarius crypto kdfs)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto mac hmac)
	    (sagittarius combinators))
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
(define-class <pkcs-pbe-parameter>
  (<asn1-encodable-container> <x509-algorithm-parameters>)
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
(define (make-pkcs-pbe-parameter (salt bytevector?) (iteration integer?))
  (make <pkcs-pbe-parameter>
    :c (make <pbe-parameter>
	 :salt (bytevector->der-octet-string salt)
	 :iteration-count (integer->der-integer iteration))))

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

(define-class <pkcs-pbes2-params>
  (<asn1-encodable-container> <x509-algorithm-parameters>)
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
(define-class <pkcs-pbkdf2-params>
  (<asn1-encodable-container> <x509-algorithm-parameters>)
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

(define-class <pkcs-rc2-cbc-parameter>
  (<asn1-encodable-container> <x509-algorithm-parameters>)
  ((version :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ rc2-cbc-parameter-version
		   asn1-encodable-container-c)
	       (lambda (v) (and v (der-integer->integer v))))
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

(define-class <pkcs-rc5-cbc-parameter>
  (<asn1-encodable-container> <x509-algorithm-parameters>)
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
	       (lambda (v) (and v (der-octet-string->bytevector v))))
    :reader pkcs-rc5-cbc-parameter-iv)))
(define (pkcs-rc5-cbc-parameter? o) (is-a? o <pkcs-rc5-cbc-parameter>))
(define (pkcs-rc5-cbc-parameter-block-size o)
  (div (pkcs-rc5-cbc-parameter-block-size-in-bits o) 8))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pbes:rc5-cbc-pad*))))
  (values <pkcs-rc5-cbc-parameter> <rc5-cbc-parameter>))

;; Algorithm identifier ctrs
(define (make-pbe-md2-des-cbc-x509-algorithm-identifier salt count)
  (make-pbe-x509-algorithm-identifier *pbes:pbe/md2-des-cbc* salt count))
(define (make-pbe-md2-rc2-cbc-x509-algorithm-identifier salt count)
  (make-pbe-x509-algorithm-identifier *pbes:pbe/md2-rc2-cbc* salt count))
(define (make-pbe-md5-des-cbc-x509-algorithm-identifier salt count)
  (make-pbe-x509-algorithm-identifier *pbes:pbe/md5-des-cbc* salt count))
(define (make-pbe-md5-rc2-cbc-x509-algorithm-identifier salt count)
  (make-pbe-x509-algorithm-identifier *pbes:pbe/md5-rc2-cbc* salt count))
(define (make-pbe-sha1-des-cbc-x509-algorithm-identifier salt count)
  (make-pbe-x509-algorithm-identifier *pbes:pbe/sha1-des-cbc* salt count))
(define (make-pbe-sha1-rc2-cbc-x509-algorithm-identifier salt count)
  (make-pbe-x509-algorithm-identifier *pbes:pbe/sha1-rc2-cbc* salt count))
(define (make-pbe-x509-algorithm-identifier oid salt count)
  (make-x509-algorithm-identifier
   (der-object-identifier->oid-string oid)
   (make-pkcs-pbe-parameter salt count)))

(define (make-pbes2-x509-algorithm-identifier kdf encryption-scheme)
  (make-x509-algorithm-identifier
   (der-object-identifier->oid-string *pbes:pbes2*)
   (make-pkcs-pbes2-params kdf encryption-scheme)))

(define (make-pbkdf2-x509-algorithm-identifier salt count . opts)
  (make-x509-algorithm-identifier
   (der-object-identifier->oid-string *pbes:pbkdf2*)
   (apply make-pkcs-pbkdf2-params salt count opts)))

(define (make-pkcs-pbes2-params
	 (kdf x509-algorithm-identifier?)
	 (enc x509-algorithm-identifier?))
  (let ((kdf-aid (x509-algorithm-identifier->algorithm-identifier kdf))
	(enc-aid (x509-algorithm-identifier->algorithm-identifier enc)))
    (make <pkcs-pbes2-params>
      :c (make <pbes2-params>
	   :key-derivation-func kdf-aid
	   :encryption-scheme enc-aid))))

(define (make-pkcs-pbkdf2-params (salt bytevector?) (count integer?)
				 :key (prf #f) (key-length #f))
  (define (->aid prf)
    (cond ((not prf) prf)
	  ((x509-algorithm-identifier? prf)
	   (x509-algorithm-identifier->algorithm-identifier prf))
	  ((der-object-identifier? prf)
	   (make <algorithm-identifier> :algorithm prf))
	  ((and (string? prf) (object-identifier-string? prf))
	   (->aid (oid-string->der-object-identifier prf)))
	  (else (assertion-violation 'make-pkcs-pbkdf2-params "Unknown PRF"))))
  (make <pkcs-pbkdf2-params>
    :c (make <pbkdf2-params>
	 :salt (make <pbkdf2-salt-choice>
		 :type 'specified
		 :value (bytevector->der-octet-string salt))
	 :iteration-count (integer->der-integer count)
	 :key-length (and key-length (integer->der-integer key-length))
	 :prf (->aid prf))))
  
(define (make-aes128-encryption-x509-algorithm-identifier iv)
  (make-iv-x509-algorithm-identifier *pbes:aes128-cbc-pad* iv))
(define (make-aes192-encryption-x509-algorithm-identifier iv)
  (make-iv-x509-algorithm-identifier *pbes:aes192-cbc-pad* iv))
(define (make-aes256-encryption-x509-algorithm-identifier iv)
  (make-iv-x509-algorithm-identifier *pbes:aes256-cbc-pad* iv))
(define (make-des3-encryption-x509-algorithm-identifier iv)
  (make-iv-x509-algorithm-identifier *pbes:desede-cbc* iv))
(define (make-des-encryption-x509-algorithm-identifier iv)
  (make-iv-x509-algorithm-identifier *pbes:des-cbc* iv))
(define (make-iv-x509-algorithm-identifier oid iv)
  (make-x509-algorithm-identifier
   (der-object-identifier->oid-string oid)
   (bytevector->der-octet-string iv)))

(define (rc2-encoding? v)
  (or (not v)
      (memv v '(160 120 58))
      (>= v 256)))
(define (make-rc2-encryption-x509-algorithm-identifier version iv)
  (make-x509-algorithm-identifier
   (der-object-identifier->oid-string *pbes:rc2-cbc*)
   (make-pkcs-rc2-cbc-parameter version iv)))
(define (make-pkcs-rc2-cbc-parameter (version rc2-encoding?) iv)
  (make <pkcs-rc2-cbc-parameter>
    :c (make <rc2-cbc-parameter>
	 :version (and version (integer->der-integer version))
	 :iv (bytevector->der-octet-string iv))))

(define (make-rc5-encryption-x509-algorithm-identifier rounds block-size . opts)
  (make-x509-algorithm-identifier
   (der-object-identifier->oid-string *pbes:rc5-cbc-pad*)
   (apply make-pkcs-rc5-cbc-parameter rounds block-size opts)))

(define (make-pkcs-rc5-cbc-parameter rounds (block-size (or '64 '128))
				:optional (iv #f))
  (unless (or (not iv) (= block-size (* (bytevector-length iv) 8)))
    (assertion-violation 'make-rc5-cbc-parameter
			 "Block size and IV don't match"))
  (unless (<= 8 rounds 127)
    (assertion-violation 'make-rc5-cbc-parameter
			 "Rounds must be between 8 and 127" rounds))
  (make <pkcs-rc5-cbc-parameter>
    :c (make <rc5-cbc-parameter>
	 :version (integer->der-integer 16)
	 :rounds (integer->der-integer rounds)
	 :block-size-in-bits (integer->der-integer block-size)
	 :iv (and iv (bytevector->der-octet-string iv)))))

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
    (let ((bv (pbkdf-1 (string->utf8 key)
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
    (let ((bv (pbkdf-1 (string->utf8 key)
		       (pkcs-pbe-parameter-salt param)
		       (pkcs-pbe-parameter-iteration-count param)
		       16
		       :digest digest)))
      (values (make-block-cipher scheme *mode:cbc*)
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
    (pbkdf-2 (string->utf8 key)
	     (pkcs-pbkdf2-params-salt param)
	     (pkcs-pbkdf2-params-iteration-count param)
	     dk-len
	     :prf prf)))

(define-generic parameter->key-length)
(define-method parameter->key-length ((p <pkcs-rc2-cbc-parameter>))
  (case (pkcs-rc2-cbc-parameter-version p)
    ((160) 5)
    ((120) 8)
    ((58) 16)
    (else => (lambda (v) (or (and v (div v 8)) 4)))))
(define-method parameter->key-length ((p <pkcs-rc5-cbc-parameter>))
  (assertion-violation 'rc5-cbc-pad
		       "key-length field of KDF parameter must be set"))
(define (check-key-length aid)
  (define param (x509-algorithm-identifier-parameters aid))
  (if (x509-algorithm-parameters? param)
      (parameter->key-length param)
      (let-values (((scheme ignore) (oid->encryption-scheme
				     (x509-algorithm-identifier-oid aid))))
	(symmetric-cipher-descriptor-max-key-length scheme))))


(define-method oid->cipher ((oid (equal (sid *pbes:pbes2*))) param)
  (define enc (pkcs-pbes2-params-encryption-scheme param))
  (let-values (((scheme mode)
		(oid->encryption-scheme (x509-algorithm-identifier-oid enc))))
    (let ((parameter (->cipher-parameters
		      scheme (x509-algorithm-identifier-parameters enc))))
      (lambda (key)
	(values (make-block-cipher scheme mode) parameter)))))

(define-generic encryption-scheme->cipher-parameters)
(define (->cipher-parameters scheme param)
  (cond ((der-octet-string? param)
	 (make-iv-parameter (der-octet-string->bytevector param)))
	((pkcs-rc2-cbc-parameter? param)
	 (make-iv-parameter (pkcs-rc2-cbc-parameter-iv param)))
	((pkcs-rc5-cbc-parameter? param)
	 (let ((iv (cond ((pkcs-rc5-cbc-parameter-iv param))
			 (else
			  (make-bytevector
			   (pkcs-rc5-cbc-parameter-block-size param) 0)))))
	   (make-cipher-parameter
	    (make-iv-parameter iv)
	    (make-round-parameter (pkcs-rc5-cbc-parameter-rounds param)))))
	;; fallback for enhancement
	(else (encryption-scheme->cipher-parameters scheme param))))

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
