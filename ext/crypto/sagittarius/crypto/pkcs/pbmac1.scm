;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkcs/pbmac1.scm - PBMAC1 in PKCS#12 MacData
;;;
;;;   Copyright (c) 2026  Sagittarius Scheme contributors
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

;; RFC 9879
#!nounbound
(library (sagittarius crypto pkcs pbmac1)
    (export *pkcs12-pbmac1-max-iterations*
	    pbmac1-algorithm-identifier?
	    pbmac1-algorithm-identifier->mac
	    make-pbmac1-mac-data)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto digests)
	    (sagittarius crypto kdfs)
	    (sagittarius crypto mac)
	    (sagittarius crypto pkcs modules pbes)
	    (sagittarius crypto pkcs modules pfx)
	    (sagittarius crypto pkcs pbes)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto random)
	    (srfi :39 parameters))

(define *pkcs12-pbmac1-max-iterations* (make-parameter 1000000))
(define sid der-object-identifier->oid-string)

(define (pbmac1-algorithm-identifier? aid)
  (equal? (algorithm-identifier-algorithm aid) *pbes:pbmac1*))

(define (pbmac1-hmac-digest aid)
  (define oid (x509-algorithm-identifier-oid aid))
  (define param (x509-algorithm-identifier-parameters aid))
  (unless (or (not param) (der-null? param))
    (assertion-violation 'pbmac1-algorithm-identifier->mac
			 "Invalid PBMAC1 HMAC parameters" param))
  (unless (member oid (map sid (list *pbes:hmac/sha224* *pbes:hmac/sha256*
				     *pbes:hmac/sha384* *pbes:hmac/sha512*
				     *pbes:hmac/sha512/224*
				     *pbes:hmac/sha512/256*)))
    (assertion-violation 'pbmac1-algorithm-identifier->mac
			 "Unsupported PBMAC1 HMAC" oid))
  (or (oid->digest-descriptor (hmac-oid->digest-oid oid))
      (assertion-violation 'pbmac1-algorithm-identifier->mac
			   "Unsupported PBMAC1 digest" oid)))

(define (pbmac1-algorithm-identifier->mac aid password)
  (define params
    (x509-algorithm-identifier-parameters
     (algorithm-identifier->x509-algorithm-identifier aid)))
  (unless (pkcs-pbmac1-params? params)
    (assertion-violation 'pbmac1-algorithm-identifier->mac
			 "Missing PBMAC1 parameters" params))
  (let* ((kdf (pkcs-pbmac1-params-key-derivation-func params))
	 (mac (pkcs-pbmac1-params-message-auth-scheme params))
	 (kdf-params (x509-algorithm-identifier-parameters kdf)))
    (unless (and (string=? (x509-algorithm-identifier-oid kdf)
			    (sid *pbes:pbkdf2*))
		 (pkcs-pbkdf2-param? kdf-params))
      (assertion-violation 'pbmac1-algorithm-identifier->mac
			   "Unsupported PBMAC1 KDF" kdf))
    (let* ((salt (pkcs-pbkdf2-params-salt kdf-params))
	   (count (pkcs-pbkdf2-params-iteration-count kdf-params))
	   (length (pkcs-pbkdf2-params-key-length kdf-params))
	   (prf (pbmac1-hmac-digest (pkcs-pbkdf2-params-prf kdf-params)))
	   (digest (pbmac1-hmac-digest mac)))
      (unless (and (positive? (bytevector-length salt))
		   (<= (bytevector-length salt) 1024)
		   (integer? count) (exact? count) (positive? count)
		   (<= count (*pkcs12-pbmac1-max-iterations*))
		   (integer? length) (exact? length) (<= 20 length 128))
	(assertion-violation 'pbmac1-algorithm-identifier->mac
			     "Invalid PBMAC1 KDF parameters" salt count length))
      (make-mac *mac:pbmac1* (string->utf8 password)
		:kdf (lambda (p)
		       (pbkdf-2 p salt count length
				:prf (mac->prf-provider *mac:hmac* :digest prf)))
		:mac-provider
		(lambda (key) (make-mac *mac:hmac* key :digest digest))))))

(define (make-pbmac1-mac-data password data prng salt-size count)
  (unless (and (integer? salt-size) (exact? salt-size)
	       (<= 1 salt-size 1024)
	       (integer? count) (exact? count)
	       (<= 1 count (*pkcs12-pbmac1-max-iterations*)))
    (assertion-violation 'make-pbmac1-mac-data
			 "Invalid PBMAC1 salt size or iteration count"
			 salt-size count))
  (let* ((salt (random-generator-read-random-bytes prng salt-size))
	 (mac-aid (make-x509-algorithm-identifier (sid *pbes:hmac/sha256*)
						   (make-der-null)))
	 (kdf-aid (make-pbkdf2-x509-algorithm-identifier
		   salt count :prf mac-aid :key-length 32))
	 (aid (make-pbmac1-x509-algorithm-identifier kdf-aid mac-aid))
	 (digest (generate-mac
		  (pbmac1-algorithm-identifier->mac
		   (x509-algorithm-identifier->algorithm-identifier aid)
		   password)
		  data)))
    (make <mac-data>
      :mac (make <digest-info>
	     :digest-algorithm
	     (x509-algorithm-identifier->algorithm-identifier aid)
	     :digest (bytevector->der-octet-string digest))
      :mac-salt (bytevector->der-octet-string
		 (random-generator-read-random-bytes prng salt-size))
      :iterations (integer->der-integer count))))

)
