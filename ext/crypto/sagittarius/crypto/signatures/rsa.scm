;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/signatures/rsa.scm - RSA signature
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
(library (sagittarius crypto signatures rsa)
    (export *signature:rsa*
	    make-signer-state signer-state->signature
	    make-verifier-state verifier-state-verify-message

	    pkcs1-emsa-pss-encode pkcs1-emsa-pss-verify
	    pkcs1-emsa-v1.5-encode pkcs1-emsa-v1.5-verify
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto signatures types)
	    (sagittarius crypto ciphers asymmetric)
	    (sagittarius crypto keys)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (sagittarius crypto secure)
	    (util bytevector))

(define *signature:rsa* *key:rsa*)

(define-class <rsa-signer-state> (<digest-sigature-state> <signer-state>)
  ((encoder :init-keyword :encoder :reader rsa-signer-state-encoder)))
(define-class <rsa-verifier-state> (<digest-sigature-state> <verifier-state>)
  ((verifier :init-keyword :verifier :reader rsa-verifier-state-verifier)))

;; Those who are very lazy..
(define-method make-signer-state ((m (eql *scheme:rsa*)) . args)
  (apply make-signer-state *signature:rsa* args))

(define-method make-signer-state ((m (eql *signature:rsa*))
				  (key <rsa-private-key>)
				  :key (encoder pkcs1-emsa-pss-encode)
				       (digest *digest:sha-256*)
				  :allow-other-keys opts)
  (make <rsa-signer-state> :encoder (apply encoder opts)
	:key key :digest digest))

(define-method signer-state->signature ((state <rsa-signer-state>))
  
  (let ((cipher (make-asymmetric-cipher *scheme:rsa*
		 :encoding (lambda ignore (values #f #f))))
	(encoder (rsa-signer-state-encoder state))
	(key (signature-state-key state))
	(signing-message (digest-signature-state-signing-message! state)))
    (asymmetric-cipher-init! cipher key)
    (asymmetric-cipher-encrypt-bytevector
     cipher (encoder (digest-signature-state-digest state)
		     (rsa-private-key-modulus key) signing-message))))

(define-method make-verifier-state ((m (eql *scheme:rsa*)) . opts)
  (apply make-verifier-state *signature:rsa* opts))
(define-method make-verifier-state ((m (eql *signature:rsa*))
				    (key <rsa-public-key>)
				    :key (verifier pkcs1-emsa-pss-verify)
					 (digest *digest:sha-256*)
				    :allow-other-keys opts)
  (make <rsa-verifier-state> :verifier (apply verifier opts)
	:key key :digest digest))

(define-method verifier-state-verify-message ((state <rsa-verifier-state>)
					      (sigature <bytevector>))
  (let ((cipher (make-asymmetric-cipher *scheme:rsa*
		 :encoding (lambda ignore (values #f #f))))
	(verifier (rsa-verifier-state-verifier state))
	(key (signature-state-key state))
	(signing-message (digest-signature-state-signing-message! state)))
    (asymmetric-cipher-init! cipher key)
    (let ((EM (asymmetric-cipher-decrypt-bytevector cipher sigature)))
      (verifier (digest-signature-state-digest state)
		(rsa-public-key-modulus key) signing-message EM))))

;; EMSA PSS ENCODE
(define default-prng (secure-random-generator *prng:chacha20*))
(define (default-salt digest)
  (random-generator-read-random-bytes default-prng
				      (digest-descriptor-digest-size digest)))

(define (pkcs1-emsa-pss-encode
	 :key (given-salt :salt #f)
	      (mgf mgf-1)
	      (given-mgf-digest :mgf-digest #f)
	 :allow-other-keys)
  (lambda (digest modulus m)
    (define digest-size (digest-descriptor-digest-size digest))
    (define md (make-message-digest digest))
    (define salt (or given-salt (default-salt digest)))
    (define salt-len (bytevector-length salt))
    (define mgf-digest (or given-mgf-digest digest))
    (define em-bits (- (bitwise-length modulus) 1))
    (define em-len (div (+ em-bits 7) 8))
    (when (< em-len (+ digest-size salt-len 2))
      (assertion-violation 'pkcs1-emsa-pss-encode
			   "Intended encoded message length too short"))

    (let ((m-dash (make-bytevector (+ digest-size salt-len 8) 0)))
      ;; M' = (0x)00 00 00 00 00 00 00 00 || mHash || salt
      (bytevector-copy! m 0 m-dash 8 digest-size)
      (bytevector-copy! salt 0 m-dash (+ 8 digest-size) salt-len)
      (let* ((H (digest-message md m-dash))
	     (PS-len (- em-len salt-len digest-size 2))
	     (PS (make-bytevector PS-len 0))
	     (DB (make-bytevector (+ PS-len salt-len 1) #x01)))
	(bytevector-copy! PS 0 DB 0 PS-len)
	(bytevector-copy! salt 0 DB (+ PS-len 1) salt-len)
	(let* ((db-mask (mgf H (- em-len digest-size 1) mgf-digest))
	       (masked-db (bytevector-xor DB db-mask))
	       (bit-mask (bitwise-arithmetic-shift-right
			  #xFF (- (* em-len 8) em-bits))))
	  (bytevector-u8-set! masked-db 0
			      (bitwise-and (bytevector-u8-ref masked-db 0)
					   bit-mask))
	  (let* ((m-len (bytevector-length masked-db))
		 (h-len (bytevector-length H))
		 (EM (make-bytevector (+ m-len h-len 1) #xBC)))
	    (bytevector-copy! masked-db 0 EM 0 m-len)
	    (bytevector-copy! H 0 EM m-len h-len)
	    EM))))))

(define (pkcs1-emsa-pss-verify
	 :key (given-salt-len :salt-len #f)
	      (mgf mgf-1)
	      (given-mgf-digest :mgf-digest #f)
	 :allow-other-keys)
  (lambda (digest modulus m EM)
    (define digest-size (digest-descriptor-digest-size digest))
    (define md (make-message-digest digest))
    (define salt-len (or given-salt-len
			 (digest-descriptor-digest-size digest)))
    (define mgf-digest (or given-mgf-digest digest))
    (define (check-zero bv limit)
      (let loop ((i 0) (ok? #t))
	(if (= i limit)
	    ok?
	    (loop (+ i 1) (and (zero? (bytevector-u8-ref bv i)) ok?)))))
    (define em-bits (- (bitwise-length modulus) 1))
    (define em-len (div (+ em-bits 7) 8))
    
    ;; we do entire step here to prevent oracle attack
    (let* ((mask-length (- em-len digest-size 1))
	   (masked-db (make-bytevector mask-length 0))
	   (H (make-bytevector digest-size 0))
	   (bit-mask (bitwise-arithmetic-shift-right
		      #xFF (- (* 8 em-len) em-bits))))
      (bytevector-copy! EM 0 masked-db 0 mask-length)
      (bytevector-copy! EM mask-length H 0 digest-size)
      (let* ((db-mask (mgf H mask-length mgf-digest))
	      ;; we need masked-db to check at the last step
	     (DB (bytevector-xor masked-db db-mask))
	     (limit2 (- em-len digest-size salt-len 2)))
	(bytevector-u8-set! DB 0
			    (bitwise-and (bytevector-u8-ref DB 0) bit-mask))
	(let ((check0 (check-zero DB limit2))
	      (check1 (= #x01 (bytevector-u8-ref DB limit2)))
	      (m-dash (make-bytevector (+ 8 digest-size salt-len) 0)))
	  (bytevector-copy! m 0 m-dash 8 digest-size)
	  (bytevector-copy! DB (- (bytevector-length DB) salt-len)
			    m-dash (+ 8 digest-size) salt-len)
	  (let ((h-dash (digest-message md m-dash)))
	    (and (safe-bytevector=? H h-dash) ;; longest, do it first
		 (not (< em-len (+ digest-size salt-len 2)))
		 (= #xBC (bytevector-u8-ref EM (- (bytevector-length EM) 1)))
		 (zero? (bitwise-and (bytevector-u8-ref masked-db 0)
				     (bitwise-not bit-mask)))
		 check0
		 check1)))))))

(define (pkcs1-emsa-v1.5-encode :allow-other-keys)
  (lambda (digest modulus m)
    (define oid (digest-descriptor-oid digest))
    (define md (make-message-digest digest))
    (let* ((em-len (div (+ (bitwise-length modulus) 7) 8))
	   (digest (der-sequence
		    (der-sequence
		     (oid-string->der-object-identifier oid)
		     (make-der-null))
		    (bytevector->der-octet-string m)))
	   (T (asn1-encodable->bytevector digest))
	   (t-len (bytevector-length T)))
      (when (< em-len (+ t-len 11))
	(error 'pkcs1-emsa-v1.5-encode
	       "Intended encoded message length too short"))
      (let* ((PS-len (- em-len t-len 3))
	     ;; Initialize with PS value
	     (EM (make-bytevector (+ PS-len 3 t-len) #xFF)))
	(bytevector-u8-set! EM 0 #x00)
	(bytevector-u8-set! EM 1 #x01)
	(bytevector-u8-set! EM (+ PS-len 2) #x00)
	(bytevector-copy! T 0 EM (+ PS-len 3) t-len) 
	EM))))

(define (pkcs1-emsa-v1.5-verify . opts)
  (define encode (pkcs1-emsa-v1.5-encode))
  (lambda (digest modulus m S)
    (let ((EM (encode digest modulus m)))
      (safe-bytevector=? EM S))))
)
