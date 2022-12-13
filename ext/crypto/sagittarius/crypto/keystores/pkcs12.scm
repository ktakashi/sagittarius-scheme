;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keystores/pkcs12.scm - PKCS#12 Keystores
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

;; aggregation library for convenience
#!nounbound
(library (sagittarius crypto keystores pkcs12)
    (export pkcs12-keystore? <pkcs12-keystore>
	    make-pkcs12-keystore
	    read-pkcs12-keystore
	    bytevector->pkcs12-keystore
	    pkcs12-keystore->bytevector
	    write-pkcs12-keystore

	    *pkcs12-integrity-descriptor:hmac/sha-256*

	    *pkcs12-privacy-descriptor:pbe/sha1-des3-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-des2-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-rc2-128-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-rc2-40-cbc*
	    *pkcs12-privacy-descriptor:pbes2/aes-256-cbc-hmac-sha256*

	    (rename (*pkcs12-privacy-descriptor:pbe/sha1-des3-cbc*
		     *pkcs12-key-encryption-descriptor:pbe/sha1-des3-cbc*)
		    (*pkcs12-privacy-descriptor:pbe/sha1-des2-cbc*
		     *pkcs12-key-encryption-descriptor:pbe/sha1-des2-cbc*)
		    (*pkcs12-privacy-descriptor:pbe/sha1-rc2-128-cbc*
		     *pkcs12-key-encryption-descriptor:pbe/sha1-rc2-128-cbc*)
		    (*pkcs12-privacy-descriptor:pbe/sha1-rc2-40-cbc*
		     *pkcs12-key-encryption-descriptor:pbe/sha1-rc2-40-cbc*)
		    (*pkcs12-privacy-descriptor:pbes2/aes-256-cbc-hmac-sha256*
		     *pkcs12-key-encryption-descriptor:pbes2/aes-256-cbc-hmac-sha256*))

	    pkcs12-keystore-friendly-names
	    pkcs12-keystore-entry-types
	    
	    pkcs12-keystore-private-key-ref
	    pkcs12-keystore-private-key-set!

	    pkcs12-keystore-certificate-ref
	    pkcs12-keystore-certificate-set!
	    pkcs12-keystore-certificate-chain-ref
	    
	    pkcs12-keystore-delete-entry!
	    pkcs12-keystore-delete-all-entries!)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys)
	    (sagittarius crypto x509)
	    (sagittarius crypto digests)
	    (sagittarius crypto pkcs keys)
	    (sagittarius crypto pkcs keystore)
	    (sagittarius crypto pkix keys)
	    (srfi :19 time))
(define (list-of pred) (lambda (l) (for-each pred l)))

;; Making users not to care about how complicated PKCS#12 is :D
(define (pkcs12-keystore-private-key-ref
	 (ks pkcs12-keystore?) (alias string?) (password string?))
  (define p (pkcs12-friendly-name-pred alias))
  (cond ((pkcs12-keystore-find-encrypted-private-key p ks) =>
	 (lambda (epki)
	   (pkcs-one-asymmetric-key-private-key
	    (pkcs-encrypted-private-key-info->pkcs-one-asymmetric-key epki
	     password))))
	(else #f)))

(define (pkcs12-keystore-private-key-set!
	 (ks pkcs12-keystore?) (alias string?) (key private-key?)
	 (password string?) (certs (list-of x509-certificate?))
	 :optional (encryption-algorithm *pkcs12-privacy-descriptor:pbes2/aes-256-cbc-hmac-sha256*))
  (define (time->nano time)
    (let ((s (time-second time))
	  (n (time-nanosecond time)))
      (+ (* s 1000000000) n)))
  (define (set-cert! cert)
    (let ((at (list (make-pkcs9-local-key-id-attribute (cert-id cert)))))
      (pkcs12-keystore-upsert-certificate! ks cert at pkcs12-local-key-id=?)))
  (define local-key-id
    (string->utf8
     (string-append "Time " (number->string (time->nano (current-time))))))
  (define attrs (list (make-pkcs9-friendly-name-attribute alias)
		      (make-pkcs9-local-key-id-attribute local-key-id)))
  
  (let ((aid (pkcs12-password-privacy-descriptor->aid
	      encryption-algorithm (pkcs12-keystore-prng ks)))
	(oak (private-key->pkcs-one-asymmetric-key key)))
    (pkcs12-keystore-upsert-encrypted-private-key! ks
     (pkcs-one-asymmetric-key->pkcs-encrypted-private-key-info oak aid password)
     attrs))
  (unless (null? certs)
    ;; set certificate
    (pkcs12-keystore-upsert-certificate! ks (car certs) attrs)
    ;; set chain
    (for-each set-cert! (cdr certs)))
  (undefined))

(define (pkcs12-keystore-certificate-ref (ks pkcs12-keystore?) (alias string?))
  (pkcs12-keystore-find-certificate (pkcs12-friendly-name-pred alias) ks))

;; let's support JVM's trusted cert OID, which is not even listed on OID ref...
(define *java-trusted-certificate-attribute*
  (make-x509-attribute "2.16.840.1.113894.746875.1.1"
		       (oid-string->der-object-identifier "2.5.29.37.0")))
(define (pkcs12-keystore-certificate-set!
	 (ks pkcs12-keystore?) (alias string?) (cert x509-certificate?))
  (define attrs (list (make-pkcs9-friendly-name-attribute alias)
		      (make-pkcs9-local-key-id-attribute (cert-id cert))
		      *java-trusted-certificate-attribute*))
  (pkcs12-keystore-upsert-certificate! ks cert attrs))

(define (pkcs12-keystore-certificate-chain-ref (ks pkcs12-keystore?)
					       (alias string?))
  (cond ((pkcs12-keystore-certificate-ref ks alias) =>
	 (lambda (cert)
	   (define all-certs (pkcs12-keystore-certificates ks))
	   (let loop ((certs all-certs) (r (list cert)))
	     (cond ((null? certs) (reverse! r))
		   ;; skip root ca cert, or self signed cert
		   ;; well, root ca cert is a self signed cert though :D
		   ((eq? (car r) (car certs)) (loop (cdr certs) r))
		   ((x509-certificate-signed-by? (car r) (car certs))
		    (loop all-certs (cons (car certs) r)))
		   (else (loop (cdr certs) r))))))
	(else #f)))

(define *all-entry-types*
  (enum-set->list (enum-set-universe (pkcs12-entry-types))))
(define (pkcs12-keystore-delete-entry!
	 (ks pkcs12-keystore?) (alias string?))
  (define fn-pred (pkcs12-friendly-name-pred alias))
  (define (remove pred type)
    (let ((e* (filter pred (pkcs12-keystore-entry-type->entry-list ks type))))
      (pkcs12-keystore-remove-entry! (lambda (e) (memq e e*)) ks type)
      ;; now local key id...
      (for-each (lambda (e)
		  (cond ((pkcs12-safe-bag-local-key-id e) =>
			 (lambda (lk)
			   (for-each (lambda (t)
				       (remove (pkcs12-local-key-id-pred lk) t))
				     *all-entry-types*)))))
		e*)))
  (for-each (lambda (type) (remove fn-pred type)) *all-entry-types*))

(define (pkcs12-keystore-delete-all-entries! (ks pkcs12-keystore?))
  (define (remove type)
    (pkcs12-keystore-remove-entry! (lambda (e) #t) ks type))
  (for-each (lambda (type) (remove type)) *all-entry-types*))

;; helpers
(define sha1-md (make-message-digest *digest:sha-1*))
(define (cert-id cert)
  (digest-message sha1-md
   (asn1-encodable->bytevector
    (public-key->subject-public-key-info (x509-certificate-public-key cert)))))
)
