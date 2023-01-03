;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/keystore.scm - PKCS#12 keystore
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
(library (sagittarius crypto pkcs keystore)
    (export pkcs12-keystore? <pkcs12-keystore>
	    make-pkcs12-keystore
	    (rename (pkcs12-keystore-private-keys-api
		     pkcs12-keystore-private-keys)
		    (pkcs12-keystore-encrypted-private-keys-api
		     pkcs12-keystore-encrypted-private-keys)
		    (pkcs12-keystore-certificates-api
		     pkcs12-keystore-certificates)
		    (pkcs12-keystore-crls-api
		     pkcs12-keystore-crls)
		    (pkcs12-keystore-secret-keys-api
		     pkcs12-keystore-secret-keys)
		    (pkcs12-keystore-safe-contents-api
		     pkcs12-keystore-safe-contents)
		    (pkcs12-keystore-friendly-names-api
		     pkcs12-keystore-friendly-names))
	    pkcs12-keystore-integrity-descriptor
	    pkcs12-keystore-integrity-descriptor-set!
	    pkcs12-keystore-privacy-descriptor
	    pkcs12-keystore-privacy-descriptor-set!
	    pkcs12-keystore-privacy-entries
	    pkcs12-keystore-privacy-entries-set!
	    pkcs12-keystore-prng
	    
	    read-pkcs12-keystore
	    bytevector->pkcs12-keystore
	    pkcs12-keystore->bytevector
	    write-pkcs12-keystore

	    pkcs12-friendly-name=?
	    pkcs12-friendly-name-pred
	    
	    pkcs12-local-key-id=?
	    pkcs12-local-key-id-pred
	    
	    pkcs12-entry-type
	    pkcs12-entry-types

	    pkcs12-keystore-entry-types
	    pkcs12-keystore-entry-type->entry-list
	    
	    pkcs12-keystore-find-secret-key
	    pkcs12-keystore-find-crl
	    pkcs12-keystore-find-certificate
	    pkcs12-keystore-find-encrypted-private-key
	    pkcs12-keystore-find-private-key

	    pkcs12-keystore-filter-secret-key
	    pkcs12-keystore-filter-crl
	    pkcs12-keystore-filter-certificate
	    pkcs12-keystore-filter-encrypted-private-key
	    pkcs12-keystore-filter-private-key

	    make-pkcs9-friendly-name-attribute
	    make-pkcs9-local-key-id-attribute
	    
	    pkcs12-keystore-add-secret-key!
	    pkcs12-keystore-add-crl!
	    pkcs12-keystore-add-certificate!
	    pkcs12-keystore-add-encrypted-private-key!
	    pkcs12-keystore-add-private-key!

	    pkcs12-keystore-upsert-secret-key!
	    pkcs12-keystore-upsert-crl!
	    pkcs12-keystore-upsert-certificate!
	    pkcs12-keystore-upsert-encrypted-private-key!
	    pkcs12-keystore-upsert-private-key!

	    pkcs12-keystore-remove-secret-key!
	    pkcs12-keystore-remove-crl!
	    pkcs12-keystore-remove-certificate!
	    pkcs12-keystore-remove-encrypted-private-key!
	    pkcs12-keystore-remove-private-key!
	    pkcs12-keystore-remove-entry!

	    ;; parameters
	    *pkcs12-integrity-salt-size*
	    *pkcs12-integrity-iteration-count*
	    *pkcs12-privacy-salt-size*
	    *pkcs12-privacy-iteration-count*

	    ;; Custom security levels, in case of applications which don't
	    ;; support PBES2 or SHA-256..., hope no such application exists
	    ;; anymore...
	    pkcs12-integrity-descriptor?
	    pkcs12-password-integrity-descriptor?
	    (rename (make-pkcs12-password-integrity-descriptor-api
		     digest-descriptor->pkcs12-password-integrity-descriptor))
	    pkcs12-privacy-descriptor?
	    pkcs12-password-privacy-descriptor?
	    x509-algorithm-identifier->password-privacy-descriptor

	    *pkcs12-privacy-descriptor:pbe/sha1-des3-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-des2-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-rc2-128-cbc*
	    *pkcs12-privacy-descriptor:pbe/sha1-rc2-40-cbc*
	    *pkcs12-privacy-descriptor:pbes2/aes-128-cbc-hmac-sha256*
	    *pkcs12-privacy-descriptor:pbes2/aes-192-cbc-hmac-sha256*
	    *pkcs12-privacy-descriptor:pbes2/aes-256-cbc-hmac-sha256*
	    	    
	    *pkcs12-integrity-descriptor:hmac/sha-256*
	    (rename (*pkcs12-integrity-descriptor:hmac/sha-256*
		     *pkcs12-integrity-descriptor:default*)
		    (*pkcs12-privacy-descriptor:pbes2/aes-256-cbc-hmac-sha256*
		     *pkcs12-privacy-descriptor:default*))
	    
	    ;; To make custom predicate
	    pkcs12-safe-bag?
	    pkcs12-safe-bag-attributes
	    pkcs12-safe-bag-friendly-name
	    pkcs12-safe-bag-local-key-id

	    ;; For my lazieness
	    pkcs12-password-privacy-descriptor->aid
	    
	    ;; For future extension, maybe...
	    ->x509-parameter-generator
	    ->key-derivation-parameter-generator
	    ->encryption-scheme-parameter-generator)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkcs modules akp)
	    (sagittarius crypto pkcs modules pfx)
	    (sagittarius crypto pkcs modules cms)
	    (sagittarius crypto pkcs modules pbes)
	    (sagittarius crypto pkcs algorithms)
	    (sagittarius crypto pkcs cms)
	    (sagittarius crypto pkcs pbes)
	    (sagittarius crypto pkcs keys)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix attributes)
	    (sagittarius crypto pkix certificate)
	    (sagittarius crypto pkix revocation)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto digests)
	    (sagittarius crypto kdfs)
	    (sagittarius crypto keys)
	    (except (sagittarius crypto mac) verify-mac)
	    (sagittarius crypto random)
	    (sagittarius crypto secure)
	    (sagittarius combinators)
	    (srfi :1 lists)
	    (srfi :39 parameters)
	    (util deque))

(define *pkcs12-integrity-salt-size* (make-parameter 32))
(define *pkcs12-privacy-salt-size* (make-parameter 32))
;; default 10000 is enough? or too much?
(define *pkcs12-integrity-iteration-count* (make-parameter 10000))
(define *pkcs12-privacy-iteration-count* (make-parameter 10000))

(define-record-type pkcs12-integrity-descriptor)
(define-record-type pkcs12-password-integrity-descriptor
  (parent pkcs12-integrity-descriptor)
  (fields mac-data-provider))
(define (pkcs12-password-integrity-descriptor->mac-data desc key data prng)
  ((pkcs12-password-integrity-descriptor-mac-data-provider desc)
   key data prng))
(define *pkcs12-integrity-descriptor:hmac/sha-256*
  (make-pkcs12-password-integrity-descriptor
   (lambda (key data prng)
     (make-mac-data *digest:sha-256*
		    key
		    (*pkcs12-integrity-salt-size*)
		    (*pkcs12-integrity-iteration-count*)
		    data prng))))
(define (make-pkcs12-password-integrity-descriptor-api (md digest-descriptor?)
	 :key (iteration (*pkcs12-integrity-iteration-count*))
	      (salt-size (*pkcs12-integrity-salt-size*)))
  (make-pkcs12-password-integrity-descriptor
   (lambda (key data prng) 
     (make-mac-data md key salt-size iteration data prng))))

(define-record-type pkcs12-privacy-descriptor)
(define-record-type pkcs12-password-privacy-descriptor
  (parent pkcs12-privacy-descriptor)
  (fields aid-provider))
(define (pkcs12-password-privacy-descriptor->aid desc prng)
  ((pkcs12-password-privacy-descriptor-aid-provider desc) prng))

(define (block-cipher-descriptor->pkcs12-privacy-descriptor descriptor
	 :key (prf *pbes:hmac/sha256*))
  (make-pkcs12-password-privacy-descriptor
   (lambda (prng)
     (let ((block-size (block-cipher-descriptor-block-length descriptor)))
       (make-pbes2-x509-algorithm-identifier
	(make-pbkdf2-x509-algorithm-identifier
	 (random-generator-read-random-bytes prng (*pkcs12-privacy-salt-size*))
	 (*pkcs12-privacy-iteration-count*)
	 :prf prf)
	(make-aes256-encryption-x509-algorithm-identifier
	 (random-generator-read-random-bytes prng block-size)))))))
(define *pkcs12-privacy-descriptor:pbes2/aes-128-cbc-hmac-sha256*
  (block-cipher-descriptor->pkcs12-privacy-descriptor *scheme:aes-128*))
(define *pkcs12-privacy-descriptor:pbes2/aes-192-cbc-hmac-sha256*
  (block-cipher-descriptor->pkcs12-privacy-descriptor *scheme:aes-192*))
(define *pkcs12-privacy-descriptor:pbes2/aes-256-cbc-hmac-sha256*
  (block-cipher-descriptor->pkcs12-privacy-descriptor *scheme:aes-256*))


(define (->pkcs12-password-privacy-descriptor oid)
  (make-pkcs12-password-privacy-descriptor
   (lambda (prng)
     (make-x509-algorithm-identifier (der-object-identifier->oid-string oid)
      (make-pkcs-pbe-parameter
       (random-generator-read-random-bytes prng (*pkcs12-privacy-salt-size*))
       (*pkcs12-privacy-iteration-count*))))))
(define *pkcs12-privacy-descriptor:pbe/sha1-des3-cbc*
  (->pkcs12-password-privacy-descriptor *pkcs12:pbe/sha1-des3-cbc*))
(define *pkcs12-privacy-descriptor:pbe/sha1-des2-cbc*
  (->pkcs12-password-privacy-descriptor *pkcs12:pbe/sha1-des2-cbc*))
(define *pkcs12-privacy-descriptor:pbe/sha1-rc2-128-cbc*
  (->pkcs12-password-privacy-descriptor *pkcs12:pbe/sha1-rc2-128-cbc*))
(define *pkcs12-privacy-descriptor:pbe/sha1-rc2-40-cbc*
  (->pkcs12-password-privacy-descriptor *pkcs12:pbe/sha1-rc2-40-cbc*))

(define-record-type pkcs12-exchange-key
  (fields integrity-key privacy-key))

(define-enumeration pkcs12-entry-type
  (private-key encrypted-private-key certificate crl secret-key safe-content)
  pkcs12-entry-types)
(define *all-entry-types* (enum-set-universe (pkcs12-entry-types)))

;; Each entries will contain pkcs12-safe-bag
(define-class <pkcs12-keystore> ()
  ((private-keys  :init-form (make-deque) :reader pkcs12-keystore-private-keys)
   (encrypted-private-keys :init-form (make-deque)
			   :reader pkcs12-keystore-encrypted-private-keys)
   (certificates  :init-form (make-deque) :reader pkcs12-keystore-certificates)
   (crls          :init-form (make-deque) :reader pkcs12-keystore-crls)
   (secret-keys   :init-form (make-deque) :reader pkcs12-keystore-secret-keys)
   (safe-contents :init-form (make-deque) :reader pkcs12-keystore-safe-contents)
   ;; for convenience
   (friendly-names :init-form (make-hashtable string-ci-hash string-ci=?)
		   :reader pkcs12-keystore-friendly-names)
   (integrity-descriptor :init-keyword :integrity-descriptor
    :init-value *pkcs12-integrity-descriptor:hmac/sha-256*
    :reader pkcs12-keystore-integrity-descriptor
    :writer pkcs12-keystore-integrity-descriptor-set!)
   (privacy-descriptor :init-keyword :privacy-descriptor
    :init-value *pkcs12-privacy-descriptor:pbes2/aes-256-cbc-hmac-sha256*
    :reader pkcs12-keystore-privacy-descriptor
    :writer pkcs12-keystore-privacy-descriptor-set!)
   (privacy-entries :init-keyword :privacy-entries
    :init-value *all-entry-types*
    :reader pkcs12-keystore-privacy-entries
    :writer pkcs12-keystore-privacy-entries-set!)
   (prng :init-form (secure-random-generator *prng:chacha20*)
	 :init-keyword :prng
	 :reader pkcs12-keystore-prng)))
(define-method write-object ((k <pkcs12-keystore>) out)
  (format out "#<pkcs12-keystore pk=~a epki=~a c=~a crl=~a sk=~a>"
	  (deque-length (pkcs12-keystore-private-keys k))
	  (deque-length (pkcs12-keystore-encrypted-private-keys k))
	  (deque-length (pkcs12-keystore-certificates k))
	  (deque-length (pkcs12-keystore-crls k))
	  (deque-length (pkcs12-keystore-secret-keys k))))
(define (pkcs12-keystore? o) (is-a? o <pkcs12-keystore>))
(define (make-pkcs12-keystore . opts) (apply make <pkcs12-keystore> opts))

(define (pkcs12-entry-type->entry-container type)
  (cond ((eq? type 'private-key) pkcs12-keystore-private-keys)
	((eq? type 'encrypted-private-key)
	 pkcs12-keystore-encrypted-private-keys)
	((eq? type 'certificate) pkcs12-keystore-certificates)
	((eq? type 'crl) pkcs12-keystore-crls)
	((eq? type 'secret-key) pkcs12-keystore-secret-keys)
	;; a bit lazy way...
	(else pkcs12-keystore-safe-contents)))

(define (pkcs12-keystore-entry-type->entry-list ks type)
  (deque->list ((pkcs12-entry-type->entry-container type) ks)))

(define (read-pkcs12-keystore exchange-key :optional (in (current-input-port)))
  (pfx->pkcs12-keystore (read-pfx in) exchange-key))
(define (bytevector->pkcs12-keystore bv exchange-key)
  (read-pkcs12-keystore exchange-key (open-bytevector-input-port bv)))

(define (pkcs12-keystore->bytevector ks exchange-key)
  (asn1-encodable->bytevector (pkcs12-keystore->pfx ks exchange-key)))

(define (write-pkcs12-keystore ks exchange-key
			       :optional (out (current-output-port)))
  (put-bytevector out (pkcs12-keystore->bytevector ks exchange-key)))

(define-syntax make-deque->list
  (syntax-rules ()
    ((_ acc get)
     (lambda ((ks pkcs12-keystore?))
       (map ($. pkcs12-safe-bag-value get) (deque->list (acc ks)))))))
	       
(define pkcs12-keystore-private-keys-api
  (make-deque->list pkcs12-keystore-private-keys values))
(define pkcs12-keystore-encrypted-private-keys-api
  (make-deque->list pkcs12-keystore-encrypted-private-keys values))
(define pkcs12-keystore-certificates-api
  (make-deque->list pkcs12-keystore-certificates pkcs12-cert-bag-value))
(define pkcs12-keystore-crls-api
  (make-deque->list pkcs12-keystore-crls pkcs12-crl-bag-value))
(define pkcs12-keystore-secret-keys-api
  (make-deque->list pkcs12-keystore-secret-keys pkcs12-secret-bag-value))
(define pkcs12-keystore-safe-contents-api
  (make-deque->list pkcs12-keystore-safe-contents values))
(define (pkcs12-keystore-friendly-names-api ks)
  ;; return a immutable copy of the friendly names hashtable
  (hashtable-copy (pkcs12-keystore-friendly-names ks)))

(define (pkcs12-keystore-entry-types ks name)
  (hashtable-ref (pkcs12-keystore-friendly-names ks) name '()))

(define (pkcs12-friendly-name=? (bag0 pkcs12-safe-bag?) (bag1 pkcs12-safe-bag?))
  (let ((fn0 (pkcs12-safe-bag-friendly-name bag0))
	(fn1 (pkcs12-safe-bag-friendly-name bag1)))
    (and fn0 fn1 (string-ci=? fn0 fn1))))

(define ((pkcs12-friendly-name-pred (name string?)) bag)
  (let ((fn (pkcs12-safe-bag-friendly-name bag)))
    (and fn (string-ci=? fn name))))

(define (pkcs12-local-key-id=? (bag0 pkcs12-safe-bag?) (bag1 pkcs12-safe-bag?))
  (let ((fn0 (pkcs12-safe-bag-local-key-id bag0))
	(fn1 (pkcs12-safe-bag-local-key-id bag1)))
    (and fn0 fn1 (bytevector=? fn0 fn1))))

(define ((pkcs12-local-key-id-pred (id bytevector?)) bag)
  (let ((fn (pkcs12-safe-bag-local-key-id bag)))
    (and fn (bytevector=? fn id))))

(define-syntax pkcs12-keystore-find
  (syntax-rules ()
    ((_ acc)
     (lambda (pred (ks pkcs12-keystore?)) (find-in-deque pred (acc ks))))))

(define-syntax optional-get
  (syntax-rules ()
    ((_ get) (lambda (v) (and v (get v))))))
(define pkcs12-keystore-find-private-key
  ($. (pkcs12-keystore-find pkcs12-keystore-private-keys)
      (optional-get pkcs12-safe-bag-value)))
(define pkcs12-keystore-find-encrypted-private-key
  ($. (pkcs12-keystore-find pkcs12-keystore-encrypted-private-keys)
      (optional-get pkcs12-safe-bag-value)))
(define pkcs12-keystore-find-certificate
  ($. (pkcs12-keystore-find pkcs12-keystore-certificates)
      (optional-get pkcs12-safe-bag-value)
      (optional-get pkcs12-cert-bag-value)))
(define pkcs12-keystore-find-crl
  ($. (pkcs12-keystore-find pkcs12-keystore-crls)
      (optional-get pkcs12-safe-bag-value)
      (optional-get pkcs12-crl-bag-value)))
(define pkcs12-keystore-find-secret-key
  ($. (pkcs12-keystore-find pkcs12-keystore-secret-keys)
      (optional-get pkcs12-safe-bag-value)
      (optional-get pkcs12-secret-bag-value)))

(define-syntax pkcs12-keystore-filter
  (syntax-rules ()
    ((_ acc get)
     (lambda (pred (ks pkcs12-keystore?))
       (map ($. pkcs12-safe-bag-value get)
	    (filter-in-deque pred (acc ks)))))))
(define pkcs12-keystore-filter-private-key
  (pkcs12-keystore-filter pkcs12-keystore-private-keys values))
(define pkcs12-keystore-filter-encrypted-private-key
  (pkcs12-keystore-filter pkcs12-keystore-encrypted-private-keys values))
(define pkcs12-keystore-filter-certificate
  (pkcs12-keystore-filter pkcs12-keystore-certificates pkcs12-cert-bag-value))
(define pkcs12-keystore-filter-crl
  (pkcs12-keystore-filter pkcs12-keystore-crls pkcs12-crl-bag-value))
(define pkcs12-keystore-filter-secret-key
  (pkcs12-keystore-filter pkcs12-keystore-secret-keys pkcs12-secret-bag-value))

(define (make-pkcs9-friendly-name-attribute (name string?))
  (make <x509-attribute> :type (sid *pkcs9:friendly-name*)
	:values (der-set (string->der-bmp-string name))))

(define (make-pkcs9-local-key-id-attribute (id bytevector?))
  (make <x509-attribute> :type (sid *pkcs9:local-key-id*)
	:values (der-set (bytevector->der-octet-string id))))

(define-syntax pkcs12-keystore-add!
  (syntax-rules ()
    ((_ pred ->safe-bag)
     (lambda (ks (obj pred)
		 :optional (attributes #f)
			   (compare pkcs12-friendly-name=?))
       (let ((bag (->safe-bag obj attributes)))
	 (store-bag ks bag compare))))))
(define (secret-key? o)
  (or (symmetric-key? o)
      (pkcs-one-asymmetric-key? o)
      (pkcs-encrypted-private-key-info? o)))
(define pkcs12-keystore-add-private-key!
  (pkcs12-keystore-add! pkcs-one-asymmetric-key?
			pkcs-one-asymmetric-key->pkcs12-safe-bag))
(define pkcs12-keystore-add-encrypted-private-key!
  (pkcs12-keystore-add! pkcs-encrypted-private-key-info?
			pkcs-encrypted-private-key-info->pkcs12-safe-bag))
(define pkcs12-keystore-add-certificate!
  (pkcs12-keystore-add! x509-certificate?
			x509-certificate->pkcs12-safe-bag))
(define pkcs12-keystore-add-crl!
  (pkcs12-keystore-add! x509-certificate-revocation-list?
			x509-certificate-revocation-list->pkcs12-safe-bag))
(define pkcs12-keystore-add-secret-key!
  (pkcs12-keystore-add! secret-key? secret-key->pkcs12-safe-bag))

(define-syntax pkcs12-keystore-upsert!
  (syntax-rules ()
    ((_ pred ->safe-bag)
     (lambda (ks (obj pred)
		 :optional (attributes #f)
			   (compare pkcs12-friendly-name=?))
       (let ((bag (->safe-bag obj attributes)))
	 (store-bag ks bag compare #t))))))
(define pkcs12-keystore-upsert-private-key!
  ($. (pkcs12-keystore-upsert! pkcs-one-asymmetric-key?
			       pkcs-one-asymmetric-key->pkcs12-safe-bag)
      (optional-get pkcs12-safe-bag-value)))
(define pkcs12-keystore-upsert-encrypted-private-key!
  ($. (pkcs12-keystore-upsert! pkcs-encrypted-private-key-info?
			       pkcs-encrypted-private-key-info->pkcs12-safe-bag)
      (optional-get pkcs12-safe-bag-value)))
(define pkcs12-keystore-upsert-certificate!
  ($. (pkcs12-keystore-upsert! x509-certificate?
			       x509-certificate->pkcs12-safe-bag)
      (optional-get pkcs12-safe-bag-value)
      (optional-get pkcs12-cert-bag-value)))
(define pkcs12-keystore-upsert-crl!
  ($. (pkcs12-keystore-upsert! x509-certificate-revocation-list?
			       x509-certificate-revocation-list->pkcs12-safe-bag)
      (optional-get pkcs12-safe-bag-value)
      (optional-get pkcs12-crl-bag-value)))
(define pkcs12-keystore-upsert-secret-key!
  ($. (pkcs12-keystore-upsert! secret-key? secret-key->pkcs12-safe-bag)
      (optional-get pkcs12-safe-bag-value)
      (optional-get pkcs12-secret-bag-value)))

(define-syntax pkcs12-keystore-remove!
  (syntax-rules ()
    ((_ type get)
     (let ((acc (pkcs12-entry-type->entry-container 'type)))
       (lambda (pred (ks pkcs12-keystore?))
	 (define names (pkcs12-keystore-friendly-names ks))
	 (define q (acc ks))
	 (define (update-friendly-names! bags)
	   (let* ((fn* (delete-duplicates!
			(filter-map pkcs12-safe-bag-friendly-name bags)
			string-ci=?))
		  (in-queue (delete-duplicates!
			     (filter-map pkcs12-safe-bag-friendly-name
					 (deque->list q))
			     string-ci=?))
		  (diff (lset-difference string-ci=? fn* in-queue)))
	     (unless (null? diff)
	       (for-each (lambda (fn)
			   (hashtable-update! names fn
					      (lambda (v) (remq 'type v)) '())
			   (when (null? (hashtable-ref names fn #f))
			     (hashtable-delete! names fn)))
			 diff))
	     
	     (map ($. pkcs12-safe-bag-value get) bags)))
	 (let ((bags (remove-from-deque! pred q)))
	   (and bags (update-friendly-names! bags))))))))
		 
(define pkcs12-keystore-remove-private-key!
  (pkcs12-keystore-remove! private-key values))
(define pkcs12-keystore-remove-encrypted-private-key!
  (pkcs12-keystore-remove! encrypted-private-key values))
(define pkcs12-keystore-remove-certificate!
  (pkcs12-keystore-remove! certificate pkcs12-cert-bag-value))
(define pkcs12-keystore-remove-crl!
  (pkcs12-keystore-remove! crl pkcs12-crl-bag-value))
(define pkcs12-keystore-remove-secret-key!
  (pkcs12-keystore-remove! secret-key pkcs12-secret-bag-value))

(define (pkcs12-keystore-remove-entry! pred ks type)
  (case type
    ((private-key) (pkcs12-keystore-remove-private-key! pred ks))
    ((encrypted-private-key)
     (pkcs12-keystore-remove-encrypted-private-key! pred ks))
    ((certificate) (pkcs12-keystore-remove-certificate! pred ks))
    ((crl) (pkcs12-keystore-remove-crl! pred ks))
    ((secret-key) (pkcs12-keystore-remove-secret-key! pred ks))
    ;; TBD?
    (else #f)))

;;;; internal
;;; Read
(define (read-pfx in)
  (asn1-object->asn1-encodable <pfx> (read-asn1-object in)))
;; NOTE: not sure if we should support public key privacy mode as this
;;       assumes decryption by private key, which can only be done either
;;       RSA or ElGamal. The latter should be possible to do with elliptic
;;       curves, so it's okay?
;;  ref:
;;   - https://www.ams.org/journals/mcom/1987-48-177/S0025-5718-1987-0866109-5/
;;   - https://asecuritysite.com/encryption/go_elgamal_ecc
(define (pfx->pkcs12-keystore pfx
			      (exchange-key (or string? pkcs12-exchange-key?)))
  (if (string? exchange-key)
      (let ((key (make-pkcs12-exchange-key exchange-key exchange-key)))
	(pfx->pkcs12-keystore pfx key))
      (load-pkcs12-keystore pfx exchange-key)))

(define (load-pkcs12-keystore pfx exchange-key)
  (define auth-safe (pfx-auth-safe pfx))
  (define content-type (content-info-content-type auth-safe))
  (define (content-info->safe-bag ci)
    (define (rec ci pd)
      (define c (content-info-content ci))
      (cond ((ber-octet-string? c)
	     (values
	      (asn1-object->asn1-encodable <safe-contents>
	       (bytevector->asn1-object (ber-octet-string->bytevector c)))
	      pd))
	    ((encrypted-data? c)
	     (let ((eci (cms-encrypted-data-encrypted-content-info
			 (encrypted-data->cms-encrypted-data c))))
	       (rec
		(asn1-encodable-container-c
		 (cms-encrypted-content-info->cms-content-info eci
		  (pkcs12-exchange-key-privacy-key exchange-key)))
		(x509-algorithm-identifier->password-privacy-descriptor
		 (cms-encrypted-content-info-content-encryption-algorithm eci)))))
	  ;; enveloped data for public key encrypted
	  (else (error 'pfx->pkcs12-keystore "Unknown contentInfo" ci))))
    (rec ci #f))
  (define (content-infos->safe-bags ci*)
    (let loop ((r '()) (ci* ci*) (privacy-desc #f))
      (if (null? ci*)
	  (values (reverse! r) privacy-desc)
	  (let-values (((sb pd) (content-info->safe-bag (car ci*))))
	    (loop (cons sb r) (cdr ci*) (or privacy-desc pd))))))
    
  (let*-values (((content mac-descriptor)
		 (cond ((equal? content-type *cms:data-content-type*)
			(verify-mac pfx
			 (pkcs12-exchange-key-integrity-key exchange-key)))
		       ((equal? content-type *cms:signed-data-content-type*)
			(error 'pfx->pkcs12-keystore "Not supported yet" pfx))
		       (else
			(error 'pfx->pkcs12-keystore
			       "Unknown content type" pfx))))
		((safe-bags privacy-descriptor)
		 (content-infos->safe-bags (authenticated-safe->list content))))
    (let ((safe-bags (map safe-bag->pkcs12-safe-bag
			  (append-map safe-contents->list safe-bags)))
	  (ks (make <pkcs12-keystore> :integrity-descriptor mac-descriptor
		    ;; TODO default privacy descriptor
		    :privacy-descriptor privacy-descriptor)))
      (for-each (lambda (bag) (store-bag ks bag)) safe-bags)
      ks)))

(define (store-bag ks bag :optional (compare #f) (upsert #f))
  (define value (pkcs12-safe-bag-value bag))
  (define (acc&type v)
    (cond ((pkcs-one-asymmetric-key? v)
	   (values pkcs12-keystore-private-keys 'private-key))
	  ((pkcs-encrypted-private-key-info? v)
	   (values pkcs12-keystore-encrypted-private-keys
		   'encrypted-private-key))
	  ((pkcs12-cert-bag? v)
	   (values pkcs12-keystore-certificates 'certificate))
	  ((pkcs12-crl-bag? v)
	   (values pkcs12-keystore-crls 'crl))
	  ((pkcs12-secret-bag? v)
	   (values pkcs12-keystore-secret-keys 'secret-key))
	  ;; a bit lazy way...
	  (else (values pkcs12-keystore-safe-contents 'safe-content))))
  (define (add-with-check deque compare bag upsert)
    (cond ((not compare)
	   (deque-push! deque bag)
	   #t)
	  (upsert
	   (let ((v (remove-from-deque! (lambda (b) (compare b bag)) deque)))
	     (deque-push! deque bag)
	     ;; return only the first one, don't do crazy amount of upsert :D
	     (or (and v (car v)) #t)))
	  (else
	   (deque-push-unique! deque compare bag)
	   (any-in-deque (lambda (v) (eq? v bag)) deque))))
  (let-values (((acc type) (acc&type value)))
    (let ((added? (add-with-check (acc ks) compare bag upsert)))
      (when added?
	(cond ((pkcs12-safe-bag-friendly-name bag) =>
	       (lambda (fn)
		 (hashtable-update! (pkcs12-keystore-friendly-names ks)
				    fn
				    (lambda (v)
				      ;; duplicate friendly name
				      ;; we don't check duplication
				      ;; as RFC doesn't forbit...
				      (cond ((memq type v) v)
					    (else (cons type v))))
				    '())))))
      (if upsert (and (not (boolean? added?)) added?) added?))))

(define (verify-mac pfx password)
  (define (aid->md aid)
    (define oid (der-object-identifier->oid-string
		 (algorithm-identifier-algorithm aid)))
    (or (oid->digest-descriptor oid)
	(assertion-violation 'verify-mac "Unsupported digest" oid)))
  (define content (content-info-content (pfx-auth-safe pfx)))
  (let* ((mac-data (pfx-mac-data pfx))
	 (mac (mac-data-mac mac-data))
	 (salt (ber-octet-string->bytevector (mac-data-mac-salt mac-data)))
	 (c (der-integer->integer (mac-data-iterations mac-data)))
	 (aid (digest-info-digest-algorithm mac))
	 (digest (ber-octet-string->bytevector (digest-info-digest mac)))
	 (data  (ber-octet-string->bytevector content))
	 (md (aid->md aid)))
    (unless (safe-bytevector=? digest (compute-mac md data password salt c))
      (error 'verify-mac
       "Mac is invalid - wrong integrity password or corrupted data"))
    (values (asn1-object->asn1-encodable <authenticated-safe>
					 (bytevector->asn1-object data))
	    (make-pkcs12-password-integrity-descriptor-api md
	     :iteration c :salt-size (bytevector-length salt)))))

;;; Write
(define (pkcs12-keystore->pfx ks
			      (exchange-key (or string? pkcs12-exchange-key?)))
  (if (string? exchange-key)
      (let ((key (make-pkcs12-exchange-key exchange-key exchange-key)))
	(pkcs12-keystore->pfx ks key))
      (unload-pkcs12-keystore ks exchange-key)))

(define (unload-pkcs12-keystore (ks pkcs12-keystore?) exchange-key)
  (define privacy-entry-types (pkcs12-keystore-privacy-entries ks))
  (define no-privacy-entry-types (enum-set-complement privacy-entry-types))
  (define privacy-desc (pkcs12-keystore-privacy-descriptor ks))
  (define integrity-desc (pkcs12-keystore-integrity-descriptor ks))
  (define privacy-key (pkcs12-exchange-key-privacy-key exchange-key))
  (define integrity-key (pkcs12-exchange-key-integrity-key exchange-key))
  (define prng (pkcs12-keystore-prng ks))
  (define (safe-bags->encrypted-data sb*)
    (and (not (null? sb*))
	 (if (pkcs12-password-privacy-descriptor? privacy-desc)
	     (make-cms-encrypted-data-content-info
	      (make-cms-encrypted-data
	       (cms-content-info->cms-encrypted-content-info
		(make-cms-data-content-info
		 (asn1-encodable->bytevector
		  (make <safe-contents>
		    :elements (map pkcs12-safe-bag->safe-bag sb*))))
		(pkcs12-password-privacy-descriptor->aid privacy-desc prng)
		privacy-key)))
	     (error 'unload-pkcs12-keystore "Not yet" privacy-desc))))
  (define (safe-bags->content-info sb*)
    (and (not (null? sb*))
	 (make-cms-data-content-info
	  (asn1-encodable->bytevector
	   (make <safe-contents>
	     :elements (map pkcs12-safe-bag->safe-bag sb*))))))
  (let* ((privacy-entries
	  (safe-bags->encrypted-data
	   (append-map (lambda (get) (deque->list (get ks)))
		       (map pkcs12-entry-type->entry-container
			    (enum-set->list privacy-entry-types)))))
	 (no-privacy-entries
	  (safe-bags->content-info
	   (append-map (lambda (get) (deque->list (get ks)))
		       (map pkcs12-entry-type->entry-container
			    (enum-set->list no-privacy-entry-types)))))
	 (auth-safe (make <authenticated-safe>
		      :elements (map cms-content-info->content-info
				     (filter values (list no-privacy-entries
							  privacy-entries))))))
    (if (pkcs12-password-integrity-descriptor? integrity-desc)
	(let ((data (asn1-encodable->bytevector auth-safe)))
	  (make <pfx>
	    :version (integer->der-integer 3)
	    :auth-safe (cms-content-info->content-info
			(make-cms-data-content-info data))
	    :mac-data (pkcs12-password-integrity-descriptor->mac-data
		       integrity-desc integrity-key data prng)))
	;; SignedData
	(error 'unload-pkcs12-keystore "Not yet"))))

(define (make-mac-data md key salt-size c data prng)
  (let* ((salt (random-generator-read-random-bytes prng salt-size))
	 (digest (compute-mac md data key salt c))
	 (oid (digest-descriptor-oid md))
	 (da (make <algorithm-identifier>
	       :algorithm (oid-string->der-object-identifier oid))))
    (make <mac-data>
      :mac (make <digest-info>
	     :digest-algorithm da
	     :digest (bytevector->der-octet-string digest))
      :mac-salt (bytevector->der-octet-string salt)
      :iterations (integer->der-integer c))))

(define (compute-mac md data password salt c)
  (let* ((mac-key (derive-mac-key md password salt c))
	 (hmac (make-mac *mac:hmac* mac-key :digest md)))
    (generate-mac hmac data)))

(define (derive-mac-key md password salt c)
  (pkcs12-derive-mac md password salt c (digest-descriptor-digest-size md)))

;; cipher
(define sid der-object-identifier->oid-string)
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pkcs12:pbe/sha1-rc2-40-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pkcs12:pbe/sha1-rc2-128-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pkcs12:pbe/sha1-des2-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))
(define-method oid->x509-algorithm-parameters-types
  ((oid (equal (sid *pkcs12:pbe/sha1-des3-cbc*))))
  (values <pkcs-pbe-parameter> <pbe-parameter>))

(define-method oid->kdf ((oid (equal (sid *pkcs12:pbe/sha1-des3-cbc*))) param)
  (pbe-parameter->pkcs12-kdf param *digest:sha-1* 24))
(define-method oid->kdf ((oid (equal (sid *pkcs12:pbe/sha1-des2-cbc*))) param)
  (pbe-parameter->pkcs12-kdf param *digest:sha-1* 16))
(define-method oid->kdf ((oid (equal (sid *pkcs12:pbe/sha1-rc2-128-cbc*)))
			 param)
  (pbe-parameter->pkcs12-kdf param *digest:sha-1* 16))
(define-method oid->kdf ((oid (equal (sid *pkcs12:pbe/sha1-rc2-40-cbc*))) param)
  (pbe-parameter->pkcs12-kdf param *digest:sha-1* 5))

(define (pbe-parameter->pkcs12-kdf param digest dk-len)
  (lambda (password . ignore)
    (pkcs12-kdf digest
		password
		(pkcs-pbe-parameter-salt param)
		(pkcs-pbe-parameter-iteration-count param)
		dk-len)))

(define-method oid->cipher
  ((oid (equal (sid *pkcs12:pbe/sha1-des3-cbc*))) param)
  (make-cipher param *digest:sha-1* *scheme:des3* *mode:cbc*))
(define-method oid->cipher
  ((oid (equal (sid *pkcs12:pbe/sha1-des2-cbc*))) param)
  (make-cipher param *digest:sha-1* *scheme:des3* *mode:cbc*))
(define-method oid->cipher
  ((oid (equal (sid *pkcs12:pbe/sha1-rc2-128-cbc*))) param)
  (make-cipher param *digest:sha-1* *scheme:rc2* *mode:cbc*))
(define-method oid->cipher
  ((oid (equal (sid *pkcs12:pbe/sha1-rc2-40-cbc*))) param)
  (make-cipher param *digest:sha-1* *scheme:rc2* *mode:cbc*))

(define (make-cipher param digest scheme mode)
  (lambda (password)
    (values (make-block-cipher scheme mode)
	    (make-iv-parameter
	     (pkcs12-derive-iv digest password
	      (pkcs-pbe-parameter-salt param)
	      (pkcs-pbe-parameter-iteration-count param)
	      (block-cipher-descriptor-block-length scheme))))))

;; safe bag
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
(define-generic bag-value->pkcs12-bag-value)
(define-method bag-value->pkcs12-bag-value (v) v) ;; default for safe contents

(define friendly-name-attr? (x509-attribute-of *pkcs9:friendly-name*))
(define local-key-id-attr? (x509-attribute-of *pkcs9:local-key-id*))
(define (pkcs12-safe-bag-friendly-name attrs)
  (and attrs
       (let ((friendly-name (find friendly-name-attr? attrs)))
	 (and friendly-name
	      (asn1-string->string 
	       (car (asn1-collection->list
		     (x509-attribute-values friendly-name))))))))
(define (pkcs12-safe-bag-local-key-id attrs)
  (and attrs
       (let ((local-key-id (find local-key-id-attr? attrs)))
	 (and local-key-id
	      (der-octet-string->bytevector
	       (car (asn1-collection->list
		     (x509-attribute-values local-key-id))))))))

(define-class <pkcs12-safe-bag> (<asn1-encodable-container>)
  ((id :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ safe-bag-bag-id asn1-encodable-container-c)
	       der-object-identifier->oid-string)
    :reader pkcs12-safe-bag-id)
   (value :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ safe-bag-bag-value asn1-encodable-container-c)
	       bag-value->pkcs12-bag-value)
    :reader pkcs12-safe-bag-value)
   (attributes :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ safe-bag-bag-attributes asn1-encodable-container-c)
	       (lambda (v) (and v (attributes->x509-attributes v))))
    :reader pkcs12-safe-bag-attributes)
   (friendly-name :allocation :virtual :cached #t
    :slot-ref (make-slot-ref (lambda (o) (slot-ref o 'attributes))
			     pkcs12-safe-bag-friendly-name)
    :reader pkcs12-safe-bag-friendly-name)
   (local-key-id :allocation :virtual :cached #t
    :slot-ref (make-slot-ref (lambda (o) (slot-ref o 'attributes))
			     pkcs12-safe-bag-local-key-id)
    :reader pkcs12-safe-bag-local-key-id)))
(define (pkcs12-safe-bag? o) (is-a? o <pkcs12-safe-bag>))
(define (safe-bag->pkcs12-safe-bag bag)
  (make <pkcs12-safe-bag> :c bag))
(define (safe-bag->bytevector bag)
  (asn1-encodable->bytevector (pkcs12-safe-bag->safe-bag bag)))
(define (pkcs12-safe-bag->safe-bag bag)
  (asn1-encodable-container-c bag))

(define-method bag-value->pkcs12-bag-value ((v <one-asymmetric-key>))
  (one-asymmetric-key->pkcs-one-asymmetric-key v))
(define-method bag-value->pkcs12-bag-value ((v <encrypted-private-key-info>))
  (encrypted-private-key-info->pkcs-encrypted-private-key-info v))

(define-class <pkcs12-cert-bag> (<asn1-encodable-container>)
  ((id :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ cert-bag-cert-id asn1-encodable-container-c)
	       der-object-identifier->oid-string)
    :reader pkcs12-cert-bag-id)
   (value :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ cert-bag-cert-value asn1-encodable-container-c)
	       values)
    :reader pkcs12-cert-bag-value)))
(define (pkcs12-cert-bag? o) (is-a? o <pkcs12-cert-bag>))
(define-method bag-value->pkcs12-bag-value ((v <cert-bag>))
  (make <pkcs12-cert-bag> :c v))

(define-class <pkcs12-crl-bag> (<asn1-encodable-container>)
  ((id :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ crl-bag-crl-id asn1-encodable-container-c)
	       der-object-identifier->oid-string)
    :reader pkcs12-crl-bag-id)
   (value :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ crl-bag-crl-value asn1-encodable-container-c)
	       values)
    :reader pkcs12-crl-bag-value)))
(define (pkcs12-crl-bag? o) (is-a? o <pkcs12-crl-bag>))
(define-method bag-value->pkcs12-bag-value ((v <crl-bag>))
  (make <pkcs12-crl-bag> :c v))

(define-generic secret-bag-value->value)
(define-class <pkcs12-secret-bag> (<asn1-encodable-container>)
  ((id :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ secret-bag-secret-type-id asn1-encodable-container-c)
	       der-object-identifier->oid-string)
    :reader pkcs12-secret-bag-id)
   (value :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ secret-bag-secret-value asn1-encodable-container-c)
	       secret-bag-value->value)
    :reader pkcs12-secret-bag-value)))
(define (pkcs12-secret-bag? o) (is-a? o <pkcs12-secret-bag>))
(define-method bag-value->pkcs12-bag-value ((v <secret-bag>))
  (make <pkcs12-secret-bag> :c v))
(define-method secret-bag-value->value ((v <one-asymmetric-key>))
  (one-asymmetric-key->pkcs-one-asymmetric-key v))
(define-method secret-bag-value->value ((v <encrypted-private-key-info>))
  (encrypted-private-key-info->pkcs-encrypted-private-key-info v))
(define-method secret-bag-value->value ((v <der-octet-string>))
  (make-symmetric-key (der-octet-string->bytevector v)))

(define (make-safe-bag id value attrs)
  (make <safe-bag>
    :bag-id id
    :bag-value value
    :bag-attributes (and attrs (x509-attributes->attributes attrs))))
(define (pkcs-one-asymmetric-key->pkcs12-safe-bag oak attributes)
  (make <pkcs12-safe-bag>
    :c (make-safe-bag *pkcs12:key-bag*
	(pkcs-one-asymmetric-key->one-asymmetric-key oak)
	attributes)))
(define (pkcs-encrypted-private-key-info->pkcs12-safe-bag epki attrs)
  (make <pkcs12-safe-bag>
    :c (make-safe-bag *pkcs12:pkcs8-shrouded-key-bag*
	(pkcs-encrypted-private-key-info->encrypted-private-key-info epki)
	 attrs)))
(define (x509-certificate->pkcs12-safe-bag cert attrs)
  (make <pkcs12-safe-bag>
    :c (make-safe-bag *pkcs12:cert-bag*
	 (make <cert-bag> 
	   :cert-id *pkcs12:x509-certificate*
	   :cert-value (bytevector->der-octet-string (x509-certificate->bytevector cert)))
	 attrs)))

(define (x509-certificate-revocation-list->pkcs12-safe-bag crl attrs)
  (make <pkcs12-safe-bag>
    :c (make-safe-bag *pkcs12:crl-bag*
	 (make <crl-bag> 
	   :crl-id *pkcs12:x509-crl*
	   :crl-value (bytevector->der-octet-string (x509-certificate-revocation-list->bytevector crl)))
	 attrs)))

(define (secret-key->pkcs12-safe-bag key attrs)
  (define (->der-octet-string asn1-encodable)
    (bytevector->der-octet-string (asn1-encodable->bytevector asn1-encodable)))
  (define (->id&values key)
    (cond ((symmetric-key? key)
	   (values *cms:data-content-type*
		   (bytevector->der-octet-string (symmetric-key-value key))))
	  ((pkcs-one-asymmetric-key? key)
	   (values *pkcs12:key-bag*
		   (->der-octet-string
		    (pkcs-one-asymmetric-key->one-asymmetric-key key))))
	  ((pkcs-encrypted-private-key-info? key)
	   (values *pkcs12:pkcs8-shrouded-key-bag*
		   (->der-octet-string
		    (pkcs-encrypted-private-key-info->encrypted-private-key-info key))))
	  (else (error 'secret-key->pkcs12-safe-bag "Shouldn't happen"))))
  (let-values (((id value) (->id&values key)))
    (make <pkcs12-safe-bag>
      :c (make-safe-bag *pkcs12:secret-bag*
	  (make <secret-bag> :secret-type-id id :secret-value value)
	  attrs))))

(define-generic ->x509-parameter-generator)
(define-method ->x509-parameter-generator ((p <pkcs-pbe-parameter>))
  (lambda (prng)
    (make-pkcs-pbe-parameter
     (random-generator-read-random-bytes prng (*pkcs12-privacy-salt-size*))
     (*pkcs12-privacy-iteration-count*))))

;; damn...
(define-generic ->key-derivation-parameter-generator)
(define-method ->key-derivation-parameter-generator ((p <pkcs-pbkdf2-params>))
  (let ((prf (pkcs-pbkdf2-params-prf p))
	(dk-len (pkcs-pbkdf2-params-key-length p)))
    (lambda (prng)
      (make-pkcs-pbkdf2-params
       (random-generator-read-random-bytes prng (*pkcs12-privacy-salt-size*))
       (*pkcs12-privacy-iteration-count*)
       :prf prf :key-length dk-len))))
(define-generic ->encryption-scheme-parameter-generator)
(define-method ->encryption-scheme-parameter-generator
  (oid (iv <der-octet-string>))
  (let-values (((scheme mode) (oid->encryption-scheme oid)))
    (let ((size (block-cipher-descriptor-block-length scheme)))
      (lambda (prng)
	(bytevector->der-octet-string
	 (random-generator-read-random-bytes prng size))))))
(define-method ->encryption-scheme-parameter-generator
  (oid (p <pkcs-rc5-cbc-parameter>))
  (let ((rounds  (pkcs-rc5-cbc-parameter-rounds p))
	(block-size (pkcs-rc5-cbc-parameter-block-size-in-bits p))
	(iv-len (cond ((pkcs-rc5-cbc-parameter-iv p) => bytevector-length)
		      (else #f))))
    (lambda (prng)
      (make-pkcs-rc5-cbc-parameter rounds block-size
       (and iv-len (random-generator-read-random-bytes prng iv-len))))))
(define-method ->encryption-scheme-parameter-generator
  (oid (p <pkcs-rc2-cbc-parameter>))
  (let ((version (pkcs-rc2-cbc-parameter-version p))
	(iv-len (bytevector-length (pkcs-rc2-cbc-parameter-iv p))))
    (lambda (prng)
      (make-pkcs-rc2-cbc-parameter version
       (and iv-len (random-generator-read-random-bytes prng iv-len))))))

(define-method ->x509-parameter-generator ((p <pkcs-pbes2-params>))
  (let* ((kdf-func (pkcs-pbes2-params-key-derivation-func p))
	 (enc-func (pkcs-pbes2-params-encryption-scheme p))
	 (kdf-oid (x509-algorithm-identifier-oid kdf-func))
	 (kdf-pgen (->key-derivation-parameter-generator
		    (x509-algorithm-identifier-parameters kdf-func)))
	 (enc-oid (x509-algorithm-identifier-oid enc-func))
	 (enc-pgen (->encryption-scheme-parameter-generator enc-oid
		    (x509-algorithm-identifier-parameters enc-func))))
    (lambda (prng)
      (make-pkcs-pbes2-params
       (make-x509-algorithm-identifier kdf-oid (kdf-pgen prng))
       (make-x509-algorithm-identifier enc-oid (enc-pgen prng))))))
  
(define (x509-algorithm-identifier->password-privacy-descriptor
	 (x509-aid x509-algorithm-identifier?))
  (let ((oid (x509-algorithm-identifier-oid x509-aid))
	(pgen (->x509-parameter-generator
	       (x509-algorithm-identifier-parameters x509-aid))))
    (make-pkcs12-password-privacy-descriptor
     (lambda (prng)
       (make-x509-algorithm-identifier oid (pgen prng))))))
)
