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
	    ;; These accessors shouldn't be used directly but exporting them
	    ;; for future extensionn maybe...
	    pkcs12-keystore-private-keys
	    pkcs12-keystore-certificates
	    pkcs12-keystore-crls
	    pkcs12-keystore-secret-keys
	    pkcs12-keystore-safe-contents
	    pkcs12-keystore-integrity-descriptor
	    pkcs12-keystore-integrity-descriptor-set!
	    (rename (pkcs12-keystore-friendly-names-api
		     pkcs12-keystore-friendly-names))
	    read-pkcs12-keystore
	    bytevector->pkcs12-keystore

	    pkcs12-keystore-find-secret-key
	    pkcs12-keystore-find-crl
	    pkcs12-keystore-find-certificate
	    pkcs12-keystore-find-private-key
	    
	    pkcs12-friendly-name=?)
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
	    (sagittarius crypto ciphers)
	    (sagittarius crypto digests)
	    (sagittarius crypto kdfs)
	    (sagittarius crypto mac)
	    (sagittarius crypto secure)
	    (sagittarius combinators)
	    (srfi :1 lists)
	    (util deque))

(define-record-type pkcs12-integrity-descriptor)
(define-record-type pkcs12-mac-descriptor
  (parent pkcs12-integrity-descriptor)
  (fields md iteration))

(define-record-type pkcs12-exchange-key
  (fields integrity-key privacy-key))

;; Each entries will contain pkcs12-safe-bag
(define-class <pkcs12-keystore> ()
  ((private-keys  :init-form (make-deque) :reader pkcs12-keystore-private-keys)
   (certificates  :init-form (make-deque) :reader pkcs12-keystore-certificates)
   (crls          :init-form (make-deque) :reader pkcs12-keystore-crls)
   (secret-keys   :init-form (make-deque) :reader pkcs12-keystore-secret-keys)
   (safe-contents :init-form (make-deque) :reader pkcs12-keystore-safe-contents)
   ;; for convenient
   (friendly-names :init-form (make-deque)
		   :reader pkcs12-keystore-friendly-names)
   (integrity-descriptor :init-keyword :integrity-descriptor
		   :reader pkcs12-keystore-integrity-descriptor
		   :writer pkcs12-keystore-integrity-descriptor-set!)))
(define-method write-object ((k <pkcs12-keystore>) out)
  (format out "#<pkcs12-keystore pk=~a c=~a crl=~a sk=~a>"
	  (deque-length (pkcs12-keystore-private-keys k))
	  (deque-length (pkcs12-keystore-certificates k))
	  (deque-length (pkcs12-keystore-crls k))
	  (deque-length (pkcs12-keystore-secret-keys k))))
(define (pkcs12-keystore? o) (is-a? o <pkcs12-keystore>))
(define pkcs12-keystore-friendly-names-api
  ($. pkcs12-keystore-friendly-names deque->list))

(define (read-pkcs12-keystore exchange-key :optional (in (current-input-port)))
  (pfx->pkcs12-keystore (read-pfx in) exchange-key))
(define (bytevector->pkcs12-keystore bv exchange-key)
  (read-pkcs12-keystore (open-bytevector-input-port bv) exchange-key))

(define ((pkcs12-friendly-name=? (name string?)) bag)
  (let ((fn (pkcs12-safe-bag-friendly-name bag)))
    (and fn (string-ci=? fn name))))
(define-syntax pkcs12-keystore-find
  (syntax-rules ()
    ((_ acc) (lambda (pred ks) (find-in-deque pred (acc ks))))))

(define-syntax optional-get
  (syntax-rules ()
    ((_ get) (lambda (v) (and v (get v))))))
(define pkcs12-keystore-find-private-key
  ($. (pkcs12-keystore-find pkcs12-keystore-private-keys)
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

;; internal
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
    (define c (content-info-content ci))
    (cond ((der-octet-string? c)
	   (asn1-object->asn1-encodable <safe-contents>
	    (bytevector->asn1-object (der-octet-string->bytevector c))))
	  ((encrypted-data? c)
	   (content-info->safe-bag
	    (asn1-encodable-container-c
	     (cms-encrypted-content-info->cms-content-info
	      (cms-encrypted-data-encrypted-content-info
	       (encrypted-data->cms-encrypted-data c))
	      (pkcs12-exchange-key-privacy-key exchange-key)))))
	  ;; enveloped data for public key encrypted
	  (else (error 'pfx->pkcs12-keystore "Unknown contentInfo" ci))))
  (let-values (((content mac-descriptor)
		(cond ((equal? content-type *cms:data-content-type*)
		       (verify-mac pfx
			(pkcs12-exchange-key-integrity-key exchange-key)))
		      ((equal? content-type *cms:signed-data-content-type*)
		       (error 'pfx->pkcs12-keystore "Not supported yet" pfx))
		      (else
		       (error 'pfx->pkcs12-keystore
			      "Unknown content type" pfx)))))
    (let ((safe-bags
	   (map safe-bag->pkcs12-safe-bag
		(append-map safe-contents->list
			    (map content-info->safe-bag
				 (authenticated-safe->list content)))))
	  (ks (make <pkcs12-keystore> :integrity-descriptor mac-descriptor)))
      (for-each (lambda (bag) (store-bag ks bag)) safe-bags)
      ks)))

(define (store-bag ks bag)
  (define value (pkcs12-safe-bag-value bag))
  (define (acc v)
    (cond ((pkcs-one-asymmetric-key? v) pkcs12-keystore-private-keys)
	  ((pkcs-encrypted-private-key-info? v) pkcs12-keystore-private-keys)
	  ((pkcs12-cert-bag? v) pkcs12-keystore-certificates)
	  ((pkcs12-crl-bag? v) pkcs12-keystore-crls)
	  ((pkcs12-secret-bag? v) pkcs12-keystore-secret-keys)
	  ;; a bit lazy way...
	  (else pkcs12-keystore-safe-contents)))
  (deque-push! ((acc value) ks) bag)
  (let ((fn (pkcs12-safe-bag-friendly-name bag)))
    (and fn (deque-push-unique!
	     (pkcs12-keystore-friendly-names ks) string-ci=? fn))))

(define (verify-mac pfx password)
  (define (aid->md aid)
    (define oid (der-object-identifier->oid-string
		 (algorithm-identifier-algorithm aid)))
    (or (oid->digest-descriptor oid)
	(assertion-violation 'verify-mac "Unsupported digest" oid)))
  (define content (content-info-content (pfx-auth-safe pfx)))
  (let* ((mac-data (pfx-mac-data pfx))
	 (mac (mac-data-mac mac-data))
	 (salt (der-octet-string->bytevector (mac-data-mac-salt mac-data)))
	 (c (der-integer->integer (mac-data-iterations mac-data)))
	 (aid (digest-info-digest-algorithm mac))
	 (digest (der-octet-string->bytevector (digest-info-digest mac)))
	 (data  (der-octet-string->bytevector content))
	 (md (aid->md aid)))
    (unless (safe-bytevector=? digest (compute-mac md data password salt c))
      (error 'verify-mac
       "Mac is invalid - wrong integrity password or corrupted data"))
    (values (asn1-object->asn1-encodable <authenticated-safe>
					 (bytevector->asn1-object data))
	    (make-pkcs12-mac-descriptor md c))))

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
    (values (make-symmetric-cipher scheme mode)
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
  (let ((friendly-name (find friendly-name-attr? attrs)))
    (and friendly-name
	 (asn1-string->string 
	  (car (asn1-collection->list
		(x509-attribute-values friendly-name)))))))
(define (pkcs12-safe-bag-local-key-id attrs)
  (let ((local-key-id (find local-key-id-attr? attrs)))
    (and local-key-id
	 (der-octet-string->bytevector
	  (car (asn1-collection->list
		(x509-attribute-values local-key-id)))))))

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
	       attributes->x509-attributes)
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
(define-method secret-bag-value->value ((v <encrypted-private-key-info>))
  (encrypted-private-key-info->pkcs-encrypted-private-key-info v))
)
