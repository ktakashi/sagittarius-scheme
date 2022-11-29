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
	    pkcs12-keystore-mac-descriptor
	    pkcs12-keystore-mac-descriptor-set!

	    read-pkcs12-keystore)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkcs modules pfx)
	    (sagittarius crypto pkcs modules cms)
	    (sagittarius crypto pkcs modules pbes)
	    (sagittarius crypto pkcs algorithms)
	    (sagittarius crypto pkcs cms)
	    (sagittarius crypto pkcs pbes)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto digests)
	    (sagittarius crypto kdfs)
	    (sagittarius crypto mac)
	    (sagittarius crypto secure)
	    (srfi :1 lists))

(define-record-type pkcs12-mac-descriptor
  (fields md iteration))

(define (make-keystore-entries)
  (make-hashtable string-ci-hash string-ci=?))
(define-class <pkcs12-keystore> ()
  ((private-keys :init-form make-keystore-entries
		 :reader pkcs12-keystore-private-keys)
   (certificates :init-form make-keystore-entries
		 :reader pkcs12-keystore-certificates)
   (crls :init-form make-keystore-entries :reader pkcs12-keystore-crls)
   (secret-keys :init-form make-keystore-entries
		:reader pkcs12-keystore-secret-keys)
   (safe-contents :init-form make-keystore-entries
		  :reader pkcs12-keystore-safe-contents)
   (mac-descriptor :init-keyword :mac-descriptor
		   :reader pkcs12-keystore-mac-descriptor
		   :writer pkcs12-keystore-mac-descriptor-set!)))
(define (pkcs12-keystore? o) (is-a? o <pkcs12-keystore>))

(define (read-pkcs12-keystore password :optional (in (current-input-port)))
  (pfx->pkcs12-keystore (read-pfx in) password))
(define (bytevector->pkcs12-keystore bv password)
  (read-pkcs12-keystore (open-bytevector-input-port bv) password))

;; internal
(define (read-pfx in)
  (asn1-object->asn1-encodable <pfx> (read-asn1-object in)))
;; TODO the password argument should be either password or private-key
;;      the latter case is public key privacy mode
;; NOTE: not sure if we should support public key privacy mode as this
;;       assumes decryption by private key, which can only be done either
;;       RSA or ElGamal. The latter should be possible to do with elliptic
;;       curves, so it's okay?
;;  ref:
;;   - https://www.ams.org/journals/mcom/1987-48-177/S0025-5718-1987-0866109-5/
;;   - https://asecuritysite.com/encryption/go_elgamal_ecc
(define (pfx->pkcs12-keystore pfx password)
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
	      password))))
	  ;; enveloped data for public key encrypted
	  (else (error 'pfx->pkcs12-keystore "Unknown contentInfo" ci))))
  (let-values (((content mac-descriptor)
		(cond ((equal? content-type *cms:data-content-type*)
		       (verify-mac pfx password))
		      ((equal? content-type *cms:signed-data-content-type*)
		       (error 'pfx->pkcs12-keystore "Not supported yet" pfx))
		      (else
		       (error 'pfx->pkcs12-keystore
			      "Unknown content type" pfx)))))
    (let ((safe-contents (append-map safe-contents->list
			  (map content-info->safe-bag
			       (authenticated-safe->list content)))))
      #;(for-each (lambda (bag)
		  (display (safe-bag-bag-value bag)) (newline))
		safe-contents)
      (make <pkcs12-keystore> :mac-descriptor mac-descriptor))))

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
       "Mac of the ContentInfo is invalid - wrong password or corrupted data"
       pfx))
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
)
