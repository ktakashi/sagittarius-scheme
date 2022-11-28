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
	    (sagittarius crypto pkcs modules pfx)
	    (sagittarius crypto pkcs modules cms)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto digests)
	    (sagittarius crypto kdfs)
	    (sagittarius crypto mac)
	    (sagittarius crypto secure))

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
  (read-pkcs12-keystore (open-bytevector-input-port bv)  password))

;; internal
(define (read-pfx in)
  (asn1-object->asn1-encodable <pfx> (read-asn1-object in)))
(define (pfx->pkcs12-keystore pfx password)
  (let ((mac-descriptor (verify-mac pfx password)))
    ;; TODO load keys and certificates
    (make <pkcs12-keystore> :mac-descriptor mac-descriptor)))

(define (verify-mac pfx password)
  (define (aid->md aid)
    (define oid (der-object-identifier->oid-string
		 (algorithm-identifier-algorithm aid)))
    (or (oid->digest-descriptor oid)
	(assertion-violation 'verify-mac "Unsupported digest" oid)))
  ;; must be data content :)
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
    (make-pkcs12-mac-descriptor md c)))

(define (compute-mac md data password salt c)
  (let* ((mac-key (derive-mac-key md password salt c))
	 (hmac (make-mac *mac:hmac* mac-key :digest md)))
    (generate-mac hmac data)))

(define (derive-mac-key md password salt c)
  (pkcs12-derive-mac md password salt c (digest-descriptor-digest-size md)))

)
