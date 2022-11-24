;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/modules/pfx.scm - PKCS#12 modules
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

;; ref
;; - https://datatracker.ietf.org/doc/html/rfc7292
#!nounbound
(library (sagittarius crypto pkcs modules pfx)
    (export asn1-object->asn1-encodable ;; for convenience
	    bytevector->asn1-encodable
	    *pkcs9:fiendly-name*
	    *pkcs9:local-key-id*
	    *pkcs9:cert-types*
	    *pkcs9:crl-types*
	    *pkcs12:pbe/sha1-rc4-128*
	    *pkcs12:pbe/sha1-rc4-40*
	    *pkcs12:pbe/sha1-des3-cbc*
	    *pkcs12:pbe/sha1-des2-cbc
	    *pkcs12:pbe/sha1-rc2-128-cbc*
	    *pkcs12:pbe/sha1-rc2-40-cbc*

	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkcs modules cms)
	    (sagittarius crypto pkcs modules akp))
(define oid oid-string->der-object-identifier)
(define *pkcs9:fiendly-name* (oid "1.2.840.113549.1.9.0.1.20"))
(define *pkcs9:local-key-id* (oid "1.2.840.113549.1.9.0.1.21"))
(define *pkcs9:cert-types*   (oid "1.2.840.113549.1.9.0.1.22"))
(define *pkcs9:crl-types*    (oid "1.2.840.113549.1.9.0.1.23"))

(define *pkcs12:pbe/sha1-rc4-128*     (oid "1.2.840.113549.1.12.1.1"))
(define *pkcs12:pbe/sha1-rc4-40*      (oid "1.2.840.113549.1.12.1.2"))
(define *pkcs12:pbe/sha1-des3-cbc*    (oid "1.2.840.113549.1.12.1.3"))
(define *pkcs12:pbe/sha1-des2-cbc     (oid "1.2.840.113549.1.12.1.4"))
(define *pkcs12:pbe/sha1-rc2-128-cbc* (oid "1.2.840.113549.1.12.1.5"))
(define *pkcs12:pbe/sha1-rc2-40-cbc*  (oid "1.2.840.113549.1.12.1.6"))

;; we define what we need here, such as DigestInfo which only appears
;; in PKCS#7, not the other CMSs.
;; DigestInfo ::= SEQUENCE {
;;   digestAlgorithm DigestAlgorithmIdentifier,
;;   digest Digest }
;; Digest ::= OCTET STRING
(define-asn1-encodable <digest-info>
  (asn1-sequence
   ((digest-algorithm :type <algorithm-identifier>
		      :reader digest-info-digest-algorithm)
    (digest :type <der-octet-string> :reader digest-info-digest))))
(define (digest-info? o) (is-a? o  <digest-info>))

;; MacData ::= SEQUENCE {
;;     mac        DigestInfo,
;;     macSalt    OCTET STRING,
;;     iterations INTEGER DEFAULT 1
;;     -- Note: The default is for historical reasons and its use is
;;     -- deprecated.
;; }
(define-asn1-encodable <mac-data>
  (asn1-sequence
   ((mac :type <digest-info> :reader mac-data-mac)
    (mac-salt :type <der-octet-string> :reader mac-data-mac-salt)
    (iterations :type <der-integer> :optional #t :reader mac-data-iterations))))
(define (mac-data? o) (is-a? o <mac-data>))

;; PFX ::= SEQUENCE {
;;     version    INTEGER {v3(3)}(v3,...),
;;     authSafe   ContentInfo,
;;     macData    MacData OPTIONAL
;; }
(define-asn1-encodable <pfx>
  (asn1-sequence
   ((version :type <der-integer> :reader pfx-version)
    (auth-safe :type  <content-info> :reader pfx-auth-safe)
    (mac-data :type <mac-data> :optional #t :reader pfx-mac-data))))
(define (pfx? o) (is-a? o <pfx>))

;; Bags
(define *pkcs12:key-bag*                (oid "1.2.840.113549.1.12.10.1.1"))
(define *pkcs12:pkcs8-shrouded-key-bag* (oid "1.2.840.113549.1.12.10.1.2"))
(define *pkcs12:cert-bag*               (oid "1.2.840.113549.1.12.10.1.3"))
(define *pkcs12:crl-bag*                (oid "1.2.840.113549.1.12.10.1.4"))
(define *pkcs12:secret-bag*             (oid "1.2.840.113549.1.12.10.1.5"))
(define *pkcs12:safe-content-bag*       (oid "1.2.840.113549.1.12.10.1.6"))

;; AuthenticatedSafe ::= SEQUENCE OF ContentInfo
;;     -- Data if unencrypted
;;     -- EncryptedData if password-encrypted
;;     -- EnvelopedData if public key-encrypted
;; SafeContents ::= SEQUENCE OF SafeBag

;; SafeBag ::= SEQUENCE {
;;     bagId         BAG-TYPE.&id ({PKCS12BagSet}),
;;     bagValue      [0] EXPLICIT BAG-TYPE.&Type({PKCS12BagSet}{@bagId}),
;;     bagAttributes SET OF PKCS12Attribute OPTIONAL
;; }
(define-asn1-encodable <safe-bag>
  (asn1-sequence
   ((bag-id :type <algorithm-identifier> :reader safe-bag-bag-id)
    (bag-value :type <asn1-encodable> :tag 0 :explicit #t
	       :reader safe-bag-bag-value)
    (bag-attributes :type <attributes> :optional #t
		    :reader safe-bag-bag-attributes))))
(define (safe-bag? o) (is-a? o <safe-bag>))

;; -- CertBag
;; CertBag ::= SEQUENCE {
;;     certId    BAG-TYPE.&id   ({CertTypes}),
;;     certValue [0] EXPLICIT BAG-TYPE.&Type ({CertTypes}{@certId})
;; }
(define *pkcs12:x509-certificate* (oid "1.2.840.113549.1.9.0.1.22.1"))
(define *pkcs12:sdsi-certificate* (oid "1.2.840.113549.1.9.0.1.22.2"))

)
