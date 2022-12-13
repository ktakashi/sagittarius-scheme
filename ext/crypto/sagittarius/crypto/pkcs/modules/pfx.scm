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
	    *pkcs9:friendly-name*
	    *pkcs9:local-key-id*
	    ;; These are not really useful to export
	    ;; *pkcs9:cert-types*
	    ;; *pkcs9:crl-types*
	    *pkcs12:pbe/sha1-rc4-128*
	    *pkcs12:pbe/sha1-rc4-40*
	    *pkcs12:pbe/sha1-des3-cbc*
	    *pkcs12:pbe/sha1-des2-cbc*
	    *pkcs12:pbe/sha1-rc2-128-cbc*
	    *pkcs12:pbe/sha1-rc2-40-cbc*

	    digest-info? <digest-info>
	    digest-info-digest-algorithm
	    digest-info-digest

	    mac-data? <mac-data>
	    mac-data-mac
	    mac-data-mac-salt
	    mac-data-iterations

	    pfx? <pfx>
	    pfx-version
	    pfx-auth-safe
	    pfx-mac-data

	    
	    authenticated-safe? <authenticated-safe>
	    authenticated-safe->list
	    
	    safe-bag? <safe-bag>
	    safe-bag-bag-id
	    safe-bag-raw-bag-value
	    safe-bag-bag-value
	    safe-bag-bag-attributes
	    safe-bag->value

	    cert-bag? <cert-bag>
	    cert-bag-cert-id
	    cert-bag-raw-cert-value
	    cert-bag-cert-value
	    cert-bag->cert

	    crl-bag? <crl-bag>
	    crl-bag-crl-id
	    crl-bag-raw-crl-value
	    crl-bag-crl-value
	    crl-bag->crl

	    secret-bag? <secret-bag>
	    secret-bag-secret-type-id
	    secret-bag-raw-secret-value
	    secret-bag-secret-value
	    secret-bag->secret-value

	    safe-contents? <safe-contents>
	    safe-contents->list
	    
	    *pkcs12:key-bag*
	    *pkcs12:pkcs8-shrouded-key-bag*
	    *pkcs12:cert-bag*
	    *pkcs12:crl-bag*
	    *pkcs12:secret-bag*
	    *pkcs12:safe-content-bag*
	    *pkcs12:x509-certificate*
	    *pkcs12:sdsi-certificate*
	    *pkcs12:x509-crl*)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix certificate)
	    (sagittarius crypto pkix revocation)
	    (sagittarius crypto pkcs modules cms)
	    (sagittarius crypto pkcs modules akp))
(define oid oid-string->der-object-identifier)
(define *pkcs9:friendly-name* (oid "1.2.840.113549.1.9.20"))
(define *pkcs9:local-key-id*  (oid "1.2.840.113549.1.9.21"))
(define *pkcs9:cert-types*    (oid "1.2.840.113549.1.9.22"))
(define *pkcs9:crl-types*     (oid "1.2.840.113549.1.9.23"))

(define *pkcs12:pbe/sha1-rc4-128*     (oid "1.2.840.113549.1.12.1.1"))
(define *pkcs12:pbe/sha1-rc4-40*      (oid "1.2.840.113549.1.12.1.2"))
(define *pkcs12:pbe/sha1-des3-cbc*    (oid "1.2.840.113549.1.12.1.3"))
(define *pkcs12:pbe/sha1-des2-cbc*    (oid "1.2.840.113549.1.12.1.4"))
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
    (digest :type <ber-octet-string> :reader digest-info-digest))))
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
    (mac-salt :type <ber-octet-string> :reader mac-data-mac-salt)
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
(define-asn1-encodable <authenticated-safe>
  (asn1-sequence
   (of :type <content-info> :reader authenticated-safe->list)))
(define (authenticated-safe? o) (is-a? o <authenticated-safe>))

;; SafeBag ::= SEQUENCE {
;;     bagId         BAG-TYPE.&id ({PKCS12BagSet}),
;;     bagValue      [0] EXPLICIT BAG-TYPE.&Type({PKCS12BagSet}{@bagId}),
;;     bagAttributes SET OF PKCS12Attribute OPTIONAL
;; }
(define-asn1-encodable <safe-bag>
  (asn1-sequence
   ((bag-id :type <der-object-identifier> :reader safe-bag-bag-id)
    (bag-value :type <asn1-encodable> :tag 0 :explicit #t
	       :reader safe-bag-raw-bag-value)
    (bag-attributes :type <attributes> :optional #t
		    :reader safe-bag-bag-attributes))))
(define (safe-bag? o) (is-a? o <safe-bag>))
(define sid der-object-identifier->oid-string)
(define (safe-bag-bag-value sb)
  (safe-bag->value (sid (safe-bag-bag-id sb)) (safe-bag-raw-bag-value sb)))
(define-generic safe-bag->value)
(define-method safe-bag->value (o v) v) ;; default

;; KeyBag ::= PrivateKeyInfo
(define-method safe-bag->value ((o (equal (sid *pkcs12:key-bag*))) v)
  (if (one-asymmetric-key? v)
      v
      (asn1-object->asn1-encodable <one-asymmetric-key> v)))

;; PKCS8ShroudedKeyBag ::= EncryptedPrivateKeyInfo
(define-method safe-bag->value
  ((o (equal (sid *pkcs12:pkcs8-shrouded-key-bag*))) v)
  (if (encrypted-private-key-info? v)
      v
      (asn1-object->asn1-encodable <encrypted-private-key-info> v)))

;; -- CertBag
;; CertBag ::= SEQUENCE {
;;     certId    BAG-TYPE.&id   ({CertTypes}),
;;     certValue [0] EXPLICIT BAG-TYPE.&Type ({CertTypes}{@certId})
;; }
(define *pkcs12:x509-certificate* (oid "1.2.840.113549.1.9.22.1"))
(define *pkcs12:sdsi-certificate* (oid "1.2.840.113549.1.9.22.2"))

(define-asn1-encodable <cert-bag>
  (asn1-sequence
   ((cert-id :type <der-object-identifier> :reader cert-bag-cert-id)
    (cert-value :type <asn1-encodable> :tag 0 :explicit #t
		:reader cert-bag-raw-cert-value))))
(define (cert-bag? o) (is-a? o <cert-bag>))
(define-method safe-bag->value ((o (equal (sid *pkcs12:cert-bag*))) v)
  (if (cert-bag? v)
      v
      (asn1-object->asn1-encodable <cert-bag> v)))
(define-generic cert-bag->cert)
(define-method cert-bag->cert (o v) v) ;; default for SDSI cert as well...
(define-method cert-bag->cert ((o (equal (sid *pkcs12:x509-certificate*))) v)
  (bytevector->x509-certificate (der-octet-string->bytevector v)))
(define (cert-bag-cert-value o)
  (cert-bag->cert (sid (cert-bag-cert-id o)) (cert-bag-raw-cert-value o)))

;; -- CRLBag
;; CRLBag ::= SEQUENCE {
;;     crlId     BAG-TYPE.&id ({CRLTypes}),
;;     crlValue [0] EXPLICIT BAG-TYPE.&Type ({CRLTypes}{@crlId})
;; }
(define *pkcs12:x509-crl* (oid "1.2.840.113549.1.9.23.1"))

(define-asn1-encodable <crl-bag>
  (asn1-sequence
   ((crl-id :type <der-object-identifier> :reader crl-bag-crl-id)
    (crl-value :type <asn1-encodable> :tag 0 :explicit #t
	       :reader crl-bag-raw-crl-value))))
(define (crl-bag? o) (is-a? o <crl-bag>))
(define-method safe-bag->value ((o (equal (sid *pkcs12:crl-bag*))) v)
  (if (crl-bag? v)
      v
      (asn1-object->asn1-encodable <crl-bag> v)))
(define-generic crl-bag->crl)
(define-method crl-bag->crl (o v) v)
(define-method crl-bag->crl ((o (equal (sid *pkcs12:x509-crl*))) v)
  (bytevector->x509-certificate-revocation-list
   (der-octet-string->bytevector v)))

(define (crl-bag-crl-value o)
  (crl-bag->crl (sid (crl-bag-crl-id o)) (crl-bag-raw-crl-value o)))

;; -- Secret Bag
;; SecretBag ::= SEQUENCE {
;;     secretTypeId  BAG-TYPE.&id ({SecretTypes}),
;;     secretValue   [0] EXPLICIT BAG-TYPE.&Type ({SecretTypes}
;;                                                {@secretTypeId})
;; }
(define-asn1-encodable <secret-bag>
  (asn1-sequence
   ((secret-type-id :type <der-object-identifier>
		    :reader secret-bag-secret-type-id)
    (secret-value :type <asn1-encodable> :tag 0 :explicit #t
		  :reader secret-bag-raw-secret-value))))
(define (secret-bag? o) (is-a? o <secret-bag>))
(define-method safe-bag->value ((o (equal (sid *pkcs12:secret-bag*))) v)
  (if (secret-bag? v)
      v
      (asn1-object->asn1-encodable <secret-bag> v)))
(define-generic secret-bag->secret-value)
(define-method secret-bag->secret-value (o v) v)
(define-method secret-bag->secret-value ((o (equal (sid *pkcs12:key-bag*))) v)
  (bytevector->asn1-encodable <one-asymmetric-key>
			      (der-octet-string->bytevector v)))
(define-method secret-bag->secret-value
  ((o (equal (sid *pkcs12:pkcs8-shrouded-key-bag*))) v)
  (bytevector->asn1-encodable <encrypted-private-key-info>
			      (der-octet-string->bytevector v)))

(define (secret-bag-secret-value o)
  (secret-bag->secret-value (sid (secret-bag-secret-type-id o))
			    (secret-bag-raw-secret-value o)))

;; SafeContents type
(define-asn1-encodable <safe-contents>
  (asn1-sequence
   (of :type <safe-bag> :reader safe-contents->list)))
(define-method safe-bag->value ((o (equal (sid *pkcs12:safe-content-bag*))) v)
  (asn1-object->asn1-encodable <safe-contents> v))
(define (safe-contents? o) (is-a? o <safe-contents>))
)
