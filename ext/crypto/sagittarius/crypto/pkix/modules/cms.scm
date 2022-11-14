;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/modules/cms.scm - Cryptographic Message Syntax (CMS)
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
;; - https://datatracker.ietf.org/doc/html/rfc5652
;; - https://datatracker.ietf.org/doc/html/rfc5083
;;   (for Authenticated-Enveloped-Data)
#!nounbound
(library (sagittarius crypto pkix modules cms)
    (export content-info? <content-info>
	    content-info-content-type content-info-raw-content
	    *cms:content-infomation*
	    content-info->content
	    ;; 4.
	    *cms:data-content-type*
	    ;; 5.
	    *cms:signed-data-content-type*
	    signer-identifier? <signer-identifier>
	    signer-identifier-value

	    signer-info? <signer-info>
	    signer-info-version
	    signer-info-sid
	    signer-info-digest-algorithm
	    signer-info-signed-attrs
	    signer-info-signature-algorithm
	    signer-info-signature
	    signer-info-unsigned-attrs

	    encapsulated-content-info? <encapsulated-content-info>
	    encapsulated-content-info-e-content-type
	    encapsulated-content-info-e-content
	    
	    signed-data? <signed-data>
	    signed-data-version
	    signed-data-digest-algorithms
	    signed-data-encap-content-info
	    signed-data-certificates
	    signed-data-crls
	    signed-data-signer-infos
	    ;; 6
	    *cms:enveloped-data-content-type*
	    recipient-identifier? <recipient-identifier>
	    recipient-identifier-value

	    key-trans-recipient-info? <key-trans-recipient-info>
	    key-trans-recipient-info-version
	    key-trans-recipient-info-rid
	    key-trans-recipient-info-key-encryption-algorithm
	    key-trans-recipient-info-encrypted-key

	    originator-identifier-or-key? <originator-identifier-or-key>
	    originator-identifier-or-key-value

	    key-identifier? <key-identifier>
	    key-identifier-id
	    key-identifier-date
	    key-identifier-other

	    recipient-key-identifier? <recipient-key-identifier>
	    recipient-key-identifier-subject-key-identifier
	    recipient-key-identifier-date
	    recipient-key-identifier-other

	    key-agree-recipient-identifier? <key-agree-recipient-identifier>
	    key-agree-recipient-identifier-value

	    recipient-encrypted-key? <recipient-encrypted-key>
	    recipient-encrypted-key-encrypted-key

	    key-agree-recipient-info? <key-agree-recipient-info>
	    key-agree-recipient-info-version
	    key-agree-recipient-info-originator
	    key-agree-recipient-info-ukm
	    key-agree-recipient-info-key-encryption-algorithm
	    key-agree-recipient-info-recipient-encrypted-keys

	    kek-identifier? <kek-identifier>
	    kek-identifier-key-identifier
	    kek-identifier-date
	    kek-identifier-other

	    kek-recipient-info? <kek-recipient-info>
	    kek-recipient-info-version
	    kek-recipient-info-kekid
	    kek-recipient-info-key-encryption-algorithm
	    kek-recipient-info-encrypted-key

	    password-recipient-info? <password-recipient-info>
	    passeord-recipient-info-version
	    password-recipient-info-key-derivation-algorithm
	    password-recipient-info-key-encryption-algorithm
	    password-recipient-info-encrypted-key

	    other-recipient-info? <other-recipient-info>
	    other-recipient-info-ori-type
	    other-recipient-info-ori-value

	    recipient-info? <recipient-info>
	    recipient-info-value

	    originator-info? <originator-info>
	    originator-info-certs
	    originator-info-crls

	    recipient-infos? <recipient-infos>
	    recipient-infos->list

	    encrypted-content-info? <encrypted-content-info>
	    encrypted-content-info-content-type
	    encrypted-content-info-content-encryption-algorithm

	    enveloped-data? <enveloped-data>
	    enveloped-data-version
	    enveloped-data-originator-info
	    enveloped-data-recipient-infos
	    enveloped-data-encrypted-content-infos
	    enveloped-data-unprotected-attrs
	    ;; 7.
	    *cms:digested-data-content-type*

	    digested-data? <digested-data>
	    digested-data-version
	    digested-data-digest-algorithm
	    digested-date-encap-content-info
	    digested-data-digest
	    ;; 8.
	    *cms:encrypted-data-content-type*

	    encrypted-data? <encrypted-data>
	    encrypted-data-version
	    encrypted-data-encrypted-content-info
	    encrypted-data-encrypted-content
	    encrypted-data-unprotected-attrs
	    ;; 9.
	    *cms:authenticated-data-content-type*

	    authenticated-data? <authenticated-data>
	    authenticated-data-version
	    authenticated-data-originator-info
	    authenticated-data-recipient-infos
	    authenticated-data-mac-algorithm
	    authenticated-data-digest-algorithm
	    authenticated-data-encap-content-info
	    authenticated-data-auth-attrs
	    authenticated-data-mac
	    authenticated-data-unauth-attrs
	    ;; 11
	    *cms:content-type-attribute-id*
	    *cms:message-digest-attribute-id*
	    *cms:signing-time-attribute-id*
	    *cms:conter-signautre-attribute-id*
	    ;; RFC 5083
	    *cms:auth-enveloped-data-content-type*

	    auth-enveloped-data? <auth-enveloped-data>
	    auth-enveloped-data-version
	    auth-enveloped-data-originator-info
	    auth-enveloped-data-recipient-infos
	    auth-enveloped-data-auth-encrypted-content-info
	    auth-enveloped-data-auth-attrs
	    auth-enveloped-data-mac
	    auth-enveloped-data-unauth-attrs)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkix modules x509))
;; We put other useful types top for our convenience
(define oid oid-string->der-object-identifier)

;; 10.2.1 RevocationInfoChoices
;; OtherRevocationInfoFormat ::= SEQUENCE {
;;   otherRevInfoFormat OBJECT IDENTIFIER,
;;   otherRevInfo ANY DEFINED BY otherRevInfoFormat }
(define-asn1-encodable <other-revocation-info-format>
  (asn1-sequence
   ((other-rev-info-format :type <der-object-identifier>
     :reader other-revocation-info-format-other-rev-info-format)
    (other-rev-info :type <asn1-encodable>
		    :reader other-revocation-info-format-other-rev-info))))
(define (other-revocation-info-format? o)
  (is-a? o <other-revocation-info-format>))
;; RevocationInfoChoice ::= CHOICE {
;;   crl CertificateList,
;;   other [1] IMPLICIT OtherRevocationInfoFormat }
(define-asn1-encodable <revocation-info-choice>
  (asn1-choice
   ((crl :type <certificate-list>)
    (other :type <other-revocation-info-format> :tag 1 :explicit #f))
   :reader revocation-info-choice-value))
(define (revocation-info-choice? o) (is-a? o <revocation-info-choice>))

;; RevocationInfoChoices ::= SET OF RevocationInfoChoice
(define-asn1-encodable <revocation-info-choices>
  (asn1-set
   (of :type <revocation-info-choice> :reader revocation-info-choices->list)))
(define (revocation-info-choices? o) (is-a? o <revocation-info-choices>))

;; 10.2.2. CertificateChoices
;; CertificateChoices ::= CHOICE {
;;   certificate Certificate,
;;   extendedCertificate [0] IMPLICIT ExtendedCertificate,  -- Obsolete
;;   v1AttrCert [1] IMPLICIT AttributeCertificateV1,        -- Obsolete
;;   v2AttrCert [2] IMPLICIT AttributeCertificateV2,
;;   other [3] IMPLICIT OtherCertificateFormat }
(define-asn1-encodable <certificate-choices>
  (asn1-choice
   ((certificate :type <certificate>)
    ;; TODO
    (extended-certificate :type <asn1-encodable> :tag 0 :explicit #f)
    (v1-attr-cert :type <asn1-encodable> :tag 1 :explicit #f)
    (v2-attr-cert :type <asn1-encodable> :tag 2 :explicit #f)
    (other :type <asn1-encodable> :tag 3 :explicit #f))
   :reader certificate-choices-certificate))
(define (certificate-choices? o) (is-a? o <certificate-choices>))

;; 10.2.3 CertificateSet
;; CertificateSet ::= SET OF CertificateChoices
(define-asn1-encodable <certificate-set>
  (asn1-set
   (of :type <certificate-choices> :reader certificate-set->list)))
(define (certificate-set? o) (is-a? o <certificate-set>))

;; 10.2.4 IssuerAndSerialNumber
;; IssuerAndSerialNumber ::= SEQUENCE {
;;   issuer Name,
;;   serialNumber CertificateSerialNumber }
;; CertificateSerialNumber ::= INTEGER
(define-asn1-encodable <issuer-and-serial-number>
  (asn1-sequence
   ((issuer :type <name> :reader issuer-and-serial-number-issuer)
    (serial-number :type <der-integer>
		   :reader issuer-and-serial-number-serial-number))))
(define (issuer-and-serial-number? o) (is-a? o <issuer-and-serial-number>))

;; 10.2.7 OtherKeyAttribute
;; OtherKeyAttribute ::= SEQUENCE {
;;   keyAttrId OBJECT IDENTIFIER,
;;   keyAttr ANY DEFINED BY keyAttrId OPTIONAL }
(define-asn1-encodable <other-key-attribute>
  (asn1-sequence
   ((key-attr-id :type <der-object-identifier>
		 :reader other-key-attribute-key-attr-id)
    (key-attr :type <asn1-encodable> :reader other-key-attribute-key-attr))))
(define (other-key-attribute? o) (is-a? o <other-key-attribute>))

;; 3. General Syntax
(define *cms:content-infomation* (oid "1.2.840.113549.1.9.16.1.6"))
;; ContentInfo ::= SEQUENCE {
;;   contentType ContentType,
;;   content [0] EXPLICIT ANY DEFINED BY contentType }

;; ContentType ::= OBJECT IDENTIFIER
(define-asn1-encodable <content-info>
  (asn1-sequence
   ((content-type :type <der-object-identifier>
		  :reader content-info-content-type)
    (content :type <asn1-encodable> :tag 0 :explicit #t
	     :reader content-info-raw-content))))
(define (content-info? o) (is-a? o <content-info>))
(define (content-info-content (content-info content-info?))
  (content-info->content
   (der-object-identifier->oid-string (content-info-content-type content-info))
   (content-info-raw-content content-info)))

(define-generic content-info->content)
;; 4. Data Content Type
(define *cms:data-content-type* (oid "1.2.840.113549.1.7.1"))
;; should we check octet-string or not?
(define-method content-info->content ((oid (equal *cms:data-content-type*)) d)
  d)

;; 5. Signed-data Content Type
(define *cms:signed-data-content-type* (oid "1.2.840.113549.1.7.2"))
(define-method content-info->content
  ((oid (equal *cms:signed-data-content-type*)) d)
  (asn1-object->asn1-encodable d <signed-data>))

;; SignerIdentifier ::= CHOICE {
;;   issuerAndSerialNumber IssuerAndSerialNumber,
;;   subjectKeyIdentifier [0] SubjectKeyIdentifier }
(define-asn1-encodable <signer-identifier>
  (asn1-choice
   ((issuer-and-serial-number :type <issuer-and-serial-number>)
    (subject-key-identifier :type <der-octet-string> :tag 0))
   :reader signer-identifier-value))
(define (signer-identifier? o) (is-a? o <signer-identifier>))

;; SignerInfo ::= SEQUENCE {
;;   version CMSVersion,
;;   sid SignerIdentifier,
;;   digestAlgorithm DigestAlgorithmIdentifier,
;;   signedAttrs [0] IMPLICIT SignedAttributes OPTIONAL,
;;   signatureAlgorithm SignatureAlgorithmIdentifier,
;;   signature SignatureValue,
;;   unsignedAttrs [1] IMPLICIT UnsignedAttributes OPTIONAL }
;; SignedAttributes ::= SET SIZE (1..MAX) OF Attribute
;; UnsignedAttributes ::= SET SIZE (1..MAX) OF Attribute
;; Attribute ::= SEQUENCE {
;;   attrType OBJECT IDENTIFIER,
;;   attrValues SET OF AttributeValue }
;; AttributeValue ::= ANY
;; SignatureValue ::= OCTET STRING
(define-asn1-encodable <signer-info>
  (asn1-sequence
   ((version :type <der-integer> :reader signer-info-version)
    (sid :type <signer-identifier> :reader signer-info-sid)
    (digest-algorithm :type <algorithm-identifier>
		      :reader signer-info-digest-algorithm)
    ;; we reuse <attributes> and <attribute> of x509
    (signed-attrs :type <attributes> :tag 0 :optional #t :explicit #f
		  :reader signer-info-signed-attrs)
    (signature-algorithm :type <algorithm-identifier>
			 :reader signer-info-signature-algorithm)
    (signature :type <der-octet-string> :reader signer-info-signature)
    (unsigned-attrs :type <attributes> :tag 1 :optional #t :explicit #f
		    :reader signer-info-unsigned-attrs))))
(define (signer-info? o) (is-a? o <signer-info>))

;; EncapsulatedContentInfo ::= SEQUENCE {
;;   eContentType ContentType,
;;   eContent [0] EXPLICIT OCTET STRING OPTIONAL }
;; ContentType ::= OBJECT IDENTIFIER
(define-asn1-encodable <encapsulated-content-info>
  (asn1-sequence
   ((e-content-type :type <der-object-identifier>
		    :reader encapsulated-content-info-e-content-type)
    (e-content :type <der-octet-string> :tag 0 :optional #t :explicit #t
	       :reader encapsulated-content-info-e-content))))
(define (encapsulated-content-info? o)
  (is-a? o <encapsulated-content-info>))

;; SignedData ::= SEQUENCE {
;;   version CMSVersion,
;;   digestAlgorithms DigestAlgorithmIdentifiers,
;;   encapContentInfo EncapsulatedContentInfo,
;;   certificates [0] IMPLICIT CertificateSet OPTIONAL,
;;   crls [1] IMPLICIT RevocationInfoChoices OPTIONAL,
;;   signerInfos SignerInfos }
;; DigestAlgorithmIdentifiers ::= SET OF DigestAlgorithmIdentifier
;; SignerInfos ::= SET OF SignerInfo
(define-asn1-encodable <signed-data>
  (asn1-sequence
   ((version :type <der-integer> :reader signed-data-version)
    (digest-algorithms :type <algorithm-identifier>
		       :multiple 'set
		       :reader signed-data-digest-algorithms)
    (encap-content-info :type <encapsulated-content-info>
			:reader signed-data-encap-content-info)
    (certificates :type <certificate-set> :tag 0 :optional #t :explicit #f
		  :reader signed-data-certificates)
    (crls :type <revocation-info-choices> :tag 1 :optional #t :explicit #f
	  :reader signed-data-crls)
    (signer-infos :type <signer-info> :multiple 'set
		  :reader signed-data-signer-infos))))
(define (signed-data? o) (is-a? o <signed-data>))

;; 6. Enveloped-data Content Type
(define *cms:enveloped-data-content-type* (oid "1.2.840.113549.1.7.3"))
(define-method content-info->content
  ((oid (equal *cms:enveloped-data-content-type*)) d)
  (asn1-object->asn1-encodable d <enveloped-data>))

;; RecipientIdentifier ::= CHOICE {
;;   issuerAndSerialNumber IssuerAndSerialNumber,
;;   subjectKeyIdentifier [0] SubjectKeyIdentifier }
;; SubjectKeyIdentifier ::= OCTET STRING
(define-asn1-encodable <recipient-identifier>
  (asn1-choice
   ((issuer-and-serial-number :type <issuer-and-serial-number>)
    (subject-key-identifier :type <der-octet-string>))
   :reader recipient-identifier-value))
(define (recipient-identifier? o) (is-a? o <recipient-identifier>))

;; KeyTransRecipientInfo ::= SEQUENCE {
;;   version CMSVersion,  -- always set to 0 or 2
;;   rid RecipientIdentifier,
;;   keyEncryptionAlgorithm KeyEncryptionAlgorithmIdentifier,
;;   encryptedKey EncryptedKey }
(define-asn1-encodable <key-trans-recipient-info>
  (asn1-sequence
   ((version :type <der-integer> :reader key-trans-recipient-info-version)
    (rid :type <recipient-identifier> :reader key-trans-recipient-info-rid)
    (key-encryption-algorithm :type <algorithm-identifier>
     :reader key-trans-recipient-info-key-encryption-algorithm)
    (encrypted-key :type <der-octet-string>
		   :reader key-trans-recipient-info-encrypted-key))))
(define (key-trans-recipient-info? o) (is-a? o <key-trans-recipient-info>))

;; OriginatorPublicKey ::= SEQUENCE {
;;   algorithm AlgorithmIdentifier,
;;   publicKey BIT STRING }
;; We use SubjectPublicKeyInfo for this, it's the same...

;; OriginatorIdentifierOrKey ::= CHOICE {
;;   issuerAndSerialNumber IssuerAndSerialNumber,
;;   subjectKeyIdentifier [0] SubjectKeyIdentifier,
;;   originatorKey [1] OriginatorPublicKey }
(define-asn1-encodable <originator-identifier-or-key>
  (asn1-choice
   ((issuer-and-serial-number :type <issuer-and-serial-number>)
    (subject-key-identifier :type <der-octet-string> :tag 0)
    (originator-key :type <subject-public-key-info> :tag 1))
   :reader originator-identifier-or-key-value))
(define (originator-identifier-or-key? o)
  (is-a? o <originator-identifier-or-key>))

;; base class for *KeyIdentifier...
(define-asn1-encodable <key-identifier>
  (asn1-sequence
   ((id :type <der-octet-string> :reader key-identifier-id)
    (date :type <der-generalized-time> :optional #t
	  :reader key-identifier-date)
    (other :type <other-key-attribute> :optional #t
	   :reader key-identifier-other))))
(define (key-identifier? o) (is-a? o <key-identifier>))

;; RecipientKeyIdentifier ::= SEQUENCE {
;;   subjectKeyIdentifier SubjectKeyIdentifier,
;;   date GeneralizedTime OPTIONAL,
;;   other OtherKeyAttribute OPTIONAL }
(define-class <recipient-key-identifier> (<key-identifier>) ())
(define recipient-key-identifier-subject-key-identifier key-identifier-id)
(define recipient-key-identifier-date key-identifier-date)
(define recipient-key-identifier-other key-identifier-other)
(define (recipient-key-identifier? o) (is-a? o <recipient-key-identifier>))

;; KeyAgreeRecipientIdentifier ::= CHOICE {
;;   issuerAndSerialNumber IssuerAndSerialNumber,
;;   rKeyId [0] IMPLICIT RecipientKeyIdentifier }
(define-asn1-encodable <key-agree-recipient-identifier>
  (asn1-choice
   ((issuer-and-serial-number :type <issuer-and-serial-number>)
    (r-key-id :type <recipient-key-identifier> :tag 0 :explicit #f))
   :reader key-agree-recipient-identifier-value))
(define (key-agree-recipient-identifier? o)
  (is-a? o <key-agree-recipient-identifier>))

;; RecipientEncryptedKey ::= SEQUENCE {
;;   rid KeyAgreeRecipientIdentifier,
;;   encryptedKey EncryptedKey }
(define-asn1-encodable <recipient-encrypted-key>
  (asn1-sequence
   ((rid :type <key-agree-recipient-identifier>
	 :reader recipient-encrypted-key-rid)
    (encrypted-key :type <der-octet-string>
		   :reader recipient-encrypted-key-encrypted-key))))
(define (recipient-encrypted-key? o) (is-a? o <recipient-encrypted-key>))

;; KeyAgreeRecipientInfo ::= SEQUENCE {
;;   version CMSVersion,  -- always set to 3
;;   originator [0] EXPLICIT OriginatorIdentifierOrKey,
;;   ukm [1] EXPLICIT UserKeyingMaterial OPTIONAL,
;;   keyEncryptionAlgorithm KeyEncryptionAlgorithmIdentifier,
;;   recipientEncryptedKeys RecipientEncryptedKeys }
;; UserKeyingMaterial ::= OCTET STRING
;; RecipientEncryptedKeys ::= SEQUENCE OF RecipientEncryptedKey
(define-asn1-encodable <key-agree-recipient-info>
  (asn1-sequence
   ((version :type <der-integer> :reader key-agree-recipient-info-version)
    (originator :type <originator-identifier-or-key> :tag 0 :explicit #t
		:reader key-agree-recipient-info-originator)
    (ukm :type <der-octet-string> :tag 1 :optional #t :explicit #t
	 :reader key-agree-recipient-info-ukm)
    (key-encryption-algorithm :type <algorithm-identifier>
     :reader key-agree-recipient-info-key-encryption-algorithm)
    (recipient-encrypted-keys :type <recipient-encrypted-key>
     :multiple 'sequence
     :reader key-agree-recipient-info-recipient-encrypted-keys))))
(define (key-agree-recipient-info? o) (is-a? o <key-agree-recipient-info>))

;; KEKIdentifier ::= SEQUENCE {
;;   keyIdentifier OCTET STRING,
;;   date GeneralizedTime OPTIONAL,
;;   other OtherKeyAttribute OPTIONAL }
(define-class <kek-identifier> (<key-identifier>) ())
(define kek-identifier-key-identifier key-identifier-id)
(define kek-identifier-date key-identifier-date)
(define kek-identifier-other key-identifier-other)
(define (kek-identifier? o) (is-a? o <kek-identifier>))
;; KEKRecipientInfo ::= SEQUENCE {
;;   version CMSVersion,  -- always set to 4
;;   kekid KEKIdentifier,
;;   keyEncryptionAlgorithm KeyEncryptionAlgorithmIdentifier,
;;   encryptedKey EncryptedKey }
(define-asn1-encodable <kek-recipient-info>
  (asn1-sequence
   ((version :type <der-integer> :reader kek-recipient-info-version)
    (kekid :type <kek-identifier> :reader kek-recipient-info-kekid)
    (key-encryption-algorithm :type <algorithm-identifier>
     :reader kek-recipient-info-key-encryption-algorithm)
    (encrypted-key :type <der-octet-string>
		   :reader kek-recipient-info-encrypted-key))))
(define (kek-recipient-info? o) (is-a? o <kek-recipient-info>))

;; PasswordRecipientInfo ::= SEQUENCE {
;;   version CMSVersion,   -- Always set to 0
;;   keyDerivationAlgorithm [0] KeyDerivationAlgorithmIdentifier
;;                                OPTIONAL,
;;   keyEncryptionAlgorithm KeyEncryptionAlgorithmIdentifier,
;;   encryptedKey EncryptedKey }
(define-asn1-encodable <password-recipient-info>
  (asn1-sequence
   ((version :type <der-integer> :reader passeord-recipient-info-version)
    (key-derivation-algorithm :type <algorithm-identifier>
     :reader password-recipient-info-key-derivation-algorithm)
    (key-encryption-algorithm :type <algorithm-identifier>
     :reader password-recipient-info-key-encryption-algorithm)
    (encrypted-key :type <der-octet-string>
    :reader password-recipient-info-encrypted-key))))
(define (password-recipient-info? o) (is-a? o <password-recipient-info>))

;; OtherRecipientInfo ::= SEQUENCE {
;;   oriType OBJECT IDENTIFIER,
;;   oriValue ANY DEFINED BY oriType }
(define-asn1-encodable <other-recipient-info>
  (asn1-sequence
   ((ori-type :type <der-object-identifier>
	      :reader other-recipient-info-ori-type)
    (ori-value :type <asn1-encodable>
	       :reader other-recipient-info-ori-value))))
(define (other-recipient-info? o) (is-a? o <other-recipient-info>))

;; RecipientInfo ::= CHOICE {
;;   ktri KeyTransRecipientInfo,
;;   kari [1] KeyAgreeRecipientInfo,
;;   kekri [2] KEKRecipientInfo,
;;   pwri [3] PasswordRecipientinfo,
;;   ori [4] OtherRecipientInfo }
;; EncryptedKey ::= OCTET STRING
(define-asn1-encodable <recipient-info>
  (asn1-choice
   ((ktri :type <key-trans-recipient-info>)
    (kari :type <key-agree-recipient-info> :tag 1)
    (kekri :type <kek-recipient-info> :tag 2)
    (pwri :type <password-recipient-info> :tag 3)
    (ori :type <other-recipient-info> :tag 4))
   :reader recipient-info-value))
(define (recipient-info? o) (is-a? o <recipient-info>))

;; OriginatorInfo ::= SEQUENCE {
;;   certs [0] IMPLICIT CertificateSet OPTIONAL,
;;   crls [1] IMPLICIT RevocationInfoChoices OPTIONAL }
(define-asn1-encodable <originator-info>
  (asn1-sequence
   ((certs :type <certificate-set> :tag 0 :optional #t :explicit #f
	   :reader originator-info-certs)
    (crls :type <revocation-info-choices> :tag 1 :optional #t :explicit #f
	  :reader originator-info-crls))))
(define (originator-info? o) (is-a? o <originator-info>))

;; RecipientInfos ::= SET SIZE (1..MAX) OF RecipientInfo
(define-asn1-encodable <recipient-infos>
  (asn1-set
   (of :type <recipient-info> :reader recipient-infos->list)))
(define (recipient-infos? o) (is-a? o <recipient-infos>))

;; EncryptedContentInfo ::= SEQUENCE {
;;   contentType ContentType,
;;   contentEncryptionAlgorithm ContentEncryptionAlgorithmIdentifier }
;; EncryptedContent ::= OCTET STRING
(define-asn1-encodable <encrypted-content-info>
  (asn1-sequence
   ((content-type :type <der-object-identifier>
		  :reader encrypted-content-info-content-type)
    (content-encryption-algorithm :type <algorithm-identifier>
     :reader encrypted-content-info-content-encryption-algorithm))))
(define (encrypted-content-info? o) (is-a? o <encrypted-content-info>))

;; EnvelopedData ::= SEQUENCE {
;;   version CMSVersion,
;;   originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
;;   recipientInfos RecipientInfos,
;;   encryptedContentInfo EncryptedContentInfo,
;;   unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL }
;; UnprotectedAttributes ::= SET SIZE (1..MAX) OF Attribute
(define-asn1-encodable <enveloped-data>
  (asn1-sequence
   ((version :type <der-integer> :reader enveloped-data-version)
    (originator-info :type <originator-info> :tag 0 :optional #t :explicit #f
		     :reader enveloped-data-originator-info)
    (recipient-infos :type <recipient-infos>
		     :reader enveloped-data-recipient-infos)
    (encrypted-content-info :type <encrypted-content-info>
			    :reader enveloped-data-encrypted-content-infos)
    (unprotected-attrs :type <attributes> :tag 1 :optional #t :explicit #f
		       :reader enveloped-data-unprotected-attrs))))
(define (enveloped-data? o) (is-a? o <enveloped-data>))

;; 7. Digested-data Content Type
(define *cms:digested-data-content-type* (oid "1.2.840.113549.1.7.5"))
(define-method content-info->content
  ((oid (equal *cms:digested-data-content-type*)) d)
  (asn1-object->asn1-encodable d <digested-data>))

;; DigestedData ::= SEQUENCE {
;;   version CMSVersion,
;;   digestAlgorithm DigestAlgorithmIdentifier,
;;   encapContentInfo EncapsulatedContentInfo,
;;   digest Digest }
;; Digest ::= OCTET STRING
(define-asn1-encodable <digested-data>
  (asn1-sequence
   ((version :type <der-integer> :reader digested-data-version)
    (digest-algorithm :type <algorithm-identifier>
		      :reader digested-data-digest-algorithm)
    (encap-content-info :type <encapsulated-content-info>
			:reader digested-date-encap-content-info)
    (digest :type <der-octet-string> :reader digested-data-digest))))
(define (digested-data? o) (is-a? o <digested-data>))

;; 8. Encrypted-data Content Type
(define *cms:encrypted-data-content-type* (oid "1.2.840.113549.1.7.6"))
(define-method content-info->content
  ((oid (equal *cms:encrypted-data-content-type*)) d)
  (asn1-object->asn1-encodable d <encrypted-data>))

;; EncryptedData ::= SEQUENCE {
;;   version CMSVersion,
;;   encryptedContentInfo EncryptedContentInfo,
;;   encryptedContent [0] IMPLICIT EncryptedContent OPTIONAL,
;;   unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL }
(define-asn1-encodable <encrypted-data>
  (asn1-sequence
   ((version :type <der-integer> :reader encrypted-data-version)
    (encrypted-content-info :type <encrypted-content-info>
     :reader encrypted-data-encrypted-content-info)
    (encrypted-content :type <der-octet-string> :tag 0 :optional #t :explicit #f
		       :reader encrypted-data-encrypted-content)
    (unprotected-attrs :type <attributes> :tag 1 :optional #t :explicit #f
		       :reader encrypted-data-unprotected-attrs))))
(define (encrypted-data? o) (is-a? o <encrypted-data>))

;; 9. Authenticated-data Content Type
(define *cms:authenticated-data-content-type*
  (oid "1.2.840.113549.1.9.16.2"))
(define-method content-info->content
  ((oid (equal *cms:authenticated-data-content-type*)) d)
  (asn1-object->asn1-encodable d <authenticated-data>))

;; AuthenticatedData ::= SEQUENCE {
;;   version CMSVersion,
;;   originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
;;   recipientInfos RecipientInfos,
;;   macAlgorithm MessageAuthenticationCodeAlgorithm,
;;   digestAlgorithm [1] DigestAlgorithmIdentifier OPTIONAL,
;;   encapContentInfo EncapsulatedContentInfo,
;;   authAttrs [2] IMPLICIT AuthAttributes OPTIONAL,
;;   mac MessageAuthenticationCode,
;;   unauthAttrs [3] IMPLICIT UnauthAttributes OPTIONAL }
;; AuthAttributes ::= SET SIZE (1..MAX) OF Attribute
;; UnauthAttributes ::= SET SIZE (1..MAX) OF Attribute
;; MessageAuthenticationCode ::= OCTET STRING
(define-asn1-encodable <authenticated-data>
  (asn1-sequence
   ((version :type <der-integer> :reader authenticated-data-version)
    (originator-info :type <originator-info> :tag 0 :optional #t :explicit #f
		     :reader authenticated-data-originator-info)
    (recipient-infos :type <recipient-infos>
		     :reader authenticated-data-recipient-infos)
    (mac-algorithm :type <algorithm-identifier>
		   :reader authenticated-data-mac-algorithm)
    (digest-algorithm :type <algorithm-identifier>
		      :tag 1 :optional #t :explicit #t
		      :reader authenticated-data-digest-algorithm)
    (encap-content-info :type <encapsulated-content-info>
			:reader authenticated-data-encap-content-info)
    (auth-attrs :type <attributes> :tag 2 :optional #t :explicit #f
		:reader authenticated-data-auth-attrs)
    (mac :type <der-octet-string> :reader authenticated-data-mac)
    (unauth-attrs :type <attributes> :tag 3 :optional #t :explicit #f
		  :reader authenticated-data-unauth-attrs))))
(define (authenticated-data? o) (is-a? o <authenticated-data>))

;; 11 Useful Attributes
(define *cms:content-type-attribute-id* (oid "1.2.840.113549.1.9.3"))
(define *cms:message-digest-attribute-id* (oid "1.2.840.113549.1.9.4"))
(define *cms:signing-time-attribute-id* (oid "1.2.840.113549.1.9.5"))
;; SigningType ::= Time ;; <asn1-time>
(define *cms:conter-signautre-attribute-id* (oid "1.2.840.113549.1.9.6"))
;; Countersignature ::= SignerInfo

;;; RFC 5083
;; 2. Authenticated-Enveloped-Data Content Type
(define *cms:auth-enveloped-data-content-type*
  (oid "1.2.840.113549.1.9.16.1.23"))
(define-method content-info->content
  ((oid (equal *cms:auth-enveloped-data-content-type*)) d)
  (asn1-object->asn1-encodable d <auth-enveloped-data>))
;; AuthEnvelopedData ::= SEQUENCE {
;;   version CMSVersion,
;;   originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
;;   recipientInfos RecipientInfos,
;;   authEncryptedContentInfo EncryptedContentInfo,
;;   authAttrs [1] IMPLICIT AuthAttributes OPTIONAL,
;;   mac MessageAuthenticationCode,
;;   unauthAttrs [2] IMPLICIT UnauthAttributes OPTIONAL }
(define-asn1-encodable <auth-enveloped-data>
  (asn1-sequence
   ((version :type <der-integer> :reader auth-enveloped-data-version)
    (originator-info :type <originator-info> :tag 0 :optional #t :explicit #f
		     :reader auth-enveloped-data-originator-info)
    (recipient-infos :type <recipient-infos>
		     :reader auth-enveloped-data-recipient-infos)
    (auth-encrypted-content-info :type <encapsulated-content-info>
     :reader auth-enveloped-data-auth-encrypted-content-info)
    (auth-attrs :type <attributes> :tag 1 :optional #t :explicit #f
		:reader auth-enveloped-data-auth-attrs)
    (mac :type <der-octet-string> :reader auth-enveloped-data-mac)
    (unauth-attrs :type <attributes> :tag 2 :optional #t :explicit #f
		  :reader auth-enveloped-data-unauth-attrs))))
(define (auth-enveloped-data? o) (is-a? o <auth-enveloped-data>))
)
