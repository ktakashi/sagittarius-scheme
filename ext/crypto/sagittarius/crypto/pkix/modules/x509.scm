;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/modules/x509.scm - PKIX modules
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

;; ASN.1 modules of PKIX using X.509
;; ref
;;  - https://datatracker.ietf.org/doc/html/rfc5912
#!nounbound
(library (sagittarius crypto pkix modules x509)
    (export algorithm-identifier? <algorithm-identifier>
	    algorithm-identifier-algorithm algorithm-identifier-parameters

	    subject-public-key-info? <subject-public-key-info>
	    subject-public-key-info-algorithm
	    subject-public-key-info-subject-public-key

	    validity? <validity>
	    validity-not-before validity-not-after
	    
	    single-attribute? <single-attribute>
	    single-attribute-type single-attribute-value

	    relative-distinguished-name? <relative-distinguished-name>
	    relative-distinguished-name-elements

	    rdn-sequence? <rdn-sequence>
	    rdn-sequence-elements
	    
	    name? <name> name-rdn*

	    extension? <extension>
	    extension-id extension-critical extension-value
	    extensions? <extensions>
	    extensions->list
	    
	    tbs-certificate? <tbs-certificate>
	    tbs-certificate-version
	    tbs-certificate-serial-number
	    tbs-certificate-signature
	    tbs-certificate-issuer
	    tbs-certificate-validity
	    tbs-certificate-subject
	    tbs-certificate-subject-public-key-info
	    tbs-certificate-issuer-unique-id
	    tbs-certificate-subject-unique-id
	    tbs-certificate-extensions

	    certificate? <certificate>
	    certificate-c
	    certificate-algorithm
	    certificate-signature
	    
	    *extension:authority-key-identifier*
	    *extension:subject-key-identifier*
	    *extension:key-usage*
	    *extension:private-key-usage-period*
	    *extension:certificate-policies*
	    *extension:policy-mappings*
	    *extension:subject-alt-name*
	    *extension:issuer-alt-name*
	    *extension:subject-directory-attributes*
	    *extension:basic-constraints*
	    *extension:name-constraints*
	    *extension:policy-constraints*
	    *extension:ext-key-usage*
	    *extension:crl-distribution-points*
	    *extension:inhibit-any-policy*
	    *extension:freshest-crl*
	    *extension:authority-info-access*
	    *extension:subject-info-access*

	    ;; CRL
	    revoked-certificate? <revoked-certificate>
	    revoked-certificate-user-certificate
	    revoked-certificate-revocation-date

	    revoked-certificates? <revoked-certificates>
	    revoked-certificates-elements

	    tbs-cert-list? <tbs-cert-list>
	    tbs-cert-list-version
	    tbs-cert-list-signature
	    tbs-cert-list-issuer
	    tbs-cert-list-this-update
	    tbs-cert-list-next-update
	    tbs-cert-list-revoked-certificates
	    tbs-cert-list-crl-extensions

	    certificate-list? <certificate-list>
	    certificate-list-c
	    certificate-list-algorithm
	    certificate-list-signature
	    
	    *extension:crl-number*
	    *extension:delta-crl-indicator*
	    *extension:issuing-distribution-point*
	    
	    *extension:crl-reason*
	    *extension:certificate-issuer*
	    *extension:hold-instruction-code*
	    *extension:invalidity-date*

	    ;; CSR
	    attribute? <attribute>
	    attribute-type
	    attribute-values

	    attributes? <attributes>
	    attributes->list

	    certification-request-info? <certification-request-info>
	    certification-request-info-version
	    certification-request-info-name
	    certification-request-info-subject-pk-info
	    certification-request-info-attributes

	    certification-request? <certification-request>
	    certification-request-certification-request-info
	    certification-request-signature-algorithm
	    certification-request-signature)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules))

;; AlgorithmIdentifier{ALGORITHM-TYPE, ALGORITHM-TYPE:AlgorithmSet} ::=
;;         SEQUENCE {
;;             algorithm   ALGORITHM-TYPE.&id({AlgorithmSet}),
;;             parameters  ALGORITHM-TYPE.
;;                    &Params({AlgorithmSet}{@algorithm}) OPTIONAL
;;         }
(define-asn1-encodable <algorithm-identifier>
  (asn1-sequence
   ((algorithm :type <der-object-identifier>
	       :reader algorithm-identifier-algorithm)
    (parameters :type <asn1-encodable> :optional #t
		:reader algorithm-identifier-parameters))))
(define (algorithm-identifier? o) (is-a? o <algorithm-identifier>))

;; SubjectPublicKeyInfo  ::=  SEQUENCE  {
;;     algorithm            AlgorithmIdentifier{PUBLIC-KEY,
;;                              {PublicKeyAlgorithms}},
;;     subjectPublicKey     BIT STRING  }
(define-asn1-encodable <subject-public-key-info>
  (asn1-sequence
   ((algorithm :type <algorithm-identifier>
	       :reader subject-public-key-info-algorithm)
    (subject-public-key :type <der-bit-string>
			:reader subject-public-key-info-subject-public-key))))
(define (subject-public-key-info? o) (is-a? o <subject-public-key-info>))

;; Validity ::= SEQUENCE {
;;     notBefore      Time,
;;     notAfter       Time  }
(define-asn1-encodable <validity>
  (asn1-sequence
   ((not-before :type <asn1-time> :reader validity-not-before)
    (not-after :type <asn1-time> :reader validity-not-after))))
(define (validity? o) (is-a? o <validity>))

;; SingleAttribute{ATTRIBUTE:AttrSet} ::= SEQUENCE {
;;     type      ATTRIBUTE.&id({AttrSet}),
;;     value     ATTRIBUTE.&Type({AttrSet}{@type})
;; }
(define-asn1-encodable <single-attribute>
  (asn1-sequence
   ((type :type <der-object-identifier> :reader single-attribute-type)
    (value :type <asn1-encodable> :reader single-attribute-value))))
(define (single-attribute? o) (is-a? o <single-attribute>))

;; DistinguishedName ::=   RDNSequence
;; RelativeDistinguishedName  ::=
;;     SET SIZE (1 .. MAX) OF SingleAttribute { {SupportedAttributes} }
(define-asn1-encodable <relative-distinguished-name>
  (asn1-set
   (of :type <single-attribute>
       :reader relative-distinguished-name-elements)))
(define (relative-distinguished-name? o)
  (is-a? o <relative-distinguished-name>))
;; Name ::= CHOICE { -- only one possibility for now --
;;     rdnSequence  RDNSequence }
;; RDNSequence ::= SEQUENCE OF RelativeDistinguishedName
(define-asn1-encodable <rdn-sequence>
  (asn1-sequence
   (of :type <relative-distinguished-name>
       :reader rdn-sequence-elements)))
(define (rdn-sequence? o) (is-a? o <rdn-sequence>))

(define <name> <rdn-sequence>)
(define name? rdn-sequence?)
(define name-rdn* rdn-sequence-elements)

;; Extension{EXTENSION:ExtensionSet} ::= SEQUENCE {
;;     extnID      EXTENSION.&id({ExtensionSet}),
;;     critical    BOOLEAN
;; --                     (EXTENSION.&Critical({ExtensionSet}{@extnID}))
;;                      DEFAULT FALSE,
;;     extnValue   OCTET STRING (CONTAINING
;;                 EXTENSION.&ExtnType({ExtensionSet}{@extnID}))
;;                 --  contains the DER encoding of the ASN.1 value
;;                 --  corresponding to the extension type identified
;;                 --  by extnID
;; }
(define-asn1-encodable <extension>
  (asn1-sequence
   ((id :type <der-object-identifier> :reader extension-id)
    (critical :type <der-boolean> :reader extension-critical :optional #t)
    (value :type <der-octet-string> :reader extension-value))))
(define (extension? o) (is-a? o <extension>))
(define (extension=? (a extension?) (b extension?))
  (equal? (extension-id a) (extension-id b)))
;; Extensions{EXTENSION:ExtensionSet} ::=
;;       SEQUENCE SIZE (1..MAX) OF Extension{{ExtensionSet}}
(define-asn1-encodable <extensions>
  (asn1-sequence
   (of :type <extension> :reader extensions->list)
   extension=?))
(define (extensions? o) (is-a? o <extensions>))
;; TBSCertificate  ::=  SEQUENCE  {
;;     version         [0]  Version DEFAULT v1,
;;     serialNumber         CertificateSerialNumber,
;;     signature            AlgorithmIdentifier{SIGNATURE-ALGORITHM,
;;                               {SignatureAlgorithms}},
;;     issuer               Name,
;;     validity             Validity,
;;     subject              Name,
;;     subjectPublicKeyInfo SubjectPublicKeyInfo,
;;     ... ,
;;     [[2:               -- If present, version MUST be v2
;;     issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL,
;;     subjectUniqueID [2]  IMPLICIT UniqueIdentifier OPTIONAL
;;     ]],
;;     [[3:               -- If present, version MUST be v3 --
;;     extensions      [3]  Extensions{{CertExtensions}} OPTIONAL
;;     ]], ... }
(define-asn1-encodable <tbs-certificate>
  (asn1-sequence
   ((version :type <der-integer> :tag 0 :explicit #t :optional #t
	     :reader tbs-certificate-version)
    (serial-number :type <der-integer> :reader tbs-certificate-serial-number)
    (signature :type <algorithm-identifier> :reader tbs-certificate-signature)
    (issuer :type <name> :reader tbs-certificate-issuer)
    (validity :type <validity> :reader tbs-certificate-validity)
    (subject :type <name> :reader tbs-certificate-subject)
    (subject-public-key-info :type <subject-public-key-info>
			     :reader tbs-certificate-subject-public-key-info)
    (issuer-unique-id :type <der-bit-string> :tag 1 :explicit #f :optional #t
		      :reader tbs-certificate-issuer-unique-id)
    (subject-unique-id :type <der-bit-string> :tag 2 :explicit #f :optional #t
		       :reader tbs-certificate-subject-unique-id)
    (extensions :type <extensions> :tag 3 :explicit #t :optional #t
		:reader tbs-certificate-extensions))))
(define (tbs-certificate? o) (is-a? o <tbs-certificate>))

;; --  SIGNED{ToBeSigned} ::= SEQUENCE {
;; --    toBeSigned  ToBeSigned,
;; --    algorithm   AlgorithmIdentifier{SIGNATURE-ALGORITHM,
;; --                    {SignatureAlgorithms}},
;; --    signature   BIT STRING
;; --  }
;; Certificate  ::=  SIGNED{TBSCertificate}
;; NOTE: We can't make signed class due to the order of slots...
(define-asn1-encodable <certificate>
  (asn1-sequence
   ((c :type <tbs-certificate> :reader certificate-c)
    (algorithm :type <algorithm-identifier> :reader certificate-algorithm)
    (signature :type <der-bit-string> :reader certificate-signature))))
(define (certificate? o) (is-a? o <certificate>))

;; CertExtensions EXTENSION ::= {
;;         ext-AuthorityKeyIdentifier | ext-SubjectKeyIdentifier |
;;         ext-KeyUsage | ext-PrivateKeyUsagePeriod |
;;         ext-CertificatePolicies | ext-PolicyMappings |
;;         ext-SubjectAltName | ext-IssuerAltName |
;;         ext-SubjectDirectoryAttributes |
;;         ext-BasicConstraints | ext-NameConstraints |
;;         ext-PolicyConstraints | ext-ExtKeyUsage |
;;         ext-CRLDistributionPoints | ext-InhibitAnyPolicy |
;;         ext-FreshestCRL | ext-AuthorityInfoAccess |
;;         ext-SubjectInfoAccessSyntax, ... }
(define oid oid-string->der-object-identifier)
(define *extension:authority-key-identifier* (oid "2.5.29.35"))
(define *extension:subject-key-identifier* (oid "2.5.29.14"))
(define *extension:key-usage* (oid "2.5.29.15"))
(define *extension:private-key-usage-period* (oid "2.5.29.16"))
(define *extension:certificate-policies* (oid "2.5.29.3"))
(define *extension:policy-mappings* (oid "2.5.29.33"))
(define *extension:subject-alt-name* (oid "2.5.29.17"))
(define *extension:issuer-alt-name* (oid "2.5.29.18"))
(define *extension:subject-directory-attributes* (oid "2.5.29.9"))
(define *extension:basic-constraints* (oid "2.5.29.19"))
(define *extension:name-constraints* (oid "2.5.29.30"))
(define *extension:policy-constraints* (oid "2.5.29.34"))
(define *extension:ext-key-usage* (oid "2.5.29.37"))
(define *extension:crl-distribution-points* (oid "2.5.29.25"))
(define *extension:inhibit-any-policy* (oid "2.5.29.54"))
(define *extension:freshest-crl* (oid "2.5.29.46"))
(define *extension:authority-info-access* (oid "1.3.6.1.5.5.7.1.1"))
(define *extension:subject-info-access* (oid "1.3.6.1.5.5.7.1.11"))

;;; CRL
;; CertificateList  ::=  SIGNED{TBSCertList}

;; TBSCertList  ::=  SEQUENCE  {
;;     version              Version OPTIONAL,
;;                                -- if present, MUST be v2
;;     signature            AlgorithmIdentifier{SIGNATURE-ALGORITHM,
;;                              {SignatureAlgorithms}},
;;     issuer               Name,
;;     thisUpdate           Time,
;;     nextUpdate           Time OPTIONAL,
;;     revokedCertificates  SEQUENCE SIZE (1..MAX) OF SEQUENCE {
;;         userCertificate  CertificateSerialNumber,
;;         revocationDate   Time,
;;         ... ,
;;         [[2:                  -- if present, version MUST be v2
;;         crlEntryExtensions  Extensions{{CrlEntryExtensions}}
;;                                 OPTIONAL
;;         ]], ...
;;     } OPTIONAL,
;;     ... ,
;;     [[2:                       -- if present, version MUST be v2
;;     crlExtensions       [0] Extensions{{CrlExtensions}}
;;                                 OPTIONAL
;;     ]], ... }
(define-asn1-encodable <revoked-certificate>
  (asn1-sequence
   ((user-certificate :type <der-integer>
		      :reader revoked-certificate-user-certificate)
    (revocation-date :type <asn1-time>
		     :reader revoked-certificate-revocation-date)
    (crl-entry-extensions :type <extensions> :optional #t))))
(define (revoked-certificate? o) (is-a? o <revoked-certificate>))

(define-asn1-encodable <revoked-certificates>
  (asn1-sequence
   (of :type <revoked-certificate> :reader revoked-certificates-elements)))
(define (revoked-certificates? o) (is-a? o <revoked-certificates>))
(define-asn1-encodable <tbs-cert-list>
  (asn1-sequence
   ((version :type <der-integer> :optional #t :reader tbs-cert-list-version)
    (signature :type <algorithm-identifier> :reader tbs-cert-list-signature)
    (issuer :type <name> :reader tbs-cert-list-issuer)
    (this-update :type <asn1-time> :reader tbs-cert-list-this-update)
    (next-update :type <asn1-time> :reader tbs-cert-list-next-update)
    (revoked-certificates :type <revoked-certificates> :optional #t
			  :reader tbs-cert-list-revoked-certificates)
    (crl-extensions :type <extensions> :tag 0 :explicit #t :optional #t
		    :reader tbs-cert-list-crl-extensions))))
(define (tbs-cert-list? o) (is-a? o <tbs-cert-list>))

;; CertificateList  ::=  SIGNED{TBSCertList}
(define-asn1-encodable <certificate-list>
  (asn1-sequence
   ((c :type <tbs-cert-list> :reader certificate-list-c)
    (algorithm :type <algorithm-identifier> :reader certificate-list-algorithm)
    (signature :type <der-bit-string> :reader certificate-list-signature))))
(define (certificate-list? o) (is-a? o <certificate-list>))

;; CrlExtensions EXTENSION ::= {
;;         ext-AuthorityKeyIdentifier | ext-IssuerAltName |
;;         ext-CRLNumber | ext-DeltaCRLIndicator |
;;         ext-IssuingDistributionPoint |  ext-FreshestCRL, ... }
(define *extension:crl-number* (oid "2.5.29.20"))
(define *extension:delta-crl-indicator* (oid "2.5.29.27"))
(define *extension:issuing-distribution-point* (oid "2.5.29.28"))

;; CrlEntryExtensions EXTENSION ::= {
;;         ext-CRLReason | ext-CertificateIssuer |
;;         ext-HoldInstructionCode | ext-InvalidityDate, ... }
(define *extension:crl-reason* (oid "2.5.29.21"))
(define *extension:certificate-issuer* (oid "2.5.29.29"))
(define *extension:hold-instruction-code* (oid "2.5.29.23"))
(define *extension:invalidity-date* (oid "2.5.29.24"))

;; CSR: PKCS#10
;; Attribute { ATTRIBUTE:IOSet } ::= SEQUENCE {
;;     type   ATTRIBUTE.&id({IOSet}),
;;     values SET SIZE(1..MAX) OF ATTRIBUTE.&Type({IOSet}{@type})
;; }
(define-asn1-encodable <attribute>
  (asn1-sequence
   ((type :type <der-object-identifier> :reader attribute-type)
    (values :type <ber-set> :reader attribute-values))))
(define (attribute? o) (is-a? o <attribute>))
;; Attributes { ATTRIBUTE:IOSet } ::= SET OF Attribute{{ IOSet }}
(define-asn1-encodable <attributes>
  (asn1-set
   (of :type <attribute> :reader attributes->list)))
(define (attributes? o) (is-a? o <attributes>))
;; CRIAttributes  ATTRIBUTE  ::= {
;;     ... -- add any locally defined attributes here -- }

;; -- Certificate requests
;; CertificationRequestInfo ::= SEQUENCE {
;;     version       INTEGER { v1(0) } (v1, ... ),
;;     subject       Name,
;;     subjectPKInfo SubjectPublicKeyInfo{{ PKInfoAlgorithms }},
;;     attributes    [0] Attributes{{ CRIAttributes }}
;; }
(define-asn1-encodable <certification-request-info>
  (asn1-sequence
   ((version :type <der-integer> :reader certification-request-info-version)
    (subject :type <name> :reader certification-request-info-name)
    (subject-pk-info :type <subject-public-key-info>
		     :reader certification-request-info-subject-pk-info)
    (attributes :type <attributes> :tag 0 :explicit #f
		:->collection make-der-set
		:reader certification-request-info-attributes))))
(define (certification-request-info? o) (is-a? o <certification-request-info>))

;; CertificationRequest ::= SEQUENCE {
;;     certificationRequestInfo  CertificationRequestInfo,
;;     signatureAlgorithm        AlgorithmIdentifier{SIGNATURE-ALGORITHM,
;;                                   { SignatureAlgorithms }},
;;     signature                 BIT STRING
;; }
(define-asn1-encodable <certification-request>
  (asn1-sequence
   ((certification-request-info :type <certification-request-info>
     :reader certification-request-certification-request-info)
    (signature-algorithm :type <algorithm-identifier>
			 :reader certification-request-signature-algorithm)
    (signature :type <der-bit-string>
	       :reader certification-request-signature))))
(define (certification-request? o) (is-a? o <certification-request>))
)
