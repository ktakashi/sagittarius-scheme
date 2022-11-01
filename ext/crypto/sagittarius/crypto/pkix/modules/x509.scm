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
    (export asn1-object->asn1-encodable ;; for convenience
	    bytevector->asn1-encodable

	    algorithm-identifier? <algorithm-identifier>
	    algorithm-identifier-algorithm algorithm-identifier-parameters

	    subject-public-key-info? <subject-public-key-info>
	    subject-public-key-info-algorithm
	    subject-public-key-info-subject-public-key

	    validity? <validity>
	    validity-not-before validity-not-after
	    
	    single-attribute? <single-attribute>
	    single-attribute-type single-attribute-value

	    relative-distinguished-name? <relative-distinguished-name>
	    relative-distinguished-name-components

	    *attribute:name*
	    *attribute:surname*
	    *attribute:given-name*
	    *attribute:initials*
	    *attribute:generation-qualifier*
	    *attribute:common-name*
	    *attribute:locality-name*
	    *attribute:state-or-province-name*
	    *attribute:organization-name*
	    *attribute:organization-unit-name*
	    *attribute:title*
	    *attribute:dn-qualifier*
	    *attribute:country-name*
	    *attribute:serial-number*
	    *attribute:pseudonym*
	    *attribute:domain-component*
	    *attribute:email-address*
	    *attribute:street-address*
	    *attribute:userid*

	    rdn-sequence? <rdn-sequence>
	    rdn-sequence->list
	    
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
	    certification-request-info-subject
	    certification-request-info-subject-pk-info
	    certification-request-info-attributes

	    certification-request? <certification-request>
	    certification-request-certification-request-info
	    certification-request-signature-algorithm
	    certification-request-signature

	    ;; These are basically for certificate/crl extensions
	    general-names? <general-names> general-names-components
	    general-name? <general-name> general-name-name general-name-type
	    directory-string? <directory-string> directory-string-value

	    authority-key-identifier? <authority-key-identifier>
	    authority-key-identifier-key-identifier
	    authority-key-identifier-authority-cert-issuer
	    authority-key-identifier-authority-cert-serial-number

	    private-key-usage-period? <private-key-usage-period>
	    private-key-usage-period-not-before
	    private-key-usage-period-not-after

	    user-notice? <user-notice>
	    user-notice-notice-ref
	    user-notice-explicit-text

	    notice-reference? <notice-reference>
	    notice-reference-organization
	    notice-reference-notice-numbers

	    display-text? <display-text>
	    display-text-string

	    *policy-qualifier-type:unotice*
	    *policy-qualifier-type:cps*

	    certificate-policies? <certificate-policies>
	    certificate-policies-policy-informations

	    policy-information? <policy-information>
	    policy-information-policy-identifier
	    policy-information-policy-qualifiers

	    policy-qualifier-info? <policy-qualifier-info>
	    policy-qualifier-info-policy-qualifier-id
	    policy-qualifier-info-qualifier

	    basic-constraints? <basic-constraints>
	    basic-constraints-ca
	    basic-constraints-path-length-constraint
	    
	    general-subtree? <general-subtree>
	    general-subtree-maximum
	    general-subtree-minimum

	    general-subtrees? <general-subtrees> general-subtrees->list

	    name-constraints? <name-constraints>
	    name-constraints-permitted-subtrees
	    name-constraints-excluded-subtrees
	    
	    policy-constraints? <policy-constraints>
	    policy-constraints-inhibit-policy-mapping
	    policy-constraints-require-explicit-policy
	    
	    ;; CSR attributes
	    *pkcs-9:channelge-password*
	    *pkcs-9:extenion-request*
	    *pkcs-9:extended-certificate-attributes*
	    )
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
       :reader relative-distinguished-name-components)))
(define (relative-distinguished-name? o)
  (is-a? o <relative-distinguished-name>))


;; SupportedAttributes ATTRIBUTE ::= {
;;     at-name | at-surname | at-givenName | at-initials |
;;     at-generationQualifier | at-x520CommonName |
;;     at-x520LocalityName | at-x520StateOrProvinceName |
;;     at-x520OrganizationName | at-x520OrganizationalUnitName |
;;     at-x520Title | at-x520dnQualifier | at-x520countryName |
;;     at-x520SerialNumber | at-x520Pseudonym | at-domainComponent |
;;     at-emailAddress, ... }
(define oid oid-string->der-object-identifier)
(define *attribute:name* (oid "2.5.4.41"))
(define *attribute:surname* (oid "2.5.4.4"))
(define *attribute:given-name* (oid "2.5.4.42"))
(define *attribute:initials* (oid "2.5.4.43"))
(define *attribute:generation-qualifier* (oid "2.5.4.44"))
(define *attribute:common-name* (oid "2.5.4.3"))
(define *attribute:locality-name* (oid "2.5.4.7"))
(define *attribute:state-or-province-name* (oid "2.5.4.8"))
(define *attribute:organization-name* (oid "2.5.4.10"))
(define *attribute:organization-unit-name* (oid "2.5.4.11"))
(define *attribute:title* (oid "2.5.4.12"))
(define *attribute:dn-qualifier* (oid "2.5.4.46"))
(define *attribute:country-name* (oid "2.5.4.6"))
(define *attribute:serial-number* (oid "2.5.4.5"))
(define *attribute:pseudonym* (oid "2.5.4.65"))
(define *attribute:domain-component* (oid "0.9.2342.19200300.100.1.25"))
(define *attribute:email-address* (oid "1.2.840.113549.1.9.1"))
;; Well RFC 1779 style require this...
(define *attribute:street-address* (oid "2.5.4.9"))
;; Well RFC 2253 style require this...
(define *attribute:userid* (oid "0.9.2342.19200300.100.1.1"))

;; Name ::= CHOICE { -- only one possibility for now --
;;     rdnSequence  RDNSequence }
;; RDNSequence ::= SEQUENCE OF RelativeDistinguishedName
(define-asn1-encodable <rdn-sequence>
  (asn1-sequence
   (of :type <relative-distinguished-name>
       :reader rdn-sequence->list)))
(define (rdn-sequence? o) (is-a? o <rdn-sequence>))

(define <name> <rdn-sequence>)
(define name? rdn-sequence?)
(define name-rdn* rdn-sequence->list)

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
(define *extension:authority-key-identifier* (oid "2.5.29.35"))
(define *extension:subject-key-identifier* (oid "2.5.29.14"))
(define *extension:key-usage* (oid "2.5.29.15"))
(define *extension:private-key-usage-period* (oid "2.5.29.16"))
(define *extension:certificate-policies* (oid "2.5.29.32"))
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

;; From RFC 5280
;; OtherName ::= SEQUENCE {
;;         type-id    OBJECT IDENTIFIER,
;;         value      [0] EXPLICIT ANY DEFINED BY type-id }
(define-asn1-encodable <other-name>
  (asn1-sequence
   ((type-id :type <der-object-identifier> :reader other-name-type-id)
    (value :type <asn1-encodable> :tag 0 :explicit #t
	   :reader other-name-value))))

;; DirectoryString{INTEGER:maxSize} ::= CHOICE {
;;     teletexString    TeletexString(SIZE (1..maxSize)),
;;     printableString  PrintableString(SIZE (1..maxSize)),
;;     bmpString        BMPString(SIZE (1..maxSize)),
;;     universalString  UniversalString(SIZE (1..maxSize)),
;;     uTF8String       UTF8String(SIZE (1..maxSize))
;; }
(define-asn1-encodable <directory-string>
  (asn1-choice
   ((teletex-string :type <der-t61-string>)
    (printable-string :type <der-printable-string>)
    (bmp-string :type <der-bmp-string>)
    (universal-string :type <der-universal-string>)
    (utf8-string :type <der-utf8-string>))
   :reader directory-string-value))
(define (directory-string? o) (is-a? o <directory-string>))
;; 
;; EDIPartyName ::= SEQUENCE {
;;     nameAssigner    [0] DirectoryString {ubMax} OPTIONAL,
;;     partyName       [1] DirectoryString {ubMax}
;; }
(define-asn1-encodable <edi-party-name>
  (asn1-sequence
   ((name-assigner :type <directory-string> :tag 0 :explicit #f :optional #t
		   :reader edi-party-name-name-assigner)
    (party-name :type <directory-string> :tag 1 :explicit #t
		:reader edi-party-name-party-name))))

;; GeneralName ::= CHOICE {
;;      otherName                   [0]  INSTANCE OF OTHER-NAME,
;;      rfc822Name                  [1]  IA5String,
;;      dNSName                     [2]  IA5String,
;;      x400Address                 [3]  ORAddress,
;;      directoryName               [4]  Name,
;;      ediPartyName                [5]  EDIPartyName,
;;      uniformResourceIdentifier   [6]  IA5String,
;;      iPAddress                   [7]  OCTET STRING,
;;      registeredID                [8]  OBJECT IDENTIFIER
;; }
;; -- AnotherName replaces OTHER-NAME ::= TYPE-IDENTIFIER, as
;; -- TYPE-IDENTIFIER is not supported in the '88 ASN.1 syntax
(define-asn1-encodable <general-name>
  (asn1-choice
   ((other-name :type <other-name> :tag 0 :explicit #f)
    (rfc822-name :type <der-ia5-string> :tag 1 :explicit #f
		 :converter bytevector->der-ia5-string)
    (dns-name :type <der-ia5-string> :tag 2 :explicit #f
	      :converter bytevector->der-ia5-string)
    ;; Sorry, we don't support its creation, if it's passed we handle
    ;; it's quite huge and never seen being used
    (x400-address :type <asn1-encodable> :tag 3 :explicit #f)
    ;; Name is choice so explicit
    (directory-name :type <name> :tag 4 :explicit #t)
    (edi-party-name :type <edi-party-name> :tag 5 :explicit #f)
    (uniform-resource-identifier :type <der-ia5-string> :tag 6 :explicit #f
				 :converter bytevector->der-ia5-string)
    (ip-address :type <der-octet-string> :tag 7 :explicit #f)
    (registered-id :type <der-object-identifier> :tag 8 :explicit #f
		   :converter bytevector->der-object-identifier))
   :reader general-name-name))
(define (general-name? o) (is-a? o <general-name>))
(define (general-name-type (general-name general-name?))
  (slot-ref general-name 'type))

;; GeneralNames ::= SEQUENCE SIZE (1..MAX) OF GeneralName
(define-asn1-encodable <general-names>
  (asn1-sequence
   (of :type <general-name> :reader general-names-components)))
(define (general-names? o) (is-a? o <general-names>))

;; AuthorityKeyIdentifier ::= SEQUENCE {
;;     keyIdentifier             [0] KeyIdentifier            OPTIONAL,
;;     authorityCertIssuer       [1] GeneralNames             OPTIONAL,
;;     authorityCertSerialNumber [2] CertificateSerialNumber  OPTIONAL }
(define-asn1-encodable <authority-key-identifier>
  (asn1-sequence
   ((key-identifier :type <der-octet-string> :tag 0 :optional #t :explicit #f
		    :reader authority-key-identifier-key-identifier)
    (authority-cert-issuer :type <general-names> :tag 1 :optional #t
      :explicit #f
      :reader authority-key-identifier-authority-cert-issuer)
    (authority-cert-serial-number :type <der-integer> :tag 2 :optional #t
      :explicit #f
      :reader authority-key-identifier-authority-cert-serial-number
      :converter bytevector->der-integer))))
(define (authority-key-identifier? o) (is-a? o <authority-key-identifier>))

;; PrivateKeyUsagePeriod ::= SEQUENCE {
;;      notBefore       [0]     GeneralizedTime OPTIONAL,
;;      notAfter        [1]     GeneralizedTime OPTIONAL }
;; (WITH COMPONENTS {..., notBefore  PRESENT } |
;;  WITH COMPONENTS {..., notAfter  PRESENT })
(define-asn1-encodable <private-key-usage-period>
  (asn1-sequence
   ((not-before :type <der-generalized-time> :tag 0 :explicit #f :optional #t
		:reader private-key-usage-period-not-before
		:converter bytevector->der-generalized-time)
    (not-after :type <der-generalized-time> :tag 1 :explicit #f :optional #t
	       :reader private-key-usage-period-not-after
	       :converter bytevector->der-generalized-time))))
(define (private-key-usage-period? o) (is-a? o <private-key-usage-period>))

;; CERT-POLICY-QUALIFIER ::= TYPE-IDENTIFIER

;; PolicyQualifierInfo ::= SEQUENCE {
;;        policyQualifierId  CERT-POLICY-QUALIFIER.
;;             &id({PolicyQualifierId}),
;;        qualifier          CERT-POLICY-QUALIFIER.
;;             &Type({PolicyQualifierId}{@policyQualifierId})}
(define-asn1-encodable <policy-qualifier-info>
  (asn1-sequence
   ((policy-qualifier-id :type <der-object-identifier>
			 :reader policy-qualifier-info-policy-qualifier-id)
    (qualifier :reader policy-qualifier-info-qualifier))))
(define (policy-qualifier-info? o) (is-a? o <policy-qualifier-info>))

;; PolicyInformation ::= SEQUENCE {
;;      policyIdentifier   CertPolicyId,
;;      policyQualifiers   SEQUENCE SIZE (1..MAX) OF
;;              PolicyQualifierInfo OPTIONAL }
;; CertPolicyId ::= OBJECT IDENTIFIER
(define-asn1-encodable <policy-information>
  (asn1-sequence
   ((policy-identifier :type <der-object-identifier>
		       :reader policy-information-policy-identifier)
    (policy-qualifiers :type <policy-qualifier-info> :optional #t
		       :multiple 'sequence
		       :reader policy-information-policy-qualifiers))))
(define (policy-information? o) (is-a? o <policy-information>))

;; CertificatePolicies ::= SEQUENCE SIZE (1..MAX) OF PolicyInformation
(define-asn1-encodable <certificate-policies>
  (asn1-sequence
   (of :type <policy-information>
       :reader certificate-policies-policy-informations)))
(define (certificate-policies? o) (is-a? o <certificate-policies>))

;; -- Implementations that recognize additional policy qualifiers MUST
;; -- augment the following definition for PolicyQualifierId

;; PolicyQualifierId CERT-POLICY-QUALIFIER ::=
;;     { pqid-cps | pqid-unotice, ... }
;; pqid-cps CERT-POLICY-QUALIFIER ::= { CPSuri IDENTIFIED BY id-qt-cps }
;; pqid-unotice CERT-POLICY-QUALIFIER ::= { UserNotice
;;     IDENTIFIED BY id-qt-unotice }
(define *policy-qualifier-type:cps* (oid "1.3.6.1.5.5.7.2.1"))
(define *policy-qualifier-type:unotice* (oid "1.3.6.1.5.5.7.2.2"))

;; BasicConstraints ::= SEQUENCE {
;;      cA                      BOOLEAN DEFAULT FALSE,
;;      pathLenConstraint       INTEGER (0..MAX) OPTIONAL
;; }
(define-asn1-encodable <basic-constraints>
  (asn1-sequence
   ((ca :type <der-boolean> :optional #t :reader basic-constraints-ca)
    (path-length-constraint :type <der-integer> :optional #t
			    :reader basic-constraints-path-length-constraint))))
(define (basic-constraints? o) (is-a? o <basic-constraints>))

;; GeneralSubtree ::= SEQUENCE {
;;      base                GeneralName,
;;      minimum         [0] BaseDistance DEFAULT 0,
;;      maximum         [1] BaseDistance OPTIONAL
;; }
;; BaseDistance ::= INTEGER (0..MAX)
(define-asn1-encodable <general-subtree>
  (asn1-sequence
   ((base :type <general-name> :reader general-subtree-base)
    (minimum :type <der-integer> :tag 0 :optional #t :explicit #f
	     :reader general-subtree-minimum
	     :converter bytevector->der-integer)
    (maximum :type <der-integer> :tag 1 :optional #t :explicit #f
	     :reader general-subtree-maximum
	     :converter bytevector->der-integer))))
(define (general-subtree? o) (is-a? o <general-subtree>))
;; GeneralSubtrees ::= SEQUENCE SIZE (1..MAX) OF GeneralSubtree
(define-asn1-encodable <general-subtrees>
  (asn1-sequence
   (of :type <general-subtree> :reader general-subtrees->list)))
(define (general-subtrees? o) (is-a? o <general-subtrees>))

;; NameConstraints ::= SEQUENCE {
;;      permittedSubtrees       [0] GeneralSubtrees OPTIONAL,
;;      excludedSubtrees        [1] GeneralSubtrees OPTIONAL
;; }
;; --
;; --  This is a constraint in the issued certificates by CAs, but is
;; --  not a requirement on EEs.
;; --
;; -- (WITH COMPONENTS { ..., permittedSubtrees PRESENT} |
;; --  WITH COMPONENTS { ..., excludedSubtrees PRESENT }}
(define-asn1-encodable <name-constraints>
  (asn1-sequence
   ((permitted-subtrees :type <general-subtrees> :tag 0 :optional #t
			:explicit #f
			:reader name-constraints-permitted-subtrees)
    (excluded-subtrees :type <general-subtrees> :tag 0 :optional #t :explicit #f
		       :reader name-constraints-excluded-subtrees))))
(define (name-constraints? o) (is-a? o <name-constraints>))

;; PolicyConstraints ::= SEQUENCE {
;;      requireExplicitPolicy           [0] SkipCerts OPTIONAL,
;;      inhibitPolicyMapping            [1] SkipCerts OPTIONAL }
;; --
;; --  This is a constraint in the issued certificates by CAs,
;; --  but is not a requirement for EEs
;; --
;; -- (WITH COMPONENTS { ..., requireExplicitPolicy PRESENT} |
;; --  WITH COMPONENTS { ..., inhibitPolicyMapping PRESENT})
;; SkipCerts ::= INTEGER (0..MAX)
(define-asn1-encodable <policy-constraints>
  (asn1-sequence
   ((require-explicit-policy :type <der-integer> :tag 0 :optional #t
     :explicit #f
     :reader policy-constraints-require-explicit-policy
     :converter bytevector->der-integer)
    (inhibit-policy-mapping :type <der-integer> :tag 1 :optional #t
     :explicit #f
     :reader policy-constraints-inhibit-policy-mapping
     :converter bytevector->der-integer))))
(define (policy-constraints? o) (is-a? o <policy-constraints>))

;; -- CPS pointer qualifier
;; CPSuri ::= IA5String

;; DisplayText ::= CHOICE {
;;      ia5String        IA5String      (SIZE (1..200)),
;;      visibleString    VisibleString  (SIZE (1..200)),
;;      bmpString        BMPString      (SIZE (1..200)),
;;      utf8String       UTF8String     (SIZE (1..200)) }
(define-asn1-encodable <display-text>
  (asn1-choice
   ((ia5-string :type <der-ia5-string>)
    (visiable-string :type <der-videotex-string>)
    (bmp-string :type <der-bmp-string>)
    (utf8-string :type <der-utf8-string>))
   :reader display-text-string))
(define (display-text? o) (is-a? o <display-text>))
;; NoticeReference ::= SEQUENCE {
;;      organization     DisplayText,
;;      noticeNumbers    SEQUENCE OF INTEGER }
(define-asn1-encodable <notice-reference>
  (asn1-sequence
   ((organization :type <display-text> :reader notice-reference-organization)
    (notice-numbers :type <der-integer> :multiple 'sequence
		    :reader notice-reference-notice-numbers))))
(define (notice-reference? o) (is-a? o <notice-reference>))

;; -- user notice qualifier
;; UserNotice ::= SEQUENCE {
;;      noticeRef        NoticeReference OPTIONAL,
;;      explicitText     DisplayText OPTIONAL}
(define-asn1-encodable <user-notice>
  (asn1-sequence
   ((notice-ref :type <notice-reference> :optional #t
		:reader user-notice-notice-ref)
    (explicit-text :type <display-text> :optional #t
		   :reader user-notice-explicit-text))))
(define (user-notice? o) (is-a? o <user-notice>))

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
    (subject :type <name> :reader certification-request-info-subject)
    (subject-pk-info :type <subject-public-key-info>
		     :reader certification-request-info-subject-pk-info)
    (attributes :type <attributes> :tag 0 :explicit #f
		:converter make-der-set
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

;; Attributes
;; ref - https://datatracker.ietf.org/doc/html/rfc2985
;; For now we don't implement everything (the same as extension)
;; 5.4 Attribute types for use with PKCS #10 certificate requests

;; challengePassword ATTRIBUTE ::= {
;;         WITH SYNTAX DirectoryString {pkcs-9-ub-challengePassword}
;;         EQUALITY MATCHING RULE caseExactMatch
;;         SINGLE VALUE TRUE
;;         ID pkcs-9-at-challengePassword
;; }
;; This attribute is obsolated more or less, no effect nowadays
(define *pkcs-9:channelge-password* (oid "1.2.840.113549.1.9.7"))

;; extensionRequest ATTRIBUTE ::= {
;;         WITH SYNTAX ExtensionRequest
;;         SINGLE VALUE TRUE
;;         ID pkcs-9-at-extensionRequest
;; }
(define *pkcs-9:extenion-request* (oid "1.2.840.113549.1.9.14"))

;; extendedCertificateAttributes ATTRIBUTE ::= {
;;         WITH SYNTAX SET OF Attribute
;;         SINGLE VALUE TRUE
;;         ID pkcs-9-at-extendedCertificateAttributes
;; }
(define *pkcs-9:extended-certificate-attributes* (oid "1.2.840.113549.1.9.9"))
)
