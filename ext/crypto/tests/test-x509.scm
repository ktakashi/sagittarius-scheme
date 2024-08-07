(import (rnrs)
	(sagittarius crypto pkix certificate)
	(sagittarius crypto pkix request)
	(sagittarius crypto pkix revocation)
	(sagittarius crypto pkix algorithms)
	(sagittarius crypto pkix signatures)
	(sagittarius crypto pkix extensions)
	;; for testing APIs
	(sagittarius crypto pkix extensions alt-names)
	(sagittarius crypto pkix extensions cps)
	(sagittarius crypto pkix extensions key-usage)
	(sagittarius crypto pkix extensions constraints)
	(sagittarius crypto keys)
	(sagittarius crypto signatures)
	(sagittarius crypto asn1)
	(sagittarius crypto digests)
	(rfc base64)
	(srfi :19)
	(srfi :64))

(test-begin "X.509 certificate")

(define cert
  (base64-decode-string
   (string-append
    "MIICcDCCAhWgAwIBAgIUOJ0BDDovxF2W3+0Kndg1cl9q5NQwCgYIKoZIzj0EAwIw"
    "gYwxCzAJBgNVBAYTAk5MMRUwEwYDVQQIDAxadWlkLUhvbGxhbmQxDzANBgNVBAcM"
    "BkxlaWRlbjEbMBkGA1UECgwSU2FnaXR0YXJpdXMgU2NoZW1lMRUwEwYDVQQDDAxU"
    "YWthc2hpIEthdG8xITAfBgkqhkiG9w0BCQEWEmt0YWthc2hpQHltYWlsLmNvbTAe"
    "Fw0yMjEwMjQxODM2MjhaFw0zMjEwMjExODM2MjhaMIGMMQswCQYDVQQGEwJOTDEV"
    "MBMGA1UECAwMWnVpZC1Ib2xsYW5kMQ8wDQYDVQQHDAZMZWlkZW4xGzAZBgNVBAoM"
    "ElNhZ2l0dGFyaXVzIFNjaGVtZTEVMBMGA1UEAwwMVGFrYXNoaSBLYXRvMSEwHwYJ"
    "KoZIhvcNAQkBFhJrdGFrYXNoaUB5bWFpbC5jb20wWTATBgcqhkjOPQIBBggqhkjO"
    "PQMBBwNCAAQdBG9xFor8DDnduyf9S1oJvX7Z7fZnZlbyen0/vXD/CghqKJRrgins"
    "yUb4D2cp0N/gvKX5b1Lkmw5dLCD02bJXo1MwUTAdBgNVHQ4EFgQUgm9i/kQ+u/KT"
    "zBcddqH9Gy8tT48wHwYDVR0jBBgwFoAUgm9i/kQ+u/KTzBcddqH9Gy8tT48wDwYD"
    "VR0TAQH/BAUwAwEB/zAKBggqhkjOPQQDAgNJADBGAiEAr3vM7SgzcSdsJ25chY5G"
    "DdYbGazEK4PU6oJRGj2RXCACIQDbm8/zohffUKa2zYJ77NUVEN4+oZquRW/ZMg1i"
    "UeWU6g==")
   :transcoder #f))

(define alt-names
  (x509-general-names
   ;; I want my own domain for this :D
   (dns-name->general-name "*.example.com")
   (rfc822-name->general-name "ktakashi@ymail.com")
   (ip-address->general-name #vu8(127 0 0 1))
   (directory-name->general-name (x509-name '(O "bla")))
   (registered-id->general-name "1.2.3.4")))

(test-assert (x509-general-name? (dns-name->general-name "*.example.com")))
(test-assert (x509-general-name? (rfc822-name->general-name "ktakashi@ymail.com")))
(test-assert (x509-general-name? (ip-address->general-name #vu8(127 0 0 1))))
(test-assert (x509-general-name? (directory-name->general-name (x509-name '(O "bla")))))
(test-assert (x509-general-name? (registered-id->general-name "1.2.3.4")))

(test-assert (x509-general-names? alt-names))
(test-assert (x509-authority-key-identifier?
	      (make-x509-authority-key-identifier :key-identifier #vu8(1))))
(test-error
 (make-x509-authority-key-identifier :authority-cert-serial-number 0))

(let ((x509-cert (read-x509-certificate (open-bytevector-input-port cert))))
  (test-assert (x509-certificate? x509-cert))
  (test-equal 3 (x509-certificate-version x509-cert))
  (test-assert (x509-name? (x509-certificate-issuer-dn x509-cert)))
  (test-assert (x509-name? (x509-certificate-subject-dn x509-cert)))
  (let ((validity (x509-certificate-validity x509-cert)))
    (test-assert (x509-validity? validity))
    (test-assert (date? (x509-validity-not-before validity)))
    (test-assert (date? (x509-validity-not-after validity))))
  (test-assert (public-key? (x509-certificate-public-key x509-cert)))
  (let-values (((out e) (open-bytevector-output-port)))
    (let ((bport (open-base64-encode-output-port out)))
      (write-x509-certificate x509-cert bport)
      (close-output-port bport)
      (test-equal (base64-encode cert :line-width #f) (e)))))

(define now (current-time))
(define subject-dn (x509-name '(C "NL")
			      '(ST "Zuid-Holland")
			      '(OU "Sagittarius Scheme")
			      '(CN "Takashi Kato")
			      '(E "ktakashi@ymail.com")))
(define (->key-operation algorithm)
  (oid->key-operation
   (if (string? algorithm)
       algorithm
       (x509-algorithm-identifier-oid algorithm))))
(define (test-certificate-builder algorithm)
  (define one-year (make-time time-duration 0 (* 3600 24 365)))
  (define one-second (make-time time-duration 0 1))
  (define key-scheme (->key-operation algorithm))
  (define issuer-dn (x509-name '(C "NL")
			       '(ST "Zuid-Holland")
			       '(L "Leiden")
			       '(OU "Sagittarius Scheme")
			       '(CN "Takashi Kato")))
  (let* ((signing-key-pair (generate-key-pair key-scheme))
	 (template (x509-certificate-template-builder
		    (issuer-dn issuer-dn)
		    (subject-dn subject-dn)
		    (serial-number 1000)
		    (not-before (time-utc->date now))
		    (not-after (time-utc->date (add-duration now one-year)))
		    (public-key (key-pair-public signing-key-pair))
		    (extensions
		     (list
		      (make-x509-subject-alternative-name-extension alt-names)
		      (make-x509-issuer-alternative-name-extension alt-names)
		      (make-x509-authority-key-identifier-extension
		       (make-x509-authority-key-identifier
			:key-identifier #vu8(1 2 3 4 5)))
		      (make-x509-subject-key-identifier-extension
		       #vu8(2 3 4 5 6))
		      (make-x509-key-usage-extension
		       (x509-key-usages digital-signature
					key-encipherment
					key-agreement
					decipher-only)
		       #t)
		      (make-x509-private-key-usage-period-extension
		       (make-x509-private-key-usage-period
			:not-before (time-utc->date now))
		       #t)
		      (make-x509-certificate-policies-extension
		       #f
		       (make-x509-policy-information
			"1.3.6.1.4.1.999999.1.1.1"
			(make-x509-policy-qualifier-info
			 *policy-qualifier-type:cps*
			 "http://cps.example.com")))
		      (make-x509-basic-constraints-extension
		       (make-x509-basic-constraints :ca #f))
		      ))))
	 (failing-key-pair (generate-key-pair key-scheme)))
    (test-assert (x509-certificate-template? template))
    (let ((cert (sign-x509-certificate-template
		 template algorithm (key-pair-private signing-key-pair))))
      (test-assert (x509-certificate? cert))
      (test-assert algorithm
		   (validate-x509-certificate cert
		    (x509-certificate-validity-validator)
		    (x509-certificate-signature-validator 
		     (key-pair-public signing-key-pair))))
      (test-error "Validity before"
		  ((x509-certificate-validity-validator 
		    (time-utc->date (subtract-duration now one-second)))
		   cert))
      (let ((expired-date (time-utc->date
			   (add-duration (add-duration now one-year)
					 one-second))))
	(test-assert (x509-certificate-expired? cert expired-date))
	(test-error "Validity after"
		    ((x509-certificate-validity-validator expired-date) cert)))
      (test-error "Signature verificateion"
		  ((x509-certificate-signature-validator
		    (key-pair-public failing-key-pair)) cert))
      cert)))

(let* ((cert (bytevector->x509-certificate
	      (x509-certificate->bytevector
	       (test-certificate-builder
		*signature-algorithm:ecdsa-sha256*))))
       (extensions (x509-certificate-extensions cert)))

  (define (test-extension extensions oid critical value)
    (let ((x (find (x509-extension-of oid) extensions)))
      (test-assert (x509-extension? x))
      (test-assert (der-object-identifier->oid-string oid)
		   (x509-extension-id x))
      (test-equal critical (x509-extension-critical? x))
      (test-equal oid value (x509-extension-value x))
      x))
  
  (define (ensure-raw-asn1-object o)
    (bytevector->asn1-object (asn1-encodable->bytevector o)))
  
  (let ((e (test-extension extensions *extension:subject-alt-name* #f
			   (ensure-raw-asn1-object
			    (x509-general-names->general-names alt-names)))))
    (test-assert (x509-general-names? (x509-subject-alternative-name-extension->x509-general-names e)))
    (test-error (x509-general-names? (x509-issuer-alternative-name-extension->x509-general-names e))))
  (let ((e (test-extension extensions *extension:issuer-alt-name* #f
			   (ensure-raw-asn1-object
			    (x509-general-names->general-names alt-names)))))
    (test-assert (x509-general-names? (x509-issuer-alternative-name-extension->x509-general-names e)))
    (test-error (x509-general-names? (x509-subject-alternative-name-extension->x509-general-names e))))
  (let ((e (test-extension extensions *extension:authority-key-identifier* #f
			   (ensure-raw-asn1-object
			    (x509-authority-key-identifier->authority-key-identifier
			     (make-x509-authority-key-identifier
			      :key-identifier #vu8(1 2 3 4 5)))))))
    (test-assert (x509-authority-key-identifier?
		  (x509-authority-key-identifier-extension->x509-authority-key-identifier e))))

  (let ((e (test-extension extensions *extension:subject-key-identifier* #f
			   (bytevector->der-octet-string #vu8(2 3 4 5 6)))))
    (test-assert (bytevector? (x509-subject-key-identifier-extension->subject-key-identifier e)))
    (test-equal #vu8(2 3 4 5 6) (x509-subject-key-identifier-extension->subject-key-identifier e)))

  (let ((key-usage-extension
	 (test-extension extensions *extension:key-usage* #t
			 (bytevector->der-bit-string #vu8(#x80 #xA8)))))
    (test-equal '(digital-signature key-encipherment
		  key-agreement decipher-only)
		(enum-set->list
		 (x509-key-usage-extension->x509-key-usages
		  key-usage-extension))))

  (let ((e (test-extension extensions *extension:private-key-usage-period* #t
	    (ensure-raw-asn1-object
	     (x509-private-key-usage-period->private-key-usage-period
	      (make-x509-private-key-usage-period
	       :not-before (time-utc->date now)))))))
    (test-assert (x509-private-key-usage-period?
		  (x509-private-key-usage-period-extension->x509-private-key-usage-period e))))

  (let ((e (test-extension extensions *extension:certificate-policies* #f
	    (ensure-raw-asn1-object
	     (der-sequence
	      (x509-policy-information->policy-information
	       (make-x509-policy-information
		"1.3.6.1.4.1.999999.1.1.1"
		(make-x509-policy-qualifier-info
		 *policy-qualifier-type:cps*
		 "http://cps.example.com"))))))))
    (test-assert (for-all x509-policy-information?
			  (x509-certificate-policies-extension->x509-policy-informations e))))

  (let ((e (test-extension extensions *extension:basic-constraints* #f
	    (ensure-raw-asn1-object
	     (x509-basic-constraints->basic-constraints
	      (make-x509-basic-constraints))))))
    (test-assert (x509-basic-constraints?
		  (x509-basic-constraints-extension->x509-basic-constraints e))))
  )

(test-certificate-builder (make-x509-algorithm-identifier
			   *signature-algorithm:rsa-ssa-pss*
			   (make-x509-rsassa-pss-param)))
(test-certificate-builder (make-x509-algorithm-identifier
			   *signature-algorithm:rsa-ssa-pss*
			   (make-x509-rsassa-pss-param :salt-length 128)))
(test-certificate-builder (make-x509-algorithm-identifier
			   *signature-algorithm:rsa-ssa-pss*
			   (make-x509-rsassa-pss-param
			    :digest *digest:sha-256*
			    :mgf-digest *digest:sha-256*)))

(for-each test-certificate-builder
	  (list *signature-algorithm:rsa-pkcs-v1.5-sha1*
		*signature-algorithm:rsa-pkcs-v1.5-sha256*
		*signature-algorithm:rsa-pkcs-v1.5-sha384*
		*signature-algorithm:rsa-pkcs-v1.5-sha512*
		*signature-algorithm:rsa-pkcs-v1.5-sha224*
		*signature-algorithm:rsa-pkcs-v1.5-sha512/224*
		*signature-algorithm:rsa-pkcs-v1.5-sha512/256*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-224*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-256*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-384*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-512*
		*signature-algorithm:dsa-sha224*
		*signature-algorithm:dsa-sha256*
		;; Key size doesn't fit...
		;; Not sure why these are defined even as max size of DSA
		;; key is 2048...
		;; *signature-algorithm:dsa-sha384*
		;; *signature-algorithm:dsa-sha512*
		*signature-algorithm:ecdsa-sha1*
		*signature-algorithm:ecdsa-sha224*
		*signature-algorithm:ecdsa-sha256*
		*signature-algorithm:ecdsa-sha384*
		*signature-algorithm:ecdsa-sha512*
		*signature-algorithm:ecdsa-sha3-224*
		*signature-algorithm:ecdsa-sha3-256*
		*signature-algorithm:ecdsa-sha3-384*
		*signature-algorithm:ecdsa-sha3-512*
		*signature-algorithm:ed25519*
		*signature-algorithm:ed448*))

;; CSR
(define csr
  (base64-decode-string
   (string-append
    "MIIBgjCCAScCAQAwgYwxCzAJBgNVBAYTAk5MMRUwEwYDVQQIDAxadWlkLUhvbGxh"
    "bmQxDzANBgNVBAcMBkxlaWRlbjEbMBkGA1UECgwSU2FnaXR0YXJpdXMgU2NoZW1l"
    "MRUwEwYDVQQDDAxUYWthc2hpIEthdG8xITAfBgkqhkiG9w0BCQEWEmt0YWthc2hp"
    "QHltYWlsLmNvbTBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABNXAN+ibFJZ2dUw7"
    "0icY4iseONCTdKfQFygCo6x0AeM5baD8FBkfMgQJhKx9HNh1Wy53KJNP80qqz1kI"
    "yye+GCSgODATBgkqhkiG9w0BCQcxBgwEdGVzdDAhBgkqhkiG9w0BCQIxFAwSU2Fn"
    "aXR0YXJpdXMgU2NoZW1lMAoGCCqGSM49BAMCA0kAMEYCIQCzLwBT9zmperdFIt6w"
    "WUQ6xs4nTQYTHBDIRRwJCCMrDgIhAP21hfe1gRR+HgHfBxvPVaXFZaEpEAWrMCsl"
    "Kxtw7xvg")
   :transcoder #f))

(test-assert (x509-certification-request?
	      (bytevector->x509-certification-request csr)))
(let ((x509-csr (bytevector->x509-certification-request csr)))
  (test-assert (validate-x509-certification-request x509-csr
		x509-certification-request-signature-validator)))

(define (test-certification-request-builder algorithm)
  (let ((cr-tmpl (x509-certification-request-template-builder
		  (subject-dn subject-dn)
		  (attributes (list
			       (make-x509-extension-request-attribute
				(make-x509-key-usage-extension
				 (x509-key-usages digital-signature
						  key-encipherment)))))))
	(key-pair (generate-key-pair (->key-operation algorithm))))
    (test-assert (x509-certification-request-template? cr-tmpl))
    (let ((cr (sign-x509-certification-request-template
	       cr-tmpl algorithm key-pair)))
      (test-assert (x509-certification-request? cr))
      (test-assert (validate-x509-certification-request cr
		    x509-certification-request-signature-validator))
      (test-equal 1 (x509-certification-request-version cr))
      (test-equal subject-dn (x509-certification-request-subject cr))
      (let ((bv (x509-certification-request->bytevector cr)))
	(test-assert (x509-certification-request?
		      (bytevector->x509-certification-request bv)))))))

(test-certification-request-builder (make-x509-algorithm-identifier
				     *signature-algorithm:rsa-ssa-pss*
				     (make-x509-rsassa-pss-param)))
(test-certification-request-builder (make-x509-algorithm-identifier
				     *signature-algorithm:rsa-ssa-pss*
				     (make-x509-rsassa-pss-param
				      :salt-length 128)))
(test-certification-request-builder (make-x509-algorithm-identifier
				     *signature-algorithm:rsa-ssa-pss*
				     (make-x509-rsassa-pss-param
				      :digest *digest:sha-256*
				      :mgf-digest *digest:sha-256*)))

(for-each test-certification-request-builder
	  (list *signature-algorithm:rsa-pkcs-v1.5-sha1*
		*signature-algorithm:rsa-pkcs-v1.5-sha256*
		*signature-algorithm:rsa-pkcs-v1.5-sha384*
		*signature-algorithm:rsa-pkcs-v1.5-sha512*
		*signature-algorithm:rsa-pkcs-v1.5-sha224*
		*signature-algorithm:rsa-pkcs-v1.5-sha512/224*
		*signature-algorithm:rsa-pkcs-v1.5-sha512/256*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-224*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-256*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-384*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-512*
		*signature-algorithm:dsa-sha224*
		*signature-algorithm:dsa-sha256*
		;; The same reason
		;; *signature-algorithm:dsa-sha384*
		;; *signature-algorithm:dsa-sha512*
		*signature-algorithm:ecdsa-sha1*
		*signature-algorithm:ecdsa-sha224*
		*signature-algorithm:ecdsa-sha256*
		*signature-algorithm:ecdsa-sha384*
		*signature-algorithm:ecdsa-sha512*
		*signature-algorithm:ecdsa-sha3-224*
		*signature-algorithm:ecdsa-sha3-256*
		*signature-algorithm:ecdsa-sha3-384*
		*signature-algorithm:ecdsa-sha3-512*
		*signature-algorithm:ed25519*
		*signature-algorithm:ed448*))

;; CRL
(define 1d (make-time time-duration 0 (* 3600 24)))
(define nowd (time-utc->date now))
(define now-1d (time-utc->date (subtract-duration now 1d)))
(define now+1d (time-utc->date (add-duration now 1d)))
(define (test-crl-builder algorithm)
  (let* ((tmpl (x509-certificate-revocation-list-template-builder
		(issuer-dn subject-dn)
		(this-update nowd)
		(revoked-certificates
		 (list (make-x509-revoked-certificate 1 nowd)))))
	 (key-pair (generate-key-pair (->key-operation algorithm)))
	 (private-key (key-pair-private key-pair))
	 (cert-templ (x509-certificate-template-builder
		      (issuer-dn subject-dn)
		      (subject-dn subject-dn)
		      (serial-number 1)
		      (public-key (key-pair-public key-pair))
		      (not-before now-1d)
		      (not-after now+1d)))
	 (cert (sign-x509-certificate-template cert-templ algorithm private-key)))
    (test-assert (x509-certificate-revocation-list-template? tmpl))
    (let ((crl (sign-x509-certificate-revocation-list-template
		tmpl algorithm private-key)))
      (test-assert (x509-certificate-revocation-list? crl))
      (test-assert (x509-certificate-revoked? cert crl))
      (test-assert (validate-x509-certificate-revocation-list crl
		    (x509-certificate-revocation-list-signature-validator
		     (key-pair-public key-pair))
		    (x509-certificate-revocation-list-issuer-validator
		     subject-dn)))
      
      (test-assert (x509-certificate-revocation-list?
		    (bytevector->x509-certificate-revocation-list
		     (x509-certificate-revocation-list->bytevector crl)))))))

(test-crl-builder (make-x509-algorithm-identifier
		   *signature-algorithm:rsa-ssa-pss*
		   (make-x509-rsassa-pss-param)))
(test-crl-builder (make-x509-algorithm-identifier
		   *signature-algorithm:rsa-ssa-pss*
		   (make-x509-rsassa-pss-param
		    :salt-length 128)))
(test-crl-builder (make-x509-algorithm-identifier
		   *signature-algorithm:rsa-ssa-pss*
		   (make-x509-rsassa-pss-param
		    :digest *digest:sha-256*
		    :mgf-digest *digest:sha-256*)))

(for-each test-crl-builder
	  (list *signature-algorithm:rsa-pkcs-v1.5-sha1*
		*signature-algorithm:rsa-pkcs-v1.5-sha256*
		*signature-algorithm:rsa-pkcs-v1.5-sha384*
		*signature-algorithm:rsa-pkcs-v1.5-sha512*
		*signature-algorithm:rsa-pkcs-v1.5-sha224*
		*signature-algorithm:rsa-pkcs-v1.5-sha512/224*
		*signature-algorithm:rsa-pkcs-v1.5-sha512/256*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-224*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-256*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-384*
		*signature-algorithm:rsa-pkcs-v1.5-sha3-512*
		*signature-algorithm:dsa-sha224*
		*signature-algorithm:dsa-sha256*
		;; The same reason
		;; *signature-algorithm:dsa-sha384*
		;; *signature-algorithm:dsa-sha512*
		*signature-algorithm:ecdsa-sha1*
		*signature-algorithm:ecdsa-sha224*
		*signature-algorithm:ecdsa-sha256*
		*signature-algorithm:ecdsa-sha384*
		*signature-algorithm:ecdsa-sha512*
		*signature-algorithm:ecdsa-sha3-224*
		*signature-algorithm:ecdsa-sha3-256*
		*signature-algorithm:ecdsa-sha3-384*
		*signature-algorithm:ecdsa-sha3-512*
		*signature-algorithm:ed25519*
		*signature-algorithm:ed448*))

(let ()
  (define key-pair (generate-key-pair *key:ecdsa*))
  (define template
    (x509-certificate-revocation-list-template-builder
     (issuer-dn (x509-name '(CN "test")))
     (this-update (current-date))
     (revoked-certificates '())))
  
  (define aid (make-x509-algorithm-identifier
	       *signature-algorithm:ecdsa-sha256*))
  (let ((crl (sign-x509-certificate-revocation-list-template
	      template aid (key-pair-private key-pair))))
    (test-assert (x509-name? (x509-certificate-revocation-list-issuer crl)))
    (test-assert (date? (x509-certificate-revocation-list-this-update crl)))
    (test-equal '() (x509-certificate-revocation-list-revoked-certificates crl))
    (test-equal #f (x509-certificate-revocation-list-crl-extensions crl))
    (test-equal #f (x509-certificate-revocation-list-next-update crl))))

(define (check-dn dn)
  (test-equal dn (x509-name->string (string->x509-name dn))))
(check-dn (x509-name->string subject-dn))
(check-dn "C=NL,OU=Sagittarius\\,Scheme,CN=ktakashi@ymail.com")

(test-end)
