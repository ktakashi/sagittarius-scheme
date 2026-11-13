(import (rnrs)
	(clos user)
	(sagittarius crypto asn1)
	(sagittarius crypto asn1 modules)
	(sagittarius crypto ciphers)
	(sagittarius crypto keys)
	(sagittarius crypto x509)
	(sagittarius crypto digests)
	(sagittarius crypto signatures)
	(sagittarius crypto pkcs keystore)
	(sagittarius crypto pkcs keys)
	(sagittarius crypto pkcs pbes) ;; to enable PBES2 and so
	(sagittarius crypto pkcs modules cms)
	(sagittarius crypto pkcs modules pbes)
	(sagittarius crypto pkcs modules pfx)
	(sagittarius crypto pkix algorithms)
	(rfc base64)
	(util file)
	(srfi :1)
	(srfi :19)
	(srfi :64))

(define p12
  (let-values (((dir base ext) (decompose-path (current-load-path))))
    (build-path* dir "data" "test.p12")))

(test-begin "PKCS#12")

(let ((ks (call-with-input-file p12
	    (lambda (in) (read-pkcs12-keystore "storepass" in))
	    :transcoder #f)))
  (test-assert (pkcs12-keystore? ks))
  ;; the file contains of 'aes', 'google' and 'key pair'
  ;; as secret key, certificate and encrypted-private-key
  ;; NOTE: 'key pair' contains key and cert
  (let ((friendly-names (pkcs12-keystore-friendly-names ks)))
    (test-assert (hashtable? friendly-names))
    (test-equal '(secret-key) (hashtable-ref friendly-names "aes" #f))
    (test-equal '(certificate) (hashtable-ref friendly-names "google" #f))
    ;; The order is unspecified, so has to be compared with lset=
    (test-assert (lset= eq? '(encrypted-private-key certificate)
			(hashtable-ref friendly-names "key pair" #f))))

  (test-assert (pkcs-encrypted-private-key-info?
		(pkcs12-keystore-find-encrypted-private-key
		 (pkcs12-friendly-name-pred "key pair") ks)))
  (test-assert (x509-certificate?
		(pkcs12-keystore-find-certificate
		 (pkcs12-friendly-name-pred "key pair") ks)))
  ;; it's encrypted
  (test-assert (pkcs-encrypted-private-key-info?
		(pkcs12-keystore-find-secret-key
		 (pkcs12-friendly-name-pred "aes") ks)))

  (test-assert (x509-certificate?
		(pkcs12-keystore-find-certificate
		 (pkcs12-friendly-name-pred "google") ks)))

  (test-equal 2 (length
		 ;; means take everything :)
		 (pkcs12-keystore-filter-certificate pkcs12-safe-bag? ks)))
  ;; integrity and/or privacy check
  ;; the result bytevector must always be different as we always generate
  ;; salt randomly when the keystore is serialized to a bytevector
  (test-assert (not (equal? (pkcs12-keystore->bytevector ks "pass")
			    (pkcs12-keystore->bytevector ks "pass"))))
  
  )

(define kp (generate-key-pair *key:ecdsa*))
(define now (current-time))
(define one-year (make-time time-duration 0 (* 3600 24 365)))
(define cert
  (sign-x509-certificate-template
   (x509-certificate-template-builder
    (issuer-dn (x509-name '(C "NL")
			  '(ST "Zuid-Holland")
			  '(L "Leiden")
			  '(CN "Sagittarius Scheme")))
    (subject-dn (x509-name '(C "NL")
			   '(ST "Zuid-Holland")
			   '(L "Leiden")
			   '(OU "Sagittarius Scheme")
			   '(CN "Takashi Kato")))
    (serial-number 1000)
    (not-before (time-utc->date now))
    (not-after (time-utc->date (add-duration now one-year)))
    (public-key (key-pair-public kp)))
   *signature-algorithm:ecdsa-sha256*
   (key-pair-private kp)))
(define crl
  (sign-x509-certificate-revocation-list-template
   (x509-certificate-revocation-list-template-builder
    (issuer-dn (x509-name '(C "NL")
			  '(ST "Zuid-Holland")
			  '(L "Leiden")
			  '(CN "Sagittarius Scheme")))
    (this-update (time-utc->date now))
    (revoked-certificates
     (list (make-x509-revoked-certificate 100 (time-utc->date now)))))
   *signature-algorithm:ecdsa-sha256*
   (key-pair-private kp)))

(define (test-keystore . opts)
  (define ks (apply make-pkcs12-keystore opts))

  (define oak (private-key->pkcs-one-asymmetric-key (key-pair-private kp)))
  (define pk-fn-attr (make-pkcs9-friendly-name-attribute "pk"))
  (define pk=? (pkcs12-friendly-name-pred "pk"))

  (define epki (pkcs-one-asymmetric-key->pkcs-encrypted-private-key-info oak
		(make-pbe-sha1-des-cbc-x509-algorithm-identifier
		 #vu8(1 2 3 4 5) 1024)
		"test"))
  
  (test-assert (pkcs12-keystore? ks))
  ;; private key
  (test-assert (pkcs12-keystore-add-private-key! ks oak (list pk-fn-attr)))
  (test-assert
   (not (pkcs12-keystore-add-private-key! ks oak (list pk-fn-attr))))
  (test-assert (pkcs-one-asymmetric-key?
		(pkcs12-keystore-upsert-private-key! ks oak
						     (list pk-fn-attr))))
  (let ((removed (pkcs12-keystore-remove-private-key! pk=? ks)))
    (test-assert (list? removed))
    (test-assert (for-all pkcs-one-asymmetric-key? removed)))
  (test-assert (not (pkcs12-keystore-upsert-private-key! ks oak
							 (list pk-fn-attr))))

  ;; encrypted private key
  (test-assert (pkcs12-keystore-add-encrypted-private-key! ks epki
							   (list pk-fn-attr)))
  (test-assert (not (pkcs12-keystore-add-encrypted-private-key!
		     ks epki (list pk-fn-attr))))
  (test-assert (pkcs-encrypted-private-key-info?
		(pkcs12-keystore-upsert-encrypted-private-key!
		 ks epki (list pk-fn-attr))))
  (let ((removed (pkcs12-keystore-remove-encrypted-private-key! pk=? ks)))
    (test-assert (list? removed))
    (test-assert (for-all pkcs-encrypted-private-key-info? removed)))
  (test-assert (not (pkcs12-keystore-upsert-encrypted-private-key!
		     ks epki (list pk-fn-attr))))

  ;; certificate
  (test-assert (pkcs12-keystore-add-certificate! ks cert
						 (list pk-fn-attr)))
  (test-assert (not (pkcs12-keystore-add-certificate!
		     ks cert (list pk-fn-attr))))
  (test-assert (x509-certificate?
		(pkcs12-keystore-upsert-certificate!
		 ks cert (list pk-fn-attr))))
  (let ((removed (pkcs12-keystore-remove-certificate! pk=? ks)))
    (test-assert (list? removed))
    (test-assert (for-all x509-certificate? removed)))
  (test-assert (not (pkcs12-keystore-upsert-certificate!
		     ks cert (list pk-fn-attr))))

  ;; crl
  (test-assert (pkcs12-keystore-add-crl! ks crl (list pk-fn-attr)))
  (test-assert (not (pkcs12-keystore-add-crl!
		     ks crl (list pk-fn-attr))))
  (test-assert (x509-certificate-revocation-list?
		(pkcs12-keystore-upsert-crl!
		 ks crl (list pk-fn-attr))))
  (let ((removed (pkcs12-keystore-remove-crl! pk=? ks)))
    (test-assert (list? removed))
    (test-assert (for-all x509-certificate-revocation-list? removed)))
  (test-assert (not (pkcs12-keystore-upsert-crl!
		     ks crl (list pk-fn-attr))))

  ;; secret key
  ;; basically, secret key can be symmetric-key, pkcs-one-asymmetric-key
  ;; or pkcs-encrypted-private-key-info.
  (let ()
    (define (test-secret-key key fn pred)
      (define fn-attr (make-pkcs9-friendly-name-attribute fn))
      (define fn=? (pkcs12-friendly-name-pred fn))
      (test-assert (pkcs12-keystore-add-secret-key! ks key
						    (list fn-attr)))
      (test-assert (not (pkcs12-keystore-add-secret-key!
			 ks key (list fn-attr))))
      (test-assert (pred
		    (pkcs12-keystore-upsert-secret-key!
		     ks key (list fn-attr))))
      (let ((removed (pkcs12-keystore-remove-secret-key! fn=? ks)))
	(test-assert (list? removed))
	(test-assert (for-all pred removed)))
      (test-assert (not (pkcs12-keystore-upsert-secret-key!
			 ks key (list fn-attr)))))
    (test-secret-key (generate-symmetric-key *scheme:aes*) "pk" symmetric-key?)
    (test-secret-key oak "oak" pkcs-one-asymmetric-key?)
    (test-secret-key epki "epki" pkcs-encrypted-private-key-info?)
    )

  (let* ((bv (pkcs12-keystore->bytevector ks "pass"))
	 (ks2 (bytevector->pkcs12-keystore bv "pass")))
    (test-assert
     (lset= eq?
	    (enum-set->list (pkcs12-entry-types private-key encrypted-private-key certificate crl secret-key))
	    (hashtable-ref (pkcs12-keystore-friendly-names ks2) "pk" '()))))
  ks)

(test-keystore)
(test-keystore :integrity-descriptor (digest-descriptor->pkcs12-password-integrity-descriptor *digest:sha3-224*))
;; weak descriptors (defined in RFC)
(test-keystore :privacy-descriptor *pkcs12-privacy-descriptor:pbe/sha1-des3-cbc*)
(test-keystore :privacy-descriptor *pkcs12-privacy-descriptor:pbe/sha1-des2-cbc*)
(test-keystore :privacy-descriptor *pkcs12-privacy-descriptor:pbe/sha1-rc2-128-cbc*)
(test-keystore :privacy-descriptor *pkcs12-privacy-descriptor:pbe/sha1-rc2-40-cbc*)

;; extra
(define pbkdf2-aid (make-pbkdf2-x509-algorithm-identifier #vu8(1 2 3 4) 1024
		    :key-length 32))
;; RC5
(test-keystore :privacy-descriptor
	       (x509-algorithm-identifier->password-privacy-descriptor
		(make-pbes2-x509-algorithm-identifier
		 pbkdf2-aid
		 (make-rc5-encryption-x509-algorithm-identifier 12 64))))
(test-keystore :privacy-descriptor
	       (x509-algorithm-identifier->password-privacy-descriptor
		(make-pbes2-x509-algorithm-identifier
		 pbkdf2-aid
		 (make-rc5-encryption-x509-algorithm-identifier
		  12 64 (make-bytevector (div 64 8))))))
(test-keystore :privacy-descriptor
	       (x509-algorithm-identifier->password-privacy-descriptor
		(make-pbes2-x509-algorithm-identifier
		 pbkdf2-aid
		 (make-rc5-encryption-x509-algorithm-identifier 12 128))))
(test-keystore :privacy-descriptor
	       (x509-algorithm-identifier->password-privacy-descriptor
		(make-pbes2-x509-algorithm-identifier
		 pbkdf2-aid
		 (make-rc5-encryption-x509-algorithm-identifier 
		  12 128 (make-bytevector (div 128 8))))))
;; RC2
(test-keystore :privacy-descriptor
	       (x509-algorithm-identifier->password-privacy-descriptor
		(make-pbes2-x509-algorithm-identifier
		 pbkdf2-aid
		 (make-rc2-encryption-x509-algorithm-identifier
		  160 (make-bytevector 8)))))

;; RFC 9879, Appendix A: https://www.rfc-editor.org/rfc/rfc9879#appendix-A
(define (rfc9879-vector n)
  (let-values (((dir base ext) (decompose-path (current-load-path))))
    (call-with-input-file
	(build-path* dir "data"
		     (string-append "rfc9879-a" (number->string n) ".p12"))
      (lambda (in) (read-pkcs12-keystore "1234" in))
      :transcoder #f)))

(for-each (lambda (n)
	    (test-assert (string-append "RFC 9879 A." (number->string n))
			 (pkcs12-keystore? (rfc9879-vector n))))
	  '(1 2 3))
(for-each (lambda (n)
	    (test-error (string-append "RFC 9879 rejects A." (number->string n))
			condition? (rfc9879-vector n)))
	  '(4 5 6))

(let* ((ks (make-pkcs12-keystore
	    :integrity-descriptor
	    *pkcs12-integrity-descriptor:pbmac1/pbkdf2/hmac-sha256*))
       (bv (pkcs12-keystore->bytevector ks "password"))
       (pfx (bytevector->asn1-encodable <pfx> bv))
       (mac-data (pfx-mac-data pfx))
       (mac (mac-data-mac mac-data))
       (aid (algorithm-identifier->x509-algorithm-identifier
	     (digest-info-digest-algorithm mac)))
       (params (x509-algorithm-identifier-parameters aid))
       (kdf (pkcs-pbmac1-params-key-derivation-func params))
       (pbkdf2 (x509-algorithm-identifier-parameters kdf))
       (hmac-aid (make-x509-algorithm-identifier
		  (der-object-identifier->oid-string *pbes:hmac/sha256*)
		  (make-der-null))))
  (define (with-mac aid digest outer-salt outer-count)
    (asn1-encodable->bytevector
     (make <pfx>
       :version (pfx-version pfx)
       :auth-safe (pfx-auth-safe pfx)
       :mac-data
       (make <mac-data>
	 :mac (make <digest-info>
		:digest-algorithm
		(x509-algorithm-identifier->algorithm-identifier aid)
		:digest (bytevector->der-octet-string digest))
	 :mac-salt (bytevector->der-octet-string outer-salt)
	 :iterations (integer->der-integer outer-count)))))
  (define (with-pbkdf2-params salt count length prf)
    (make-pbmac1-x509-algorithm-identifier
     (make-pbkdf2-x509-algorithm-identifier
      salt count :key-length length :prf prf)
     hmac-aid))
  (define digest (ber-octet-string->bytevector (digest-info-digest mac)))
  (define salt #vu8(1 2 3 4 5 6 7 8))
  (define unicode-digest
    #vu8(145 184 185 210 11 41 29 127 137 148 171 98 6 200 144 245
	 117 99 25 174 27 233 87 151 59 181 210 224 4 109 152 49))
  (define decomposed-digest
    #vu8(16 251 195 2 172 56 154 176 143 122 138 6 225 125 34 217
	 99 237 29 252 124 234 55 249 62 122 49 87 29 171 97 8))
  (test-equal "PBMAC1 OID" (der-object-identifier->oid-string *pbes:pbmac1*)
	      (x509-algorithm-identifier-oid aid))
  (test-assert "PBMAC1 parameters" (pkcs-pbmac1-params? params))
  (test-equal "PBKDF2 OID" (der-object-identifier->oid-string *pbes:pbkdf2*)
	      (x509-algorithm-identifier-oid kdf))
  (test-equal "explicit 32 byte key" 32
	      (pkcs-pbkdf2-params-key-length pbkdf2))
  (test-equal "SHA-256 PRF"
	      (x509-algorithm-identifier-oid hmac-aid)
	      (x509-algorithm-identifier-oid
	       (pkcs-pbkdf2-params-prf pbkdf2)))
  (test-assert "non-empty outer salt"
	       (positive? (bytevector-length
			   (ber-octet-string->bytevector
			    (mac-data-mac-salt mac-data)))))
  (test-assert "positive outer iterations"
	       (positive? (der-integer->integer (mac-data-iterations mac-data))))
  (test-assert "PBMAC1 round trip"
	       (pkcs12-keystore?
		(bytevector->pkcs12-keystore bv "password")))
  (test-error "wrong password" condition?
	      (bytevector->pkcs12-keystore bv "wrong"))
  (test-error "modified authenticated-safe content" condition?
	      (bytevector->pkcs12-keystore
	       (asn1-encodable->bytevector
		(make <pfx>
		  :version (pfx-version pfx)
		  :auth-safe
		  (make <content-info>
		    :content-type (content-info-content-type (pfx-auth-safe pfx))
		    :content (bytevector->der-octet-string #vu8(48 1 0)))
		  :mac-data mac-data))
	       "password"))
  (test-error "configurable PBMAC1 iteration limit" condition?
	      (parameterize ((*pkcs12-pbmac1-max-iterations* 1))
		(bytevector->pkcs12-keystore bv "password")))
  (test-assert "outer salt and iterations are ignored"
	       (pkcs12-keystore?
		(bytevector->pkcs12-keystore
		 (with-mac aid digest #vu8() 0) "password")))
  (test-assert "UTF-8 including supplementary and trailing NUL"
	       (pkcs12-keystore?
		(bytevector->pkcs12-keystore
		 (with-mac (with-pbkdf2-params salt 10 32 hmac-aid)
			   unicode-digest salt 1)
		 "päss😀\x0;")))
  (test-assert "Unicode is not normalized"
	       (pkcs12-keystore?
		(bytevector->pkcs12-keystore
		 (with-mac (with-pbkdf2-params salt 10 32 hmac-aid)
			   decomposed-digest salt 1)
		 "pa\x0308;ss😀\x0;")))
  (test-error "different normalization is not accepted" condition?
	      (bytevector->pkcs12-keystore
	       (with-mac (with-pbkdf2-params salt 10 32 hmac-aid)
			 unicode-digest salt 1)
	       "pa\x0308;ss😀\x0;"))
  (for-each
   (lambda (bad)
     (test-error "invalid PBMAC1 parameters" condition?
		 (bytevector->pkcs12-keystore
		  (with-mac bad digest salt 1) "password")))
   (list (make-x509-algorithm-identifier
	  (der-object-identifier->oid-string *pbes:pbmac1*))
	 (with-pbkdf2-params salt 10 #f hmac-aid)
	 (with-pbkdf2-params salt 0 32 hmac-aid)
	 (with-pbkdf2-params salt -1 32 hmac-aid)
	 (with-pbkdf2-params salt 10 19 hmac-aid)
	 (with-pbkdf2-params salt 10 129 hmac-aid)
	 (with-pbkdf2-params #vu8() 10 32 hmac-aid)
	 (with-pbkdf2-params salt 1000001 32 hmac-aid)
	 (with-pbkdf2-params salt 10 32 #f)
	 (with-pbkdf2-params salt 10 32
			     (make-x509-algorithm-identifier
			      (der-object-identifier->oid-string *pbes:hmac/sha1*)))
	 (with-pbkdf2-params salt 10 32
			     (make-x509-algorithm-identifier
			      (der-object-identifier->oid-string *pbes:hmac/sha256*)
			      (integer->der-integer 1)))
	 (make-pbmac1-x509-algorithm-identifier
	  (make-x509-algorithm-identifier "1.2.3") hmac-aid)
	 (make-pbmac1-x509-algorithm-identifier
	  (make-pbkdf2-x509-algorithm-identifier
	   salt 10 :key-length 32 :prf hmac-aid)
	  (make-x509-algorithm-identifier
	   (der-object-identifier->oid-string *pbes:hmac/sha1*))))))

(test-end)
