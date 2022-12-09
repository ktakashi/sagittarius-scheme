(import (rnrs)
	(sagittarius crypto ciphers)
	(sagittarius crypto keys)
	(sagittarius crypto x509)
	(sagittarius crypto digests)
	(sagittarius crypto signatures)
	(sagittarius crypto pkcs keystore)
	(sagittarius crypto pkcs keys)
	(sagittarius crypto pkcs pbes) ;; to enable PBES2 and so
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

(test-end)
