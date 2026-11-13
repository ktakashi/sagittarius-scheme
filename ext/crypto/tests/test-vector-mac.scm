(import (rnrs)
	(sagittarius)
	(sagittarius crypto ciphers)
	(sagittarius crypto keys)
	(sagittarius crypto digests)
	(sagittarius crypto kdfs)
	(sagittarius crypto mac)
	(srfi :64))

(define (test-hmac source :key algorithm key-size tag-size tests)
  (define (->digest a)
    (cond ((string=? a "HMACSHA1")     *digest:sha-1*)
	  ((string=? a "HMACSHA224")   *digest:sha-224*)
	  ((string=? a "HMACSHA256")   *digest:sha-256*)
	  ((string=? a "HMACSHA384")   *digest:sha-384*)
	  ((string=? a "HMACSHA512")   *digest:sha-512*)
	  ((string=? a "HMACSHA3-224") *digest:sha3-224*)
	  ((string=? a "HMACSHA3-256") *digest:sha3-256*)
	  ((string=? a "HMACSHA3-384") *digest:sha3-384*)
	  ((string=? a "HMACSHA3-512") *digest:sha3-512*)
	  (else (assertion-violation 'test-hmac "Unknown algorithm" a))))
  (define ((check digest) test)
    (let-values (((id comment key msg tag result flag)
		  (apply values (vector->list test))))
      (let ((mac (make-mac *mac:hmac* key :digest digest))
	    (size (div tag-size 8)))
	(test-equal (list algorithm comment)
		    result (equal? tag (generate-mac mac msg size))))))
  (let ((digest (->digest algorithm)))
    (for-each (check digest) tests)))

(test-begin "HMAC test vectors")
(include "./testvectors/hmac.scm")
(test-end)

(define (test-cmac source :key algorithm key-size tag-size tests)
  (define (->cipher a)
    (cond ((string=? a "AES-CMAC") *scheme:aes*)
	  (else (assertion-violation 'test-hmac "Unknown algorithm" a))))
  (define ((check cipher) test)
    (let-values (((id comment key msg tag result flag)
		  (apply values (vector->list test))))
      (let ((mac (make-mac *mac:cmac* key :cipher cipher))
	    (size (div tag-size 8)))
	(if (string=? comment "invalid key size")
	    (test-error (list algorithm comment) (generate-mac mac msg size))
	    (test-equal (list algorithm comment) result
			(equal? tag (generate-mac mac msg size)))))))
  (let ((cipher (->cipher algorithm)))
    (for-each (check cipher) tests)))

(test-begin "CMAC")
(include "./testvectors/cmac.scm")
(test-end)

(define (test-gmac source :key algorithm key-size tag-size tests)
  (define (->cipher a)
    (cond ((string=? a "AES-GMAC") *scheme:aes*)
	  (else (assertion-violation 'test-hmac "Unknown algorithm" a))))
  (define ((check cipher) test)
    (let-values (((id comment key iv msg tag result flag)
		  (apply values (vector->list test))))
      (if (string=? comment "invalid key size")
	  (test-error (list id algorithm comment)
		      (make-mac *mac:gmac* key :cipher cipher :iv iv))
	  (let ((mac (make-mac *mac:gmac* key :cipher cipher :iv iv))
		(size (div tag-size 8)))
	    (if (string=? comment "invalid nonce size")
		(test-error (list id algorithm comment)
			    (generate-mac mac msg size))
		(test-equal (list id algorithm comment) result
			    (equal? tag (generate-mac mac msg size))))))))
  (let ((cipher (->cipher algorithm)))
    (for-each (check cipher) tests)))

(test-begin "GMAC")
(include "./testvectors/gmac.scm")
(test-end)

(test-begin "PBMAC1")
(let* ((password (string->utf8 "päss😀\x0;"))
       (prf (mac->prf-provider *mac:hmac* :digest *digest:sha-256*))
       (kdf (lambda (p) (pbkdf-2 p #vu8(1 2 3 4 5 6 7 8) 10 32
				  :prf prf)))
       (mac-provider (lambda (key)
		       (make-mac *mac:hmac* key :digest *digest:sha-256*)))
       (mac (make-mac *mac:pbmac1* password
		      :kdf kdf :mac-provider mac-provider))
       (expected
	#vu8(145 184 185 210 11 41 29 127 137 148 171 98 6 200 144 245
	     117 99 25 174 27 233 87 151 59 181 210 224 4 109 152 49)))
  (test-equal "RFC 9879 UTF-8 PBMAC1" expected (generate-mac mac #vu8(48 0)))
  (test-equal "PBMAC1 size" 32 (mac-mac-size mac))
  (test-equal "PBMAC1 OID" "1.2.840.113549.1.5.14" (mac-oid mac))
  (test-equal "reusable MAC" expected (generate-mac mac #vu8(48 0)))
  (verify-mac mac #vu8(48 0) expected)
  (test-error "PBMAC1 verification rejects tampering" condition?
	      (verify-mac mac #vu8(48 1) expected))
  (mac-init! mac)
  (mac-process! mac #vu8(48))
  (mac-process! mac #vu8(0))
  (let ((result (make-bytevector 32)))
    (mac-done! mac result)
    (test-equal "streaming PBMAC1" expected result))
  (let ((generator (make-mac-generator mac))
	(result (make-bytevector 32)))
    (generator #vu8(48 0) result)
    (test-equal "PBMAC1 generator" expected result))
  (test-error "invalid PBMAC1 KDF" assertion-violation?
	      (make-mac *mac:pbmac1* password :kdf #f
			:mac-provider mac-provider))
  (test-error "PBMAC1 KDF must return a key" assertion-violation?
	      (make-mac *mac:pbmac1* password :kdf (lambda (p) #f)
			:mac-provider mac-provider))
  (test-error "invalid PBMAC1 MAC provider" assertion-violation?
	      (make-mac *mac:pbmac1* password :kdf kdf
			:mac-provider (lambda (key) key))))
(let* ((key #vu8(1 2 3 4 5 6))
       (data #vu8(7 8 9))
       (mac (make-mac *mac:pbmac1* key
		      :kdf (lambda (p) p)
		      :mac-provider
		      (lambda (k) (make-mac *mac:hmac* k
					    :digest *digest:sha-512*)))))
  (test-equal "alternate KDF and underlying MAC"
	      (generate-mac (make-mac *mac:hmac* key :digest *digest:sha-512*)
			    data)
	      (generate-mac mac data)))
(let* ((key #vu8(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
       (data #vu8(7 8 9))
       (mac (make-mac *mac:pbmac1* key
		      :kdf (lambda (p) p)
		      :mac-provider
		      (lambda (k) (make-mac *mac:cmac* k
					    :cipher *scheme:aes-128*)))))
  (test-equal "PBMAC1 with CMAC"
	      (generate-mac (make-mac *mac:cmac* key
				      :cipher *scheme:aes-128*) data)
	      (generate-mac mac data)))
(test-end)
