(import (rnrs)
	(sagittarius)
	(sagittarius crypto ciphers)
	(sagittarius crypto keys)
	(sagittarius crypto digests)
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
