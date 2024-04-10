(import (rnrs)
	(sagittarius)
	(sagittarius crypto digests)
	(sagittarius crypto math prime)
	(sagittarius crypto kdfs)
	(srfi :64))

(define (test-prime name tests)
  (define (check test)
    (let ((id (vector-ref test 0))
	  (comment (vector-ref test 1))
	  (value (vector-ref test 2))
	  (result (vector-ref test 3)))
      ;; We don't accept negative prime
      (unless (string=? "negative of a prime" comment)
	(test-equal (format "~a ~d (~a): ~a" name id result comment)
		    result
		    (probable-prime? value)))))
  (for-each check tests))

(test-begin "Prime number test vectors")
(include "./testvectors/prime.scm")
(test-end)

(define (test-hkdf source :key algorithm key-size tests)
  (define (->digest a)
    (cond ((string=? a "HKDF-SHA-1")   *digest:sha-1*)
	  ((string=? a "HKDF-SHA-256") *digest:sha-256*)
	  ((string=? a "HKDF-SHA-384") *digest:sha-384*)
	  ((string=? a "HKDF-SHA-512") *digest:sha-512*)
	  (else (assertion-violation 'test-hkdf "Unknown algorithm" a))))
  (define ((check digest) test)
    (let-values (((id comment ikm salt info size okm result flag)
		  (apply values (vector->list test))))
      (cond (result (test-equal (cons* source comment flag) okm
				(hkdf digest ikm salt info size)))
	    ((member "SizeTooLarge" flag)
	     (test-error (cons* source comment flag)
			 (hkdf digest ikm salt info size)))
	    (else
	     (test-assert (cons* source comment flag)
		  (not (equal? okm (hkdf digest ikm salt info size))))))))
  (for-each (check (->digest algorithm)) tests))

(test-begin "HKDF")
(include "./testvectors/hkdf.scm")
(test-end)

(include "./test-vector-mac.scm")
(include "./test-vector-aead.scm")
(include "./test-vector-ecdh.scm")
(include "./test-vector-signature.scm")

