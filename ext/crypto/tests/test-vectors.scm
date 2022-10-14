(import (rnrs)
	(sagittarius crypto signatures)
	(sagittarius crypto keys)
	(sagittarius crypto digests)
	(sagittarius crypto math prime)
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


;; In our implementation, size of the signature matters
;; So, if the signature size is not properly constructed,
;; it's an error. So below test(s) will fail even though
;; it should be 'acceptable'
(define signature-size-test
  '("incorrect size of signature"))

(define (test-signature/testvector source
	 :key algorithm public-key tests der-encode
	      (digest #f) (mgf #f) (mgf-digest #f) (salt-length #f))
  ;; if key import is not supported, then ignore
  (define (import-key k)
    (guard (e ((implementation-restriction-violation? e) #f)
	      (else (test-assert "Failed to import key" #f) #f))
      (import-public-key k (public-key-format subject-public-key-info))))
  (define (->mgf mgf)
    (if (string=? mgf "MGF1")
	mgf-1
	(error '->mgf (string-append "Unknown MGF function: " mgf))))
  (define (->digest d)
    (cond ((string=? d "SHA-1")       *digest:sha-1*)
	  ((string=? d "SHA-224")     *digest:sha-224*)
	  ((string=? d "SHA-256")     *digest:sha-256*)
	  ((string=? d "SHA-384")     *digest:sha-384*)
	  ((string=? d "SHA-512")     *digest:sha-512*)
	  ((string=? d "SHA-512/224") *digest:sha-512/224*)
	  ((string=? d "SHA-512/256") *digest:sha-512/256*)
	  ((string=? d "SHA3-224")    *digest:sha3-224*)
	  ((string=? d "SHA3-256")    *digest:sha3-256*)
	  ((string=? d "SHA3-384")    *digest:sha3-384*)
	  ((string=? d "SHA3-512")    *digest:sha3-512*)
	  (else (error '->digest (string-append "Unknown digest: "d)))))
  (define (algorithm->signature-algorithm algorithm pkey)
    (cond ((string=? algorithm "DSA")
	   (values *signature:dsa*
		   `(:der-encode ,der-encode
		     :digest ,(->digest digest))))
	  ((string=? algorithm "ECDSA")
	   (values *signature:ecdsa*
		   `(:der-encode ,der-encode
		     :digest ,(->digest digest))))
	  ((string=? algorithm "EDDSA")
	   (values (if (ed25519-key? pkey)
		       *signature:ed25519*
		       *signature:ed448*) '()))
	  ((string=? algorithm "RSASSA-PSS")
	   (values *signature:rsa*
		   `(:verifier ,pkcs1-emsa-pss-verify
		     :digest ,(->digest digest)
		     :salt-length ,salt-length
		     :mgf ,(->mgf mgf)
		     :mgf-digest ,(->digest mgf-digest))))
	  ((string=? algorithm "RSASSA-PKCS1-v1_5" algorithm)
	   (values *signature:rsa*
		   `(:verifier ,pkcs1-emsa-v1.5-verify
		     :digest ,(->digest digest))))
	  (else (error #f (string-append "Unknown algorithm " algorithm)))))
  (define (->verifier algorithm pkey)
    (guard (e (else (test-assert (condition-message e) #f) #f))
      (let-values (((alg param)
		    (algorithm->signature-algorithm algorithm pkey)))
	(apply make-verifier alg pkey param))))
  (define ((verify-signature verifier) test)
    (define (safe-verify verifier msg sig)
      (guard (e (else #f))
	(verifier-verify-signature verifier msg sig)))
    (let ((id (vector-ref test 0))
	  (comment (vector-ref test 1))
	  (msg (vector-ref test 2))
	  (sig (vector-ref test 3))
	  (result (vector-ref test 4))
	  (tags (vector-ref test 5)))
      (unless (or (member comment signature-size-test))
	(test-equal (format "~a ~d (~a): ~a" source id result comment)
		    result (safe-verify verifier msg sig)))))
  (let* ((pkey (import-key public-key))
	 (verifier (->verifier algorithm pkey)))
    (when verifier
      ;; (print "Testing " source)
      (test-assert verifier)
      (for-each (verify-signature verifier) tests))))

(test-begin "Signature test vectors")
(include "./testvectors/signature.scm")
(test-end)
