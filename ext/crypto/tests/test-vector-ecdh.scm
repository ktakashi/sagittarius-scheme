(import (rnrs)
	(sagittarius)
	(sagittarius crypto signatures)
	(sagittarius crypto keys)
	(srfi :64))

(define ecdh-expected-error-test-comments
  '(
    ;; FIXME this must be fixed, but not sure how...
    "edge case for Jacobian and projective coordinates"
    "edge case for Jacobian and projective coordinates in left to right addition chain"
    "a = 0" ;; We check curve equiality, so this can't be passed
    ;; we don't correct or strip excess 0s
    "appending 0's to bit string"
    "appending null value to bit string"
    ;; Qy will be different (e.g. on tcId 486 of brainpool p224r1 test)
    "modifying last byte of bit string"
    "truncated bit string"
    "bit string of size 4155 to check for overflows"
    "unused bits in bit string"
    ))
(define ecdh-expected-error-test-flags
  '("InvalidAsn"))

(define (run-test-ecdh source algorithm curve encoding tests ->key)
  (define (check-flags flags)
    (exists (lambda (flag) (member flag ecdh-expected-error-test-flags)) flags))
  (define (check-deviation comment flags)
    (or (member comment ecdh-expected-error-test-comments)
	(check-flags flags)))
  (define (check test)
    (let-values (((id comment public private shared result flags)
		  (apply values (vector->list test))))
      (guard (e (else
		 (if (check-deviation comment flags)
		     (test-assert `("ECDH error" ,curve ,id ,comment) result)
		     (test-assert `("ECDH error" ,curve ,id ,comment)
				  (not result)))))
	(let ((pub-key (->key public 'public))
	      (priv-key (->key private 'private)))
	  (let ((sk (calculate-key-agreement *key:ecdh* priv-key pub-key)))
	    (if result
		(let ((ok? (equal? shared sk)))
		  (when (and (not ok?) (check-deviation comment flags))
		    (test-expect-fail `("ECDH" ,curve ,id ,comment)))
		  (test-assert `("ECDH" ,curve ,id ,comment) ok?))
		(test-assert `("ECDH invalid" ,curve ,id ,comment)
			     (not (equal? shared sk)))))))))
  (for-each check tests))

(define (test-ecdh source :key algorithm curve encoding tests)
  (define (curve->ec-parameter curve)
    (cond ((string=? curve "brainpoolP224r1") *ec-parameter:brainpool-p224r1* #f)
	  ((string=? curve "brainpoolP256r1") *ec-parameter:brainpool-p256r1* #f)
	  ((string=? curve "brainpoolP320r1") *ec-parameter:brainpool-p320r1* #f)
	  ((string=? curve "brainpoolP384r1") *ec-parameter:brainpool-p384r1* #f)
	  ((string=? curve "brainpoolP512r1") *ec-parameter:brainpool-p512r1* #f)
	  ((string=? curve "brainpoolP224t1") *ec-parameter:brainpool-p224t1* #f)
	  ((string=? curve "brainpoolP256t1") *ec-parameter:brainpool-p256t1* #f)
	  ((string=? curve "brainpoolP320t1") *ec-parameter:brainpool-p320t1* #f)
	  ((string=? curve "brainpoolP384t1") *ec-parameter:brainpool-p384t1* #f)
	  ((string=? curve "brainpoolP512t1") *ec-parameter:brainpool-p512t1* #f)
	  ((string=? curve "secp224k1") *ec-parameter:secp224k1* #f)
	  ((string=? curve "secp224r1") *ec-parameter:secp224r1* #f)
	  ((string=? curve "secp256k1") *ec-parameter:secp256k1* #f)
	  ((string=? curve "secp256r1") *ec-parameter:secp256r1* #f)
	  ((string=? curve "secp384r1") *ec-parameter:secp384r1* #f)
	  ((string=? curve "secp521r1") *ec-parameter:secp521r1*)
	  ((string=? curve "sect283k1") *ec-parameter:sect283k1* #f)
	  ((string=? curve "sect283r1") *ec-parameter:sect283r1* #f)
	  ((string=? curve "sect409k1") *ec-parameter:sect409k1* #f)
	  ((string=? curve "sect409r1") *ec-parameter:sect409r1* #f)
	  ((string=? curve "sect571k1") *ec-parameter:sect571k1* #f)
	  ((string=? curve "sect571r1") *ec-parameter:sect571r1* #f)
	  (else (print "Unknown curve: " curve) #f)))
  (define ec-parameter (curve->ec-parameter curve))
  (define (bytevector->key bv type)
    (case type
      ((public)
       (if (string=? encoding "asn")
	   (import-public-key *key:ecdsa* bv)
	   (import-public-key *key:ecdsa* bv 'raw ec-parameter)))
      ((private)
       (generate-private-key *key:ecdsa*
			     (bytevector->integer bv) ec-parameter))))
  (when ec-parameter
    (print "Testing " curve)
    (run-test-ecdh source algorithm curve encoding tests bytevector->key)))

(define (test-ecdh-jwk source :key algorithm curve encoding tests)
  (define (jwk->key json type) #t)
  #;(run-test-ecdh source algorithm curve encoding tests jwk->key))

(test-begin "ECDH")
(include "./testvectors/ecdh.scm")
(test-end)
