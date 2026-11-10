(import (rnrs)
	(srfi :64 testing)
	(math ec))

(test-begin "EC Jacobian scalar multiplication")

(define (affine-reference-mul curve p k)
  (let ((n (abs k))
	(neg? (negative? k)))
    (let loop ((n n) (acc ec-infinity-point) (base p))
      (if (zero? n)
	  (if neg? (ec-point-negate curve acc) acc)
	  (loop (bitwise-arithmetic-shift-right n 1)
		(if (odd? n) (ec-point-add curve acc base) acc)
		(ec-point-twice curve base))))))

(define (parameter-name param)
  (or (ec-parameter-oid param) "unnamed-curve"))

(define (derive-test-scalar n)
  (let ((k (mod (+ (div n 2) #x123456789abcdef123456789) n)))
    (if (zero? k) 5 k)))

(define (assert-on-curve name curve point)
  (unless (ec-point-infinity? point)
    (test-assert (format "~a result is on curve" name)
		 (valid-ec-point? curve point))))

(define prime-parameters
  (list NIST-P-192
	NIST-P-224
	NIST-P-256
	NIST-P-384
	NIST-P-521
	secp160r1
	secp160k1
	secp160r2
	secp192k1
	secp224k1
	secp256k1
	brainpool-p160r1
	brainpool-p160t1
	brainpool-p192r1
	brainpool-p192t1
	brainpool-p224r1
	brainpool-p224t1
	brainpool-p256r1
	brainpool-p256t1
	brainpool-p320r1
	brainpool-p320t1
	brainpool-p384r1
	brainpool-p384t1
	brainpool-p512r1
	brainpool-p512t1
	curve25519))

(define (test-prime-curve parameter)
  (let* ((curve (ec-parameter-curve parameter))
	 (name (parameter-name parameter))
	 (field (elliptic-curve-field curve))
	 (g (ec-parameter-g parameter))
	 (n (ec-parameter-n parameter))
	 (k (derive-test-scalar n))
	 (k2 (if (> n 11) (- n 11) 3))
	 (kg (ec-point-mul curve g k))
	 (k2g (ec-point-mul curve g k2)))
    (test-assert (format "~a uses prime field" name) (ec-field-fp? field))
    (test-assert (format "~a 0*G = O" name)
		 (ec-point-infinity? (ec-point-mul curve g 0)))
    (test-equal (format "~a 1*G = G" name)
		g
		(ec-point-mul curve g 1))
    (test-equal (format "~a 2*G = 2G" name)
		(ec-point-twice curve g)
		(ec-point-mul curve g 2))
    (test-assert (format "~a n*G = O" name)
		 (ec-point-infinity? (ec-point-mul curve g n)))
    (test-equal (format "~a (n-1)*G = -G" name)
		(ec-point-negate curve g)
		(ec-point-mul curve g (- n 1)))
    (test-equal (format "~a negative scalar" name)
		(ec-point-negate curve kg)
		(ec-point-mul curve g (- k)))
    (test-equal (format "~a reference scalar k" name)
		(affine-reference-mul curve g k)
		kg)
    (test-equal (format "~a reference scalar k2" name)
		(affine-reference-mul curve g k2)
		k2g)
    (test-equal (format "~a add inverse" name)
		ec-infinity-point
		(ec-point-add curve g (ec-point-negate curve g)))
    (test-equal (format "~a add infinity (left)" name)
		g
		(ec-point-add curve ec-infinity-point g))
    (test-equal (format "~a add infinity (right)" name)
		g
		(ec-point-add curve g ec-infinity-point))
    (assert-on-curve name curve kg)
    (assert-on-curve name curve k2g)))

(for-each test-prime-curve prime-parameters)

(define binary-parameters
  (list NIST-K-163
	NIST-K-233
	NIST-K-283
	NIST-K-409
	NIST-K-571
	NIST-B-163
	NIST-B-233
	NIST-B-283
	NIST-B-409
	NIST-B-571
	sect163r1
	sect239k1
	sect113r1))

(define (test-binary-curve parameter)
  (let* ((curve (ec-parameter-curve parameter))
	 (name (parameter-name parameter))
	 (field (elliptic-curve-field curve))
	 (g (ec-parameter-g parameter))
	 (n (ec-parameter-n parameter))
	 (k (derive-test-scalar n))
	 (k2 (if (> n 13) (- n 13) 9)))
    (test-assert (format "~a uses binary field" name) (ec-field-f2m? field))
    (for-each
     (lambda (k)
       (test-equal (format "~a fallback scalar ~a" name k)
		   (affine-reference-mul curve g k)
		   (ec-point-mul curve g k)))
     `(0 1 2 3 5 17 -17 ,k ,(- k) ,k2 ,(- k2)))
    (test-equal (format "~a (n-1)*G = -G" name)
		(ec-point-negate curve g)
		(ec-point-mul curve g (- n 1)))
    (test-assert (format "~a n*G = O" name)
		 (ec-point-infinity? (ec-point-mul curve g n)))))

(for-each test-binary-curve binary-parameters)

(test-end)
