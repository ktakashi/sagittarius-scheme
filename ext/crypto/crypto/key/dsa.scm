;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; dsa.scm Cryptographic library
;;; 
#!compatible
(library (crypto key dsa)
    (export DSA
	    generate-dsa-parameter)
    (import (rnrs)
	    (math)
	    (sagittarius)
	    (clos user)
	    (sagittarius crypto)
	    (crypto key pair)
	    (asn.1))

  (define +key-length+ `((1024 . ,SHA-1) (2048 . ,SHA-256) (3072 . ,SHA-256)))
  ;; class
  (define-class <dsa-key-parameter> ()
    ((p :init-keyword :p)
     (q :init-keyword :q)
     (g :init-keyword :g)))

  (define (generate-dsa-parameter L :key (prng (secure-random RC4)))
    (define (search-p L* q)
      (let* ((test (bitwise-arithmetic-shift-left 1 (- L 1)))
	     (r0   (bitwise-arithmetic-shift-left q 1))
	     (buf  (make-bytevector L*)))
	(let loop ()
	  (read-random-bytes! prng buf L*)
	  (let* ((X (bytevector->integer buf))
		 (c (mod X r0))
		 (p (- X (- c 1))))
	    (if (and (>= p test) (is-prime? p))
		p
		(loop))))))
    (define (search-g h p q) (mod-expt h (/ (- p 1) q) p))
    (let* ((h (cond ((assv L +key-length+) => cdr)
		    (else (error 'generate-dsa-parameter "non supported key bit"
				 L))))
	   ;; bytes for convenience
	   (N* (hash-size h))
	   (q  (random-prime N*))
	   (p  (search-p (div L 8) q))
	   (g  (search-g 2 p q)))
      (make <dsa-key-parameter> :p p :q q :g g)))

  (define (generate-dsa-keypair p q g :key (prng (secure-random RC4)))
    (define (search-x q)
      (let ((len (div (bitwise-length q) 8)))
	(let loop ((xx (read-random-bytes prng len)))
	  (let ((x (bytevector->integer xx)))
	    (if (= x q)
		(loop (read-random-bytes prng len))
		x)))))
    (let* ((x (search-x q))
	   (y (mod-expt g x p)))
      (values x y)))

  ;; marker
  (define-class <dsa> () ())
  (define DSA (make <dsa>))

  (define-class <dsa-public-key> (<public-key> <dsa-key-parameter>)
    ((Y :init-keyword :Y)))
  (define-method write-object ((pr <dsa-public-key>) out)
    (format out "#<dsa-public-key~%")
    (format out "   p: ~x~%" (slot-ref pr 'p))
    (format out "   q: ~x~%" (slot-ref pr 'q))
    (format out "   g: ~x~%" (slot-ref pr 'g))
    (format out "   Y: ~x~%>" (slot-ref pr 'Y)))

  (define-class <dsa-private-key> (<private-key> <dsa-key-parameter>)
    ((X :init-keyword :X)))
  (define-method write-object ((pr <dsa-private-key>) out)
    (format out "#<dsa-private-key~%")
    (format out "   p: ~x~%" (slot-ref pr 'p))
    (format out "   q: ~x~%" (slot-ref pr 'q))
    (format out "   g: ~x~%" (slot-ref pr 'g))
    (format out "   X: ~x~%>" (slot-ref pr 'X)))

  (define-method generate-key-pair ((m <dsa>) :key (size 1024)
				    (prng (secure-random RC4))
				    (parameter #f))
    (let* ((pr (if parameter 
		   parameter 
		   (generate-dsa-parameter size :prng prng)))
	   (p (slot-ref pr 'p))
	   (q (slot-ref pr 'q))
	   (g (slot-ref pr 'g)))
      (let-values (((x y) (generate-dsa-keypair p q g :prng prng)))
	(let ((private (make <dsa-private-key> :p p :q q :g g :X x))
	      (public (make <dsa-public-key> :p p :q q :g g :Y y)))
	  (make-keypair private public)))))

  ;; According to OpenSSL
  ;; DSA signature data is packes like this;
  ;; DSASignature ::= SEQUENCE {
  ;;   r INTEGER,
  ;;   s INTEGER
  ;; }
  (define (dsa-sign bv key :key (hasher :hash #f) (prng (secure-random RC4)))
    (define (get-digester p)
      (if hasher
	  hasher
	  (cond ((assv (bitwise-length p) +key-length+) => cdr)
		(else (error 'dsa-sign "can't determine the hash algorithm")))))
    (unless (is-a? key <dsa-private-key>)
      (raise-encrypt-error 'dsa-sign "invalid key" 'DSA))
    (let* ((p (slot-ref key 'p))
	   (q (slot-ref key 'q))
	   (g (slot-ref key 'g))
	   (x (slot-ref key 'X))
	   (digester (get-digester p))
	   (k (random prng q))
	   (r (mod (mod-expt g k p) q))
	   (xr+h (mod (+ (bytevector->integer (hash digester bv)) (* x r)) q))
	   (kinv (mod-inverse k q))
	   (s (mod (* (if (> xr+h q) (- xr+h q) xr+h) kinv) q)))
      (encode (make-der-sequence
	       (make-der-integer r)
	       (make-der-integer s)))))

  (define (dsa-verify bv key))

  ;; cipher
  (define-class <dsa-cipher-spi> (<cipher-spi>) ())
  (define-method initialize ((o <dsa-cipher-spi>) initargs)
    (let ((key (car initargs)))
      (let-keywords (cdr initargs)
	  ((prng (secure-random RC4))
	   . ignore)
	(slot-set! o 'name 'DSA)
	(slot-set! o 'key key)
	(slot-set! o 'encrypt 
		   (lambda ignore (error 'encrypt "not supported in DSA")))
	(slot-set! o 'decrypt
		   (lambda ignore (error 'decrypt "not supported in DSA")))
	(slot-set! o 'padder #f)
	(slot-set! o 'signer dsa-sign)
	(slot-set! o 'verifier dsa-verify)
	(slot-set! o 'keysize (lambda (target) 1024)) ;should we?
	o)))
  (register-spi DSA <dsa-cipher-spi>)

  #|
      PublicKeyInfo ::= SEQUENCE {
        algorithm AlgorithmIdentifier,
        PublicKey BIT STRING
      }
      AlgorithmIdentifier ::= SEQUENCE {
        algorithm ALGORITHM.id,
        parameters Dss-Parms
      }
      Dss-Parms ::= SEQUENCE {
        p INTEGER,
        q INTEGER,
        g INTEGER
      }
      DSAPublicKey ::= BITSTRING {
        publicExponent INTEGER
      }
  |#
  ;; I think this is a bad format since sign and verify always need
  ;; p, q and g. but it's like this...
  (define oid (make-der-object-identifier "1.2.840.10040.4.1"))
  (define-method export-public-key ((m <dsa>) (key <dsa-public-key>))
    (encode (make-der-sequence
	     (make-der-sequence
	      oid
	      (make-der-sequence
	       (make-der-integer (slot-ref key 'p))
	       (make-der-integer (slot-ref key 'q))
	       (make-der-integer (slot-ref key 'g))))
	     (make-der-bit-string 
	      (encode (make-der-integer (slot-ref key 'Y)))))))

  (define-method import-public-key ((marker <dsa>) (in <bytevector>))
    (import-public-key DSA (open-bytevector-input-port in)))
  (define-method import-public-key ((marker <dsa>) (in <port>))
    (import-public-key DSA (read-asn.1-object in)))
  (define-method import-public-key ((marker <dsa>) (in <asn.1-sequence>))
    (let ((objects (slot-ref in 'sequence)))
      (unless (= 2 (length objects))
	(assertion-violation 'import-public-key "bad sequence size" in))
      (unless (and (is-a? (car objects) <asn.1-sequence>)
		   (is-a? (cadr objects) <der-bit-string>))
	(assertion-violation 'import-public-key "bad component" in))
      (let ((params (slot-ref (car objects) 'sequence)))
	(unless (= 2 (length params))
	  (assertion-violation 'import-public-key "bad sequence size" in))
	(unless (and (and (is-a? (car params) <der-object-identifier>)
			  (equal? (car params) oid))
		     (is-a? (cadr params) <asn.1-sequence>))
	  (assertion-violation 'import-public-key "bad component" in))
	(let ((ber-pqg (slot-ref (cadr params) 'sequence))
	      (Y (read-asn.1-object 
		  (open-bytevector-input-port (slot-ref (cadr objects) 'data))))
	      )
	  (unless (is-a? Y <der-integer>)
	    (assertion-violation 'import-public-key "bad component" in))
	  (make <dsa-public-key>
	    :p (bytevector->integer (slot-ref (car ber-pqg) 'bytes))
	    :q (bytevector->integer (slot-ref (cadr ber-pqg) 'bytes))
	    :g (bytevector->integer (slot-ref (caddr ber-pqg) 'bytes))
	    :Y (bytevector->integer (slot-ref Y 'bytes)))))))



)