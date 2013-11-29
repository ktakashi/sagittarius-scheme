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
	    (asn.1)
	    (util bytevector))

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
  ;; should we extends public key so that verify can be done with private key?  
  (define-class <dsa-private-key> (<private-key> <dsa-key-parameter>)
    ((Y :init-keyword :Y :init-value #f)
     (X :init-keyword :X)))
  (define-method write-object ((pr <dsa-private-key>) out)
    (format out "#<dsa-private-key~%")
    (format out "   p: ~x~%" (slot-ref pr 'p))
    (format out "   q: ~x~%" (slot-ref pr 'q))
    (format out "   g: ~x~%" (slot-ref pr 'g))
    (format out "   Y: ~x~%" (slot-ref pr 'Y))
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
	(let ((private (make <dsa-private-key> :p p :q q :g g :Y y :X x))
	      (public (make <dsa-public-key> :p p :q q :g g :Y y)))
	  (make-keypair private public)))))

  (define-method generate-public-key ((marker <dsa>) p q g y)
    (make <dsa-public-key> :p p :q q :g g :Y y))

  ;; According to OpenSSL
  ;; DSA signature data is packes like this;
  ;; DSASignature ::= SEQUENCE {
  ;;   r INTEGER,
  ;;   s INTEGER
  ;; }
  (define (get-digester who p)
    (cond ((assv (bitwise-length p) +key-length+) => cdr)
	  (else (error who "can't determine the hash algorithm"))))

  (define (dsa-sign bv key :key (prng (secure-random RC4))
		    (der-encode #t))
    (unless (is-a? key <dsa-private-key>)
      (raise-encrypt-error 'dsa-sign "invalid key" 'DSA))
    (let* ((p (slot-ref key 'p))
	   (q (slot-ref key 'q))
	   (g (slot-ref key 'g))
	   (x (slot-ref key 'X))
	   (digester (get-digester 'dsa-sign p))
	   (k (random prng q))
	   (r (mod (mod-expt g k p) q))
	   (xr+h (mod (+ (bytevector->integer (hash digester bv)) (* x r)) q))
	   (kinv (mod-inverse k q))
	   (s (mod (* (if (> xr+h q) (- xr+h q) xr+h) kinv) q)))
      (if der-encode
	  (encode (make-der-sequence
		   (make-der-integer r)
		   (make-der-integer s)))
	  (bytevector-append (integer->bytevector r) (integer->bytevector s)))))

  (define (dsa-verify M S key :key (der-encode #t))
    ;; signature must be the DER encoded object with r and s
    ;; if the S is not an ASN.1 object then reader will raise an error :)
    (define (parse-r&s p)
      (if der-encode
	  (let ((r&s (read-asn.1-object (open-bytevector-input-port S))))
	    (unless (and (is-a? r&s <asn.1-sequence>)
			 (= 2 (length (slot-ref r&s 'sequence))))
	      (error 'dsa-verify "invalid signature"))
	    (values (der-integer->integer (car (slot-ref r&s 'sequence)))
		    (der-integer->integer (cadr (slot-ref r&s 'sequence)))))
	  (let* ((digester (get-digester 'dsa-verify p))
		 (size     (hash-size digester)))
	    (let*-values (((r s) (bytevector-split-at* S size)))
	      (values (bytevector->integer r) (bytevector->integer s))))))

    (unless (is-a? key <dsa-public-key>) (error 'dsa-verify "invalid key"))
    (let ((q (slot-ref key 'q))
	  (p (slot-ref key 'p))
	  (g (slot-ref key 'g))
	  (y (slot-ref key 'Y)))
      (let-values (((r s) (parse-r&s p)))
	(unless (and (< 0 r q) (< 0 s q)) 
	  (error 'dsa-verify "invalid signature"))
	(let* ((digester (get-digester 'dsa-verify p))
	       (w  (mod-inverse s q))
	       (u1 (mod (* (bytevector->integer (hash digester M)) w) q))
	       (u2 (mod (* r w) q))
	       (u1d (mod-expt g u1 p))
	       (u2d (mod-expt y u2 p))
	       (v  (mod (mod (* u1d u2d) p) q)))
	  (or (= v r)
	      (error 'dsa-verify "inconsistent"))))))

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
  ;; For DSA public key, we need 'Public Key' structure.
  ;; I think this is a bad format since sign and verify always need
  ;; p, q and g. but it's like this...
  ;; If every one uses publicExponent + parameter format
  ;; I would go for that one though...
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
    (define (extract-param&keys who in)
      (let ((objects (slot-ref in 'sequence)))
	(unless (= 2 (length objects))
	  (assertion-violation who "bad sequence size" in))
	(unless (and (is-a? (car objects) <asn.1-sequence>)
		     (is-a? (cadr objects) <der-bit-string>))
	  (assertion-violation who "bad component" in))
	(let ((params (slot-ref (car objects) 'sequence)))
	  (unless (= 2 (length params))
	    (assertion-violation who "bad sequence size" in))
	  (unless (and (and (is-a? (car params) <der-object-identifier>)
			    (equal? (car params) oid))
		       (is-a? (cadr params) <asn.1-sequence>))
	    (assertion-violation who "bad component" in))
	  (let ((ber-pqg (slot-ref (cadr params) 'sequence))
		(k (read-asn.1-object 
		    (open-bytevector-input-port 
		     (slot-ref (cadr objects) 'data)))))
	    (unless (is-a? k <der-integer>)
	      (assertion-violation who "bad component" in))
	    (values (der-integer->integer (car ber-pqg))
		    (der-integer->integer (cadr ber-pqg))
		    (der-integer->integer (caddr ber-pqg))
		    (der-integer->integer k))))))
    (let-values (((p q g Y) (extract-param&keys 'import-public-key in)))
      (make <dsa-public-key> :p p :q q :g g :Y Y)))
  #|
      // From OpenSSL
      DSAPrivateKey ::= OCTETSTRING {
        version Version,
        p INTEGER,
        q INTEGER,
        g INTEGER,
        publicExponent  INTEGER,
        privateExponent INTEGER
      }
  |#
  (define-method export-private-key ((m <dsa>) (key <dsa-private-key>))
    ;; should we do like this or OpenSSL DSA private key format?
    (encode (make-der-sequence
	     (make-der-integer 0)
	     (make-der-integer (slot-ref key 'p))
	     (make-der-integer (slot-ref key 'q))
	     (make-der-integer (slot-ref key 'g))
	     (make-der-integer (slot-ref key 'Y))
	     (make-der-integer (slot-ref key 'X)))))

  (define-method import-private-key ((marker <dsa>) (in <bytevector>))
    (import-private-key DSA (open-bytevector-input-port in)))
  (define-method import-private-key ((marker <dsa>) (in <port>))
    (import-private-key DSA (read-asn.1-object in)))
  (define-method import-private-key ((marker <dsa>) (in <asn.1-sequence>))
    (let ((objs (slot-ref in 'sequence)))
      (unless (= 6 (length objs)) 
	(error 'import-private-key "invalid sequence size" in))
      (let ((pqgYX (cdr objs)))
	(make <dsa-private-key>
	  :p (der-integer->integer (car pqgYX))
	  :q (der-integer->integer (cadr pqgYX))
	  :g (der-integer->integer (caddr pqgYX))
	  :Y (der-integer->integer (cadddr pqgYX))
	  :X (der-integer->integer (car (cddddr pqgYX)))))))
)