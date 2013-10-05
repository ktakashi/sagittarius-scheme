;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; rsa.scm Cryptographic library
;;; 
#!compatible
(library (crypto key rsa)
    (export ;; padding block type
	    pkcs-v1.5-padding
	    PKCS-1-EME
	    PKCS-1-EMSA
	    ;; encrypt/decrypt
	    rsa-encrypt
	    rsa-decrypt
	    ;; signing/verify
	    rsa-sign
	    rsa-verify
	    ;; marker
	    RSA
	    )
    (import (rnrs)
	    (crypto pkcs)
	    (crypto key pair)
	    (math)
	    (math hash)
	    (math random)
	    (math prime)
	    (math helper)
	    (asn.1)
	    (asn.1 reader)
	    (clos user)
	    (srfi :1 lists)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto))

  (define-class <rsa-private-key> (<private-key>)
    ((modulus :init-keyword :modulus)
     (private-exponent :init-keyword :private-exponent)))
  (define (make-rsa-private-key m pe)
    (make <rsa-private-key>
      :modulus m :private-exponent pe))
  (define (rsa-private-key? o) (is-a? o <rsa-private-key>))

  (define-class <rsa-private-crt-key> (<rsa-private-key>)
    ((public-exponent :init-keyword :public-exponent)
     (p :init-keyword :p)	;; prime factor 1  
     (q :init-keyword :q)	;; prime factor 2  
     (dP :init-keyword :dP)	;; e*dP = 1 mod p-1P
     (dQ :init-keyword :dQ)	;; e*dQ = 1 mod q-1
     (qP :init-keyword :qP)))	;; q*qP = 1/q mod p
  (define (make-rsa-private-crt-key m e d p q)
    (make <rsa-private-crt-key>
      :modulus m :private-exponent d
      :public-exponent e
      :p p :q q
      :dP (mod d (- p 1))
      :dQ (mod d (- q 1))
      :qP (mod-inverse q p)))
  (define (rsa-private-crt-key? o) (is-a? o <rsa-private-crt-key>))
  (define-method object-equal? ((o1 <rsa-private-key>) (o2 <rsa-private-key>))
    (and (eqv? (slot-ref o1 'modulus) (slot-ref o2 'modulus))
	 (eqv? (slot-ref o1 'private-exponent) (slot-ref o2 'private-exponent))
	 ))
  (define-method object-equal? ((o1 <rsa-private-crt-key>)
				(o2 <rsa-private-crt-key>))
    (and (eqv? (slot-ref o1 'modulus) (slot-ref o2 'modulus))
	 (eqv? (slot-ref o1 'private-exponent) (slot-ref o2 'private-exponent))
	 (eqv? (slot-ref o1 'p) (slot-ref o2 'p))
	 (eqv? (slot-ref o1 'q) (slot-ref o2 'q))
	 (eqv? (slot-ref o1 'dP) (slot-ref o2 'dP))
	 (eqv? (slot-ref o1 'dQ) (slot-ref o2 'dQ))
	 (eqv? (slot-ref o1 'qP) (slot-ref o2 'qP))
	 ))
  (define-method write-object ((o <rsa-private-crt-key>) (p <port>))
    (let ((buf (call-with-string-output-port
		(lambda (out)
		  (format out "#<rsa-private-crt-key~%")
		  (format out "            modulus: ~x~%" (slot-ref o 'modulus))
		  (format out "    public exponent: ~x~%"
			  (slot-ref o 'public-exponent))
		  (format out "   private exponent: ~x~%"
			  (slot-ref o 'private-exponent))
		  (format out "           prime P: ~x~%" (slot-ref o 'p))
		  (format out "           prime Q: ~x~%" (slot-ref o 'q))
		  (format out "  prime exponent P: ~x~%" (slot-ref o 'dP))
		  (format out "  prime exponent Q: ~x~%" (slot-ref o 'dQ))
		  (format out "   crt coefficient: ~x~%" (slot-ref o 'qP))
		  (display #\> out)))))
      (display buf p)))

  (define-class <rsa-public-key> (<public-key>)
    ((modulus :init-keyword :modulus)
     (exponent :init-keyword :exponent)))
  (define (make-rsa-public-key m e)
    (make <rsa-public-key> :modulus m :exponent e))
  (define (rsa-public-key? o) (is-a? o <rsa-public-key>))
  (define-method object-equal? ((o1 <rsa-public-key>) (o2 <rsa-public-key>))
    (and (eqv? (slot-ref o1 'modulus) (slot-ref o2 'modulus))
	 (eqv? (slot-ref o1 'exponent) (slot-ref o2 'exponent))))
  (define-method write-object ((o <rsa-public-key>) (p <port>))
    (let ((buf (call-with-string-output-port
		(lambda (out)
		  (format out "#<rsa-pubic-key~%")
		  (format out "            modulus: ~x~%" (slot-ref o 'modulus))
		  (format out "    public exponent: ~x~%"
			  (slot-ref o 'exponent))
		  (display #\> out)))))
      (display buf p)))

  ;; marker class for generic functions
  (define-class <rsa> () ())
  (define RSA (make <rsa>))

  (define *rsa-min-keysize* 256)
  (define *rsa-max-keysize* 4096)

  (define (rsa-generate-private-key modulus private-exponent
				    :key (public-exponent #f)
					 (p #f)
					 (q #f))
    ;; if crt-key missing one of them
    (when (and (or public-exponent p q)
	       (not (and public-exponent p q)))
      (assertion-violation 'rsa-generate-private-key
			   "invalid crt-key generation"
			   public-exponent p q))
    (if (and public-exponent p q)
	(make-rsa-private-crt-key modulus public-exponent private-exponent p q)
	(make-rsa-private-key modulus private-exponent)))
  
  (define-method generate-private-key ((marker <rsa>)
				       (m <integer>)
				       (pe <integer>)
				       . rest)
    (apply rsa-generate-private-key m pe rest))

  (define-method generate-public-key ((marker <rsa>)
				      (m <integer>)
				      (e <integer>))
    (make-rsa-public-key m e))

  ;; RSA key-pair generator
  (define (rsa-generate-key-pair size prng e)
    (define (create-key-pair n e d p q)
      (let ((private (make-rsa-private-crt-key n e d p q))
	    (public (make-rsa-public-key n e)))
	(make-keypair private public)))

    (define (rsa-random-prime check)
      (let loop ((p (random-prime (/ size 16) :prng prng)))
	(if (and (or (not check)
		     (not (= p check)))
		 (= 1 (gcd (- p 1) e)))
	    p
	    (loop (random-prime (/ size 16 :prng prng))))))

    (when (or (< size *rsa-min-keysize*)
	      (> size *rsa-max-keysize*))
      (assertion-violation 'rsa-generate-key-pair
			   "invalid RSA key size" size))
    (unless (is-prime? e)
      (assertion-violation 'rsa-generate-key-pair
			   "exponent is not prime number" e))

    (let* ((p (rsa-random-prime #f))
	   (q (rsa-random-prime p))
	   (n (* p q))
	   (phi (* (- p 1) (- q 1))))
      (let ((d (mod-inverse e phi)))
	(create-key-pair n e d p q))))

  (define-method generate-key-pair ((marker <rsa>) . args)
    (let-keywords args
	((e 65537)
	 (size 1024)
	 (prng (secure-random RC4))
	 . others)
      (rsa-generate-key-pair size prng e)))

  ;; cipher
  (define-class <rsa-cipher-spi> (<cipher-spi>) ())
  (define-method initialize ((o <rsa-cipher-spi>) initargs)
    (let ((key (car initargs))
	  (rest (cdr initargs)))
      (let-keywords rest
	  ((prng (secure-random RC4))
	   (padding pkcs-v1.5-padding)
	   (block-type PKCS-1-EME)
	   . others)
	(let ((padder (if padding (padding prng key block-type) #f)))
	  (slot-set! o 'name 'RSA)
	  (slot-set! o 'key key)
	  (slot-set! o 'encrypt rsa-encrypt)
	  (slot-set! o 'decrypt rsa-decrypt)
	  (slot-set! o 'padder padder)
	  (slot-set! o 'signer rsa-sign)
	  (slot-set! o 'verifier rsa-verify)
	  (slot-set! o 'keysize rsa-keysize)
	  o))))
  ;; use marker
  (register-spi RSA <rsa-cipher-spi>)

  (define (rsa-keysize keysize) *rsa-max-keysize*)

  ;; util
  (define (rsa-mod-expt bv key . opt)
    (let ((sign? (if (null? opt) #f (car opt)))
	  (chunk (bytevector->integer bv)))
      (cond ((rsa-public-key? key)
	     (let ((r (mod-expt chunk
				(slot-ref key 'exponent)
				(slot-ref key 'modulus))))
	       (if sign?
		   (integer->bytevector r (bytevector-length bv))
		   (integer->bytevector r))))
	    ((rsa-private-crt-key? key)
	     ;; use CRT
	     (let ((p  (slot-ref key 'p))
		   (q  (slot-ref key 'q))
		   (dp (slot-ref key 'dP))
		   (dq (slot-ref key 'dQ))
		   (qp (slot-ref key 'qP)))
	       ;; b = chunk
	       (let* ((a (mod-expt chunk dp p)) ; b ^ dP mod p
		      (b (mod-expt chunk dq q)) ; b ^ dQ mod q
		      (c (mod (* (- a b) qp) p)) ; (a - b) * qp (mod p)
		      (d (+ b (* q c))))	   ; b + q * c
		 (if sign?
		     (integer->bytevector d (bytevector-length bv))
		     (integer->bytevector d)))))
	    ((rsa-private-key? key)
	     (let* ((modulus (slot-ref key 'modulus))
		    (private-exponent (slot-ref key 'private-exponent))
		    (a (mod-expt chunk private-exponent modulus)))
	       (if sign?
		   (integer->bytevector a (bytevector-length bv))
		   (integer->bytevector a))))
	    (else
	     (assertion-violation 'rsa-mod-expt
				  "invalid parameter" chunk key)))))

  ;; encrypt/decrypt
  ;; This procedure must be called from C and bv may be padded there.
  (define (rsa-encrypt bv key)
    (let ((key-length (align-size (slot-ref key 'modulus)))
	  (data-length (bytevector-length bv)))
      ;; for consistancy with JCE
      (when (> data-length key-length)
	(raise-encrypt-error 'rsa-encrypt
			     "too much data for RSA block"
			     'RSA))
      (rsa-mod-expt bv key)))
    
  (define (rsa-decrypt bv key)
    (let ((key-length (align-size (slot-ref key 'modulus)))
	  (data-length (bytevector-length bv)))
      ;; for consistancy with JCE
      (when (> data-length key-length)
	(raise-encrypt-error 'rsa-encrypt
			     "too much data for RSA block"
			     'RSA))
      (rsa-mod-expt bv key)))


  (define (rsa-sign bv key :key (encode pkcs1-emsa-pss-encode)
			   :allow-other-keys opt)
    (or (rsa-private-key? key)
	(raise-encrypt-error 'rsa-sign "invalid key" 'RSA))
    (let* ((modulus (slot-ref key 'modulus))
	   (len (bitwise-length modulus))
	   (data (apply encode bv (- len 1) opt)))
      (rsa-mod-expt data key #t)))

  (define (rsa-verify M S key :key (verify pkcs1-emsa-pss-verify)
			      :allow-other-keys opt)
    (or (rsa-public-key? key)
	(raise-decrypt-error 'rsa-verify "invalid key" 'RSA))
    (let* ((modulus (slot-ref key 'modulus))
	   (k       (align-size modulus)))
      ;; length check
      (unless (= k (bytevector-length S))
	(raise-decrypt-error 'rsa-verify "invalid signature" 'RSA))
      (let* ((EM (rsa-mod-expt S key))
	     (len (bitwise-length modulus))
	     (k (align-size (bit len)))
	     (em-len (bytevector-length EM)))
	;; padding 0, if the length is not the same as modulus
	(unless (= em-len k)
	  (let ((new (make-bytevector k 0)))
	    (bytevector-copy! EM 0 new (- k em-len) em-len)
	    (set! EM new)))
	(apply verify M EM (- len 1) opt))))
      
  ;; padding
  ;; PKCS#1 EME
  (define PKCS-1-EMSA 1)
  (define PKCS-1-EME  2)

  (define (pkcs-v1.5-padding prng key block-type)
    (define (encode data modulus)
      (let ((modulus-length (align-size modulus))
	    (message-length (bytevector-length data)))
	(when (> (+ message-length 11) modulus-length)
	  (raise-encode-error 'pkcs-v1.5-padding
			       "too much data for RSA block"))
	(let* ((ps-length (- modulus-length message-length 3))
	       (ps (if (= block-type PKCS-1-EME)
		       (read-random-bytes prng ps-length)
		       (make-bytevector ps-length #xFF))))
	  (when (= block-type PKCS-1-EME)
	    (do ((i 0 (+ i 1)))
		((= i ps-length) #t)
	      ;; transform zero bytes (if any) to non-zero random bytes
	      (when (zero? (bytevector-u8-ref ps i))
		(bytevector-u8-set! ps i (bytevector-u8-ref
					  (read-random-bytes prng 1) 0)))))
	  (let ((bv (make-bytevector (+ 2 ps-length 1 message-length) 0)))
	    ;; set block-type
	    (bytevector-u8-set! bv 1 block-type)
	    (bytevector-copy! ps 0 bv 2 ps-length)
	    (bytevector-u8-set! bv (+ 2 ps-length) 0)
	    (bytevector-copy! data 0 bv (+ 2 ps-length 1) message-length)
	    bv))))
    
    (define (decode data modulus)
      (let ((modulus-length (align-size modulus))
	    (message-length (bytevector-length data))
	    (type (bytevector-u8-ref data 0)))
	(unless (= type block-type)
	  (raise-decode-error 'pkcs-v1.5-padding "invalid block type"))
	(let ((from (do ((i 1 (+ i 1)))
			((= (bytevector-u8-ref data i) 0) (+ i 1))
		      (unless (or (= type PKCS-1-EME)
				  (= (bytevector-u8-ref data i) #xff))
			(raise-decode-error 'pkcs-v1.5-padding
					    "invalid EMSA padding"
					    (bytevector-u8-ref data i))))))
	  (when (or (>= from modulus-length)
		    (< from 9))
	    (raise-decode-error 'pkcs-v1.5-padding "invalid padding length"))
	  (let* ((len (- message-length from))
		 (bv (make-bytevector len 0)))
	    (bytevector-copy! data from bv 0 len)
	    bv))))

    (lambda (data pad?)
      (unless (or (rsa-public-key? key) (rsa-private-key? key))
	(if pad?
	    (raise-encode-error 'pkcs-v1.5-padding "public key required" key)
	    (raise-decode-error 'pkcs-v1.5-padding "private key required" key)))
      ;; both public and private key has the same slot
      (let ((modulus (slot-ref key 'modulus)))
	(if pad?
	    (encode data modulus)
	    (decode data modulus)))))

  #|
      RSAPublicKey ::= SEQUENCE {
          modulus           INTEGER,  -- n
          publicExponent    INTEGER   -- e
      }
  |#
  (define (rsa-export-public-key key)
    (let ((der (make-der-sequence
		(make-der-integer (slot-ref key 'modulus))
		(make-der-integer (slot-ref key 'exponent)))))
      (encode der)))
  (define-method export-public-key ((marker <rsa>) (key <rsa-public-key>))
    (rsa-export-public-key key))

  (define (rsa-import-public-key public)
    ;; validate
    (unless (is-a? public <asn.1-sequence>)
      (assertion-violation 'rsa-import-public-key "invalid der object" public))
    (let ((objects (slot-ref public 'sequence)))
      (unless (= 2 (length objects))
	(assertion-violation 'rsa-import-public-key
			     "bad sequence size" public))
      (or (for-all (lambda (o) (is-a? o <der-integer>)) objects)
	  (assertion-violation 'rsa-import-public-key
			       "all sequence componenet must be <der-integer>"
			       objects))
      (generate-public-key RSA
			   (bytevector->integer
			    (slot-ref (car objects) 'bytes))
			   (bytevector->integer
			    (slot-ref (cadr objects) 'bytes)))))
  (define-method import-public-key ((marker <rsa>) (in <bytevector>))
    (import-public-key marker (open-bytevector-input-port in)))
  (define-method import-public-key ((marker <rsa>) (in <port>))
    (import-public-key marker (read-asn.1-object in)))
  (define-method import-public-key ((marker <rsa>) (in <asn.1-sequence>))
    (rsa-import-public-key in))

  #|
      RSAPrivateKey ::= SEQUENCE {
          version           Version,
          modulus           INTEGER,  -- n
          publicExponent    INTEGER,  -- e
          privateExponent   INTEGER,  -- d
          prime1            INTEGER,  -- p
          prime2            INTEGER,  -- q
          exponent1         INTEGER,  -- d mod (p-1)
          exponent2         INTEGER,  -- d mod (q-1)
          coefficient       INTEGER,  -- (inverse of q) mod p
          otherPrimeInfos   OtherPrimeInfos OPTIONAL -- We do not support this.
      }
      Version ::= INTEGER { two-prime(0), multi(1) }
         (CONSTRAINED BY
         {-- version must be multi if otherPrimeInfos present --})
  |#
  (define (rsa-export-private-key private)
    (let ((der (make-der-sequence
		;; version must be always 0 since we do not support
		;; otherPrimeInfos.
		(make-der-integer 0)
		(make-der-integer (slot-ref private 'modulus))
		(make-der-integer (slot-ref private 'public-exponent))
		(make-der-integer (slot-ref private 'private-exponent))
		(make-der-integer (slot-ref private 'p))
		(make-der-integer (slot-ref private 'q))
		(make-der-integer (slot-ref private 'dP))
		(make-der-integer (slot-ref private 'dQ))
		(make-der-integer (slot-ref private 'dP)))))
      (encode der)))
  (define-method export-private-key ((marker <rsa>) (key <rsa-private-crt-key>))
    (rsa-export-private-key key))
  
  (define (rsa-import-private-key private)
    ;; validate
    (unless (is-a? private <asn.1-sequence>)
      (assertion-violation 'rsa-import-private-key 
			   "invalid der object" private))
    (let ((objects (slot-ref private 'sequence)))
      (unless (<= 9 (length objects) 10)
	(assertion-violation 'rsa-import-private-key
			     "bad sequence size" private))
      (do ((i 0 (+ i 1)) (o objects (cdr o)))
	  ((= i 9))
	(unless (is-a? (car o) <der-integer>)
	  (assertion-violation 'rsa-import-private-key
			       "invalid object" i (car o))))
      (let ((version (car objects))
	    (modulus (cadr objects))
	    (public-exponent (third objects))
	    (private-exponent (fourth objects))
	    (p (fifth objects))
	    (q (sixth objects))
	    ;; the rest we don't need
	    )
	(unless (<= 0 (bytevector->integer (slot-ref version 'bytes)) 1)
	  (assertion-violation 'rsa-import-private-key
			       "wrong version for RSA private key" version))
	(rsa-generate-private-key 
	 (bytevector->integer
	  (slot-ref modulus 'bytes))
	 (bytevector->integer
	  (slot-ref private-exponent 'bytes))
	 :public-exponent (bytevector->integer
			   (slot-ref public-exponent 'bytes))
	 :p (bytevector->integer (slot-ref p 'bytes))
	 :q (bytevector->integer (slot-ref q 'bytes))))))

  (define-method import-private-key ((marker <rsa>) (in <bytevector>))
    (import-private-key marker (open-bytevector-input-port in)))
  (define-method import-private-key ((marker <rsa>) (in <port>))
    (import-private-key marker (read-asn.1-object in)))
  (define-method import-private-key ((marker <rsa>) (in <asn.1-sequence>))
    (rsa-import-private-key in))
)