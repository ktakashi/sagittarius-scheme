;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; rsa.scm Cryptographic library
;;; 
#!core
(library (crypto rsa)
    (export ;; padding block type
	    pkcs-v1.5-padding
	    PKCS-1-EME
	    PKCS-1-EMSA
	    rsa-oaep-padding
	    ;; encrypt/decrypt
	    rsa-encrypt
	    rsa-decrypt
	    ;; signing/verify
	    rsa-sign
	    rsa-verify
	    ;; marker
	    RSA

	    ;; CLOS
	    <rsa-private-key> <rsa-private-crt-key>
	    <rsa-public-key>
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
	    (util bytevector)
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
  (define RSA :rsa)

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
  
  (define-method generate-private-key ((marker (eql RSA))
				       (m <integer>)
				       (pe <integer>)
				       . rest)
    (apply rsa-generate-private-key m pe rest))

  (define-method generate-public-key ((marker (eql RSA))
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

  (define-method generate-key-pair ((marker (eql RSA)) . args)
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
	  (slot-set! o 'encrypt (%rsa-encrypt padding))
	  (slot-set! o 'decrypt (%rsa-decrypt padding))
	  (slot-set! o 'padder padder)
	  (slot-set! o 'signer rsa-sign)
	  (slot-set! o 'verifier rsa-verify)
	  (slot-set! o 'keysize rsa-keysize)
	  o))))
  ;; use marker
  (register-spi RSA <rsa-cipher-spi>)

  (define (rsa-keysize keysize) *rsa-max-keysize*)

  ;; util
  ;; FIXME this kinda sucks!
  (define (rsa-mod-expt bv key padding?)
    (let ((chunk (bytevector->integer bv)))
      (cond ((rsa-public-key? key)
	     (let ((r (mod-expt chunk
				(slot-ref key 'exponent)
				(slot-ref key 'modulus))))
	       (if padding?
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
		 (if padding?
		     (integer->bytevector d (bytevector-length bv))
		     (integer->bytevector d)))))
	    ((rsa-private-key? key)
	     (let* ((modulus (slot-ref key 'modulus))
		    (private-exponent (slot-ref key 'private-exponent))
		    (a (mod-expt chunk private-exponent modulus)))
	       (if padding?
		   (integer->bytevector a (bytevector-length bv))
		   (integer->bytevector a))))
	    (else
	     (assertion-violation 'rsa-mod-expt
				  "invalid parameter" chunk key)))))

  ;; encrypt/decrypt
  ;; This procedure must be called from C and bv may be padded there.
  (define (%rsa-encrypt padding?)
    (lambda (bv key)
      (let ((key-length (align-size (slot-ref key 'modulus)))
	    (data-length (bytevector-length bv)))
	;; for consistancy with JCE
	(when (> data-length key-length)
	  (raise-encrypt-error 'rsa-encrypt
			       "too much data for RSA block"
			       'RSA))
	(rsa-mod-expt bv key padding?))))
  ;; backward compatibility
  (define rsa-encrypt (%rsa-encrypt #f))
  
  (define (%rsa-decrypt padding?)
    (lambda (bv key)
      (let ((key-length (align-size (slot-ref key 'modulus)))
	    (data-length (bytevector-length bv)))
	;; for consistancy with JCE
	(when (> data-length key-length)
	  (raise-encrypt-error 'rsa-encrypt
			       "too much data for RSA block"
			       'RSA))
	(rsa-mod-expt bv key padding?))))
  (define rsa-decrypt (%rsa-decrypt #f))

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
      (let* ((EM (rsa-mod-expt S key #f))
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

  ;; OAEP
  ;; Reference: RFC3447
  ;;  https://tools.ietf.org/html/rfc3447
  ;; TODO separate encoder and decoder to (crypto pkcs)
  ;; hasher must be given explicitly
  (define (rsa-oaep-padding hasher :key (mgf mgf-1) (label #vu8()))
    ;; ignore block-type
    (let* ((algo (hash-algorithm hasher))
	   (hlen (hash-size algo))
	   (lhash (hash algo label)))
      ;; TODO check label length against hash function input limitation.
      (lambda (prng key block-type)
	(define (encode data modulus)
;; 	  (define (fixup ps)
;; 	    ;; make sure it doesn't contain 01
;; 	    (define len (bytevector-length ps))
;; 	    (let loop ((i 0))
;; 	      (cond ((= i len) ps)
;; 		    ((= (bytevector-u8-ref ps i) #x01)
;; 		     (bytevector-u8-set! ps i 0)
;; 		     (loop (+ i 1)))
;; 		    (else (loop (+ i 1))))))
	  (let* ((modulus-length (align-size modulus))
;; 		 (ps (fixup (read-random-bytes prng (- modulus-length
;; 						       (bytevector-length data)
;; 						       (* hlen 2)
;; 						       2))))
		 ;; FUCK!!! Above is *perfectly* fine according to the
		 ;; RFC but bloody bouncy castle expects PS is all 0
		 ;; padded and it's already widely used.
		 (ps (make-bytevector (- modulus-length
					 (bytevector-length data)
					 (* hlen 2)
					 2) 0))
		 (db (bytevector-append lhash ps #vu8(#x01) data))
		 (seed (read-random-bytes prng hlen))
		 (db-mask (mgf seed (- modulus-length hlen 1) algo))
		 (masked-db (bytevector-xor db db-mask))
		 (seed-mask (mgf masked-db hlen algo))
		 (masked-seed (bytevector-xor seed seed-mask)))
	    (bytevector-append #vu8(#x00) masked-seed masked-db)))
	(define (decode data modulus)
	  (define (parse-em data)
	    (values (bytevector-u8-ref data 0)
		    (bytevector-copy data 1 (+ hlen 1))
		    ;; TODO length check
		    (bytevector-copy data (+ hlen 1))))
	  (define (parse-db db)
	    (define (find-ps-end db)
	      (let loop ((i hlen))
		(if (= (bytevector-u8-ref db i) #x01)
		    i
		    (loop (+ i 1)))))
	    (let ((ps-end (find-ps-end db)))
	      (values (bytevector-copy db 0 hlen)
		      (bytevector-copy db hlen ps-end)
		      (bytevector-u8-ref db ps-end)
		      (bytevector-copy db (+ ps-end 1)))))
	  (let-values (((Y masked-seed masked-db) (parse-em data)))
	    (let* ((seed-mask (mgf masked-db hlen algo))
		   (seed (bytevector-xor masked-seed seed-mask))
		   (modulus-length (align-size modulus))
		   (db-mask (mgf seed (- modulus-length hlen 1) algo))
		   (db (bytevector-xor masked-db db-mask)))
	      (let-values (((lhash-dash ps one M) (parse-db db)))
		(unless (and (bytevector=? lhash lhash-dash)
			     (zero? Y)
			     (= one #x01))
		  (raise-decode-error 'rsa-oaep-padding "decryption error"))
		M))))
	(lambda (data pad?)
	  (unless (or (rsa-public-key? key) (rsa-private-key? key))
	    (raise-encode-error 'rsa-oaep-padding 
				(if pad?
				    "public key required" 
				    "private key required")
				key))
	  (let ((modulus (slot-ref key 'modulus)))
	    (define (input-blocksize)
	      (let ((core-size (- (align-size (bit (bitwise-length modulus)))
				  (if pad? 1 0))))
		(if pad?
		    (- core-size 1 (* hlen 2))
		    core-size)))
	    ;; input length check
	    (when (> (bytevector-length data) (input-blocksize))
	      (raise-encode-error 'rsa-oaep-padding
				  "too much data for RSA block size"))

	    (if pad?
		(encode data modulus)
		(decode data modulus)))))))

  ;; PKCS#1 EME
  (define-constant PKCS-1-EMSA 1)
  (define-constant PKCS-1-EME  2)

  ;; TODO separate encoder and decoder to (crypto pkcs)
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
		(do ((j 0 (+ j 1)) (r (read-random-bytes prng 1)
				      (read-random-bytes prng 1)))
		    ((not (zero? (bytevector-u8-ref r 0)))
		     (bytevector-u8-set! ps i (bytevector-u8-ref r 0)))))))
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
	    (type (bytevector-u8-ref data 1)))
	(unless (= type block-type)
	  (raise-decode-error 'pkcs-v1.5-padding "invalid block type"))
	(let ((from (do ((i 2 (+ i 1)))
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
  ;; we don't need marker but backward compatibility
  (define-method export-public-key ((marker (eql RSA)) (key <rsa-public-key>))
    (rsa-export-public-key key))
  (define-method export-public-key ((key <rsa-public-key>))
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
  (define-method import-public-key ((marker (eql RSA)) (in <bytevector>))
    (import-public-key marker (open-bytevector-input-port in)))
  (define-method import-public-key ((marker (eql RSA)) (in <port>))
    (import-public-key marker (read-asn.1-object in)))
  (define-method import-public-key ((marker (eql RSA)) (in <asn.1-sequence>))
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
  (define-method export-private-key ((marker (eql RSA)) (key <rsa-private-crt-key>))
    (rsa-export-private-key key))
  (define-method export-private-key ((key <rsa-private-crt-key>))
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
	 (der-integer->integer modulus)
	 (der-integer->integer private-exponent)
	 :public-exponent (der-integer->integer public-exponent)
	 :p (der-integer->integer p)
	 :q (der-integer->integer q)))))

  (define-method import-private-key ((marker (eql RSA)) (in <bytevector>))
    (import-private-key marker (open-bytevector-input-port in)))
  (define-method import-private-key ((marker (eql RSA)) (in <port>))
    (import-private-key marker (read-asn.1-object in)))
  (define-method import-private-key ((marker (eql RSA)) (in <asn.1-sequence>))
    (rsa-import-private-key in))
)
