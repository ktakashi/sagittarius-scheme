;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/eddsa.scm - EdDSA cipher
;;;
;;;  Copyright (c) 2021 Takashi Kato. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; references:
;;  - [RFC 8032](https://datatracker.ietf.org/doc/html/rfc8032)

#!nounbound
#!read-macro=sagittarius/bv-string
(library (crypto eddsa)
    (export EdDSA
	    Ed25519 Ed25519ctx Ed25519ph 
	    Ed448 Ed448ph
	    
	    <eddsa-key> eddsa-key?
	    eddsa-key-parameter
	    <eddsa-private-key> eddsa-private-key?
	    eddsa-private-key-random eddsa-private-key-public-key
	    <eddsa-public-key> eddsa-public-key?
	    eddsa-public-key-data

	    ed25519-key? ed25519-public-key? ed25519-private-key?
	    ed448-key? ed448-public-key? ed448-private-key?
	    
	    ;; low level APIs?
	    make-eddsa-signer make-eddsa-verifier
	    ed25519-scheme ed25519ctx-scheme ed25519ph-scheme
	    ed448-scheme ed448ph-scheme
	    )
    (import (rnrs)
	    (clos user)
	    (math)
	    (math ec)
	    (math modular)
	    (crypto key pair)
	    (sagittarius crypto)
	    (sagittarius) ;; for bytevector->integer/endian
	    (core misc)	  ;; for define-vector-type;
	    (util bytevector)
	    (srfi :2 and-let*)
	    )
;;; Interfaces
(define EdDSA :eddsa)
(define Ed25519 :ed25519)
(define Ed25519ctx :ed25519ctx)
(define Ed25519ph :ed25519ph)
(define Ed448 :ed448)
(define Ed448ph :ed448ph)

(define-class <eddsa-key> ()
  ((parameter :init-keyword :parameter :reader eddsa-key-parameter)))
(define (eddsa-key? o) (is-a? o <eddsa-key>))

(define-class <eddsa-private-key> (<private-key> <eddsa-key>)
  ((random :init-keyword :random :reader eddsa-private-key-random)
   (public-key :init-keyword :public-key :reader eddsa-private-key-public-key)))
(define (eddsa-private-key? o) (is-a? o <eddsa-private-key>))

(define-class <eddsa-public-key> (<public-key> <eddsa-key>)
  ((data :init-keyword :data :reader eddsa-public-key-data)))
(define (eddsa-public-key? o) (is-a? o <eddsa-public-key>))

(define (ed25519-key? key)
  (and (eddsa-key? key)
       (eq? 'ed25519 (eddsa-parameter-name (eddsa-key-parameter key)))))
(define (ed448-key? key)
  (and (eddsa-key? key)
       (eq? 'ed448 (eddsa-parameter-name (eddsa-key-parameter key)))))
(define (ed25519-public-key? key)
  (and (eddsa-public-key? key) (ed25519-key? key)))
(define (ed25519-private-key? key)
  (and (eddsa-private-key? key) (ed25519-key? key)))
(define (ed448-public-key? key)
  (and (eddsa-public-key? key) (ed448-key? key)))
(define (ed448-private-key? key)
  (and (eddsa-private-key? key) (ed448-key? key)))

(define-class <eddsa-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <eddsa-cipher-spi>) initargs)
  (define (check-ed25519 key)
    (unless (ed25519-key? key)
      (assertion-violation 'eddsa-cipher "Wrong type for the key")))
  (define (check-ed448 key)
    (unless (ed448-key? key)
      (assertion-violation 'eddsa-cipher "Wrong type for the key")))
  (define (type->scheme type key)
    (cond ((eq? type Ed25519)    (check-ed25519 key) ed25519-scheme)
	  ((eq? type Ed25519ctx) (check-ed25519 key) ed25519ctx-scheme)
	  ((eq? type Ed25519ph)  (check-ed25519 key) ed25519ph-scheme)
	  ((eq? type Ed448)   (check-ed448 key) ed448-scheme)
	  ((eq? type Ed448ph) (check-ed448 key) ed448ph-scheme)
	  (else (if (ed25519-key? key) ed25519-scheme ed448-scheme))))
  (let ((key (car initargs)))
    (let-keywords (cdr initargs)
	((type #f) . ignore)
      (let ((scheme (type->scheme type key)))
	(slot-set! o 'name 'EdDSA)
	(slot-set! o 'key key)
	(slot-set! o 'encrypt 
		   (lambda ignore (error 'encrypt "not supported in EdDSA")))
	(slot-set! o 'decrypt
		   (lambda ignore (error 'decrypt "not supported in EdDSA")))
	(slot-set! o 'padder #f)
	(slot-set! o 'signer (make-eddsa-signer scheme))
	(slot-set! o 'verifier (make-eddsa-verifier scheme))
	(slot-set! o 'keysize #f)))))
(register-spi EdDSA <eddsa-cipher-spi>)

;; this is super generic ;)
(define-method export-public-key ((key <eddsa-public-key>))
  (eddsa-public-key-data key))
(define-method export-private-key ((key <eddsa-private-key>))
  (eddsa-private-key-random key))

;;; Ed25519 Framework
(define-method generate-public-key ((m (eql Ed25519)) data)
  (generate-ed25519-public-key data))
(define-method import-public-key ((m (eql Ed25519)) data)
  (generate-ed25519-public-key data))
(define-method export-public-key ((m (eql Ed25519)) (key <eddsa-public-key>))
  (eddsa-public-key-data key))

(define-method generate-private-key ((m (eql Ed25519)) random)
  (generate-ed25519-private-key random))
(define-method import-private-key ((m (eql Ed25519)) random)
  (generate-ed25519-private-key random))
(define-method export-private-key ((m (eql Ed25519)) (key <eddsa-private-key>))
  (eddsa-private-key-random key))

(define-method generate-key-pair ((m (eql Ed25519))
				  ;; should we start using ChaCha20?
				  :key (prng (secure-random RC4)))
  (generate-ed25519-key-pair prng))
(define-class <ed25519-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <ed25519-cipher-spi>) initargs)
  (let ((key (car initargs)))
    (slot-set! o 'name 'Ed25519)
    (slot-set! o 'key key)
    (slot-set! o 'encrypt 
	       (lambda ignore (error 'encrypt "not supported in Ed25519")))
    (slot-set! o 'decrypt
	       (lambda ignore (error 'decrypt "not supported in Ed25519")))
    (slot-set! o 'padder #f)
    (slot-set! o 'signer (make-eddsa-signer ed25519-scheme))
    (slot-set! o 'verifier (make-eddsa-verifier ed25519-scheme))
    (slot-set! o 'keysize #f)))
(register-spi Ed25519 <ed25519-cipher-spi>)

;;; Ed25519ctx Framework
(define-method generate-public-key ((m (eql Ed25519ctx)) data)
  (generate-ed25519-public-key data))
(define-method import-public-key ((m (eql Ed25519ctx)) data)
  (generate-ed25519-public-key data))
(define-method export-public-key ((m (eql Ed25519ctx)) (key <eddsa-public-key>))
  (eddsa-public-key-data key))

(define-method generate-private-key ((m (eql Ed25519ctx)) random)
  (generate-ed25519-private-key random))
(define-method import-private-key ((m (eql Ed25519ctx)) random)
  (generate-ed25519-private-key random))
(define-method export-private-key ((m (eql Ed25519ctx)) (key <eddsa-private-key>))
  (eddsa-private-key-random key))

(define-method generate-key-pair ((m (eql Ed25519ctx))
				  :key (prng (secure-random RC4)))
  (generate-ed25519-key-pair prng))
(define-class <ed25519ctx-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <ed25519ctx-cipher-spi>) initargs)
  (let ((key (car initargs)))
    (slot-set! o 'name 'Ed25519)
    (slot-set! o 'key key)
    (slot-set! o 'encrypt 
	       (lambda ignore (error 'encrypt "not supported in Ed25519ctx")))
    (slot-set! o 'decrypt
	       (lambda ignore (error 'decrypt "not supported in Ed25519ctx")))
    (slot-set! o 'padder #f)
    (slot-set! o 'signer (make-eddsa-signer ed25519ctx-scheme))
    (slot-set! o 'verifier (make-eddsa-verifier ed25519ctx-scheme))
    (slot-set! o 'keysize #f)))
(register-spi Ed25519ctx <ed25519ctx-cipher-spi>)

;;; Ed25519ph Framework
(define-method generate-public-key ((m (eql Ed25519ph)) data)
  (generate-ed25519-public-key data))
(define-method import-public-key ((m (eql Ed25519ph)) data)
  (generate-ed25519-public-key data))
(define-method export-public-key ((m (eql Ed25519ph)) (key <eddsa-public-key>))
  (eddsa-public-key-data key))

(define-method generate-private-key ((m (eql Ed25519ph)) random)
  (generate-ed25519-private-key random))
(define-method import-private-key ((m (eql Ed25519ph)) random)
  (generate-ed25519-private-key random))
(define-method export-private-key ((m (eql Ed25519ph)) (key <eddsa-private-key>))
  (eddsa-private-key-random key))

(define-method generate-key-pair ((m (eql Ed25519ph))
				  :key (prng (secure-random RC4)))
  (generate-ed25519-key-pair prng))
(define-class <ed25519ph-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <ed25519ph-cipher-spi>) initargs)
  (let ((key (car initargs)))
    (slot-set! o 'name 'Ed25519)
    (slot-set! o 'key key)
    (slot-set! o 'encrypt 
	       (lambda ignore (error 'encrypt "not supported in Ed25519ph")))
    (slot-set! o 'decrypt
	       (lambda ignore (error 'decrypt "not supported in Ed25519ph")))
    (slot-set! o 'padder #f)
    (slot-set! o 'signer (make-eddsa-signer ed25519ph-scheme))
    (slot-set! o 'verifier (make-eddsa-verifier ed25519ph-scheme))
    (slot-set! o 'keysize #f)))
(register-spi Ed25519ph <ed25519ph-cipher-spi>)

;; Ed448 Framework
(define-method generate-public-key ((m (eql Ed448)) data)
  (generate-ed448-public-key data))
(define-method import-public-key ((m (eql Ed448)) data)
  (generate-ed448-public-key data))
(define-method export-public-key ((m (eql Ed448)) (key <eddsa-public-key>))
  (eddsa-public-key-data key))

(define-method generate-private-key ((m (eql Ed448)) random)
  (generate-ed448-private-key random))
(define-method import-private-key ((m (eql Ed448)) random)
  (generate-ed448-private-key random))
(define-method export-private-key ((m (eql Ed448)) (key <eddsa-private-key>))
  (eddsa-private-key-random key))

(define-method generate-key-pair ((m (eql Ed448))
				  ;; should we start using ChaCha20?
				  :key (prng (secure-random RC4)))
  (generate-ed448-key-pair prng))
(define-class <ed448-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <ed448-cipher-spi>) initargs)
  (let ((key (car initargs)))
    (slot-set! o 'name 'Ed25519)
    (slot-set! o 'key key)
    (slot-set! o 'encrypt 
	       (lambda ignore (error 'encrypt "not supported in Ed448")))
    (slot-set! o 'decrypt
	       (lambda ignore (error 'decrypt "not supported in Ed448")))
    (slot-set! o 'padder #f)
    (slot-set! o 'signer (make-eddsa-signer ed448-scheme))
    (slot-set! o 'verifier (make-eddsa-verifier ed448-scheme))
    (slot-set! o 'keysize #f)))
(register-spi Ed448 <ed448-cipher-spi>)

;; Ed448ph Framework
(define-method generate-public-key ((m (eql Ed448ph)) data)
  (generate-ed448-public-key data))
(define-method import-public-key ((m (eql Ed448ph)) data)
  (generate-ed448-public-key data))
(define-method export-public-key ((m (eql Ed448ph)) (key <eddsa-public-key>))
  (eddsa-public-key-data key))

(define-method generate-private-key ((m (eql Ed448ph)) random)
  (generate-ed448-private-key random))
(define-method import-private-key ((m (eql Ed448ph)) random)
  (generate-ed448-private-key random))
(define-method export-private-key ((m (eql Ed448ph)) (key <eddsa-private-key>))
  (eddsa-private-key-random key))

(define-method generate-key-pair ((m (eql Ed448ph))
				  ;; should we start using ChaCha20?
				  :key (prng (secure-random RC4)))
  (generate-ed448-key-pair prng))
(define-class <ed448ph-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <ed448ph-cipher-spi>) initargs)
  (let ((key (car initargs)))
    (slot-set! o 'name 'Ed448)
    (slot-set! o 'key key)
    (slot-set! o 'encrypt 
	       (lambda ignore (error 'encrypt "not supported in Ed448ph")))
    (slot-set! o 'decrypt
	       (lambda ignore (error 'decrypt "not supported in Ed448ph")))
    (slot-set! o 'padder #f)
    (slot-set! o 'signer (make-eddsa-signer ed448ph-scheme))
    (slot-set! o 'verifier (make-eddsa-verifier ed448ph-scheme))
    (slot-set! o 'keysize #f)))
(register-spi Ed448ph <ed448ph-cipher-spi>)


;;; Ed25519 implementations
(define (generate-ed25519-key-pair prng)
  (let* ((random (read-random-bytes prng 32))
	 (private-key (generate-ed25519-private-key random)))
    (make-keypair private-key
		  (eddsa-private-key-public-key private-key))))

(define (generate-ed25519-public-key data)
  (make <eddsa-public-key> :data data :parameter ed25519-parameter))

(define (generate-ed25519-private-key random)
    #|
5.1.5.  Key Generation

   The private key is 32 octets (256 bits, corresponding to b) of
   cryptographically secure random data.  See [RFC4086] for a discussion
   about randomness.

   The 32-byte public key is generated by the following steps.

   1.  Hash the 32-byte private key using SHA-512, storing the digest in
       a 64-octet large buffer, denoted h.  Only the lower 32 bytes are
       used for generating the public key.

   2.  Prune the buffer: The lowest three bits of the first octet are
       cleared, the highest bit of the last octet is cleared, and the
       second highest bit of the last octet is set.

   3.  Interpret the buffer as the little-endian integer, forming a
       secret scalar s.  Perform a fixed-base scalar multiplication
       [s]B.

   4.  The public key A is the encoding of the point [s]B.  First,
       encode the y-coordinate (in the range 0 <= y < p) as a little-
       endian string of 32 octets.  The most significant bit of the
       final octet is always zero.  To form the encoding of the point
       [s]B, copy the least significant bit of the x coordinate to the
       most significant bit of the final octet.  The result is the
       public key.
  |#
  (define (generate-public-key random)
    (let ((h (hash SHA-512 random))
	  (l (make-bytevector 32)))
      ;; 1
      (bytevector-copy! h 0 l 0 32)
      ;; 2
      (eddsa-clamp! l 3 254 256)
      ;; 3
      (let* ((s (bytevector->integer/endian l (endianness little)))
	     (sB (ed-point-mul ed25519-parameter
			       (eddsa-parameter-B ed25519-parameter)
			       s)))
	;; 4
	(ed-point-encode-base ed25519-parameter sB
			      (eddsa-parameter-b ed25519-parameter)))))
  (unless (= (bytevector-length random) 32)
    (assertion-violation 'generate-ed25519-private-key "Invalid key size"))
  (let ((pub (generate-ed25519-public-key (generate-public-key random))))
    (make <eddsa-private-key> :parameter ed25519-parameter
	  :random random :public-key pub)))

(define (generate-ed448-key-pair prng)
  (let* ((random (read-random-bytes prng 57))
	 (private-key (generate-ed448-private-key random)))
    (make-keypair private-key
		  (eddsa-private-key-public-key private-key))))
(define (generate-ed448-public-key data)
  (make <eddsa-public-key> :data data :parameter ed448-parameter))
(define (generate-ed448-private-key random)
  #|
5.2.5.  Key Generation

   The private key is 57 octets (456 bits, corresponding to b) of
   cryptographically secure random data.  See [RFC4086] for a discussion
   about randomness.

   The 57-byte public key is generated by the following steps:

   1.  Hash the 57-byte private key using SHAKE256(x, 114), storing the
       digest in a 114-octet large buffer, denoted h.  Only the lower 57
       bytes are used for generating the public key.

   2.  Prune the buffer: The two least significant bits of the first
       octet are cleared, all eight bits the last octet are cleared, and
       the highest bit of the second to last octet is set.

   3.  Interpret the buffer as the little-endian integer, forming a
       secret scalar s.  Perform a known-base-point scalar
       multiplication [s]B.

   4.  The public key A is the encoding of the point [s]B.  First encode
       the y-coordinate (in the range 0 <= y < p) as a little-endian
       string of 57 octets.  The most significant bit of the final octet
       is always zero.  To form the encoding of the point [s]B, copy the
       least significant bit of the x coordinate to the most significant
       bit of the final octet.  The result is the public key.
  |#
  (define (generate-public-key random)
    (let ((h (hash SHAKE256 random :size 114))
	  (l (make-bytevector 57)))
      ;; 1
      (bytevector-copy! h 0 l 0 57)
      ;; 2
      (eddsa-clamp! l 2 447 456)
      ;; 3
      (let* ((s (bytevector->integer/endian l (endianness little)))
	     (sB (ed-point-mul ed448-parameter
			       (eddsa-parameter-B ed448-parameter)
			       s)))
	;; 4
	(ed-point-encode-base ed448-parameter sB
			      (eddsa-parameter-b ed448-parameter)))))
  (unless (= (bytevector-length random) 57)
    (assertion-violation 'generate-ed448-private-key "Invalid key size"))
  (let ((pub (generate-ed448-public-key (generate-public-key random))))
    (make <eddsa-private-key> :parameter ed448-parameter
	  :random random :public-key pub)))

(define (eddsa-clamp! a c n b)
  (do ((i 0 (+ i 1)))
      ((= i c))
    (bytevector-u8-set! a (div i 8)
     (bitwise-and (bytevector-u8-ref a (div i 8))
		  (bitwise-not (bitwise-arithmetic-shift-left 1 (mod i 8))))))
  (bytevector-u8-set! a (div n 8)
   (bitwise-ior (bytevector-u8-ref a (div n 8))
		(bitwise-arithmetic-shift-left 1 (mod n 8))))
  (do ((i (+ n 1) (+ i 1)))
      ((= i b) a)
    (bytevector-u8-set! a (div i 8)
     (bitwise-and (bytevector-u8-ref a (div i 8))
		  (bitwise-not (bitwise-arithmetic-shift-left 1 (mod i 8)))))))

;;; Ed25519 parameters
(define-vector-type eddsa-parameter
  (make-eddsa-parameter name p b c n d a B L) eddsa-parameter?
  (name eddsa-parameter-name)
  (p eddsa-parameter-p)
  (b eddsa-parameter-b)
  (c eddsa-parameter-c)
  (n eddsa-parameter-n)
  (d eddsa-parameter-d)
  (a eddsa-parameter-a)
  (B eddsa-parameter-B)
  (L eddsa-parameter-L))

;; 5.1 Ed25519ph, Ed25519ctx, and Ed25519
(define ed25519-parameter
  (let ((xb #x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a)
	(yb #x6666666666666666666666666666666666666666666666666666666666666658)
	(p (ec-field-fp-p
	    (elliptic-curve-field (ec-parameter-curve curve25519))))
	(d #x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3))
    (make-eddsa-parameter
     'ed25519
     p					       ;; p = 2^255 - 19
     256				       ;; b
     3					       ;; c 
     254				       ;; n
     d					       ;; d
     -1					       ;; a
     (make-ed-point xb yb 1 (mod (* xb yb) p)) ;; B (x y z t)
     (ec-parameter-n curve25519) ;; L (ec-parameter-n = order)
     )))
;; once we have short Weierstrass form in (math ec), do like above
(define ed448-parameter
  (let ((p (- (expt 2 448) (expt 2 224) 1))
	(xb #x4f1970c66bed0ded221d15a622bf36da9e146570470f1767ea6de324a3d3a46412ae1af72ab66511433b80e18b00938e2626a82bc70cc05e)
	(yb #x693f46716eb6bc248876203756c9c7624bea73736ca3984087789c1e05a0c2d73ad3ff1ce67c39c4fdbd132c4ed7c8ad9808795bf230fa14)
	(L (- (expt 2 446) #x8335dc163bb124b65129c96fde933d8d723a70aadc873d6d54a7bb0d)))
    (make-eddsa-parameter
     'ed448
     p			       ;; p
     456		       ;; b
     2			       ;; c
     447		       ;; n
     -39081		       ;; d
     1			       ;; a
     (make-ed-point xb yb 1 0) ;; B (x y z t) note: t isn't used
     L			       ;; L
    )))

(define-vector-type eddsa-scheme
  (make-eddsa-scheme parameter prehash inithash) eddsa-scheme?
  (parameter eddsa-scheme-parameter)
  (prehash eddsa-scheme-prehash)
  (inithash eddsa-scheme-inithash))

(define ed25519-scheme
  (make-eddsa-scheme ed25519-parameter
    #f (lambda (data ctx hflag)
	 (when (or hflag (and ctx (> (bytevector-length ctx) 0)))
	   (assertion-violation 'ed25519 "Context/hashes not supported"))
	 (hash SHA-512 data))))

(define (ed25519ctx-inithash data ctx hflag)
  (let ((dom-prefix (if ctx
			(if (< (bytevector-length ctx) 256)
			    (bytevector-append
			     #*"SigEd25519 no Ed25519 collisions"
			     (if hflag #vu8(1) #vu8(0))
			     (make-bytevector 1 (bytevector-length ctx))
			     ctx)
			    (assertion-violation 'Ed25519
						 "Context too big"))
			#vu8())))
    (hash SHA-512 (bytevector-append dom-prefix data))))
  
(define ed25519ctx-scheme
  (make-eddsa-scheme ed25519-parameter #f ed25519ctx-inithash))

(define ed25519ph-scheme
  (make-eddsa-scheme ed25519-parameter
		     (lambda (x y) (hash SHA-512 x))
		     ed25519ctx-inithash))

(define (ed448-inithash data ctx hflag)
  (let ((dom-prefix (if ctx
			(if (< (bytevector-length ctx) 256)
			    (bytevector-append
			     #*"SigEd448"
			     (if hflag #vu8(1) #vu8(0))
			     (make-bytevector 1 (bytevector-length ctx))
			     ctx)
			    (assertion-violation 'Ed448
						 "Context too big"))
			#vu8())))
    (hash SHAKE256 (bytevector-append dom-prefix data) :size 114)))
(define ed448-scheme
  (make-eddsa-scheme ed448-parameter #f ed448-inithash))
(define ed448ph-scheme
  (make-eddsa-scheme ed448-parameter
		     (lambda (data ctx) (hash SHAKE256 data :size 64))
		     ed448-inithash))


(define (make-eddsa-signer scheme)
  (lambda (bv key :key (context #f))
    (define prehash (eddsa-scheme-prehash scheme))
    
    (define (sign msg key ctx scheme)
      (define prehash (eddsa-scheme-prehash scheme))
      (define inithash (eddsa-scheme-inithash scheme))
      (define parameter (eddsa-scheme-parameter scheme))
      (define l (eddsa-parameter-L parameter))
      (define c (eddsa-parameter-c parameter))
      (define n (eddsa-parameter-n parameter))
      (define b (eddsa-parameter-b parameter))
      (define B (eddsa-parameter-B parameter))
      (define pub-key
	(eddsa-public-key-data (eddsa-private-key-public-key key)))
      
      (let* ((khash (inithash (eddsa-private-key-random key) #f prehash))
	     (a (bytevector->integer/endian
		 (eddsa-clamp! (bytevector-copy khash 0 (div b 8)) c n b)
		 (endianness little)))
	     (seed (bytevector-copy khash (div b 8)))
	     (r (bytevector->integer/endian
		 (inithash (bytevector-append seed msg) ctx prehash)
		 (endianness little)))
	     (R (ed-point-encode-base parameter (ed-point-mul parameter B r) b))
	     (h (mod (bytevector->integer/endian
		      (inithash (bytevector-append R pub-key msg) ctx prehash)
		      (endianness little))
		     l))
	     (S (integer->bytevector/endian (mod (+ r (* h a)) l)
					    (endianness little)
					    (div b 8))))
	(bytevector-append R S)))
    (let ((ctx (or context #vu8())))
      (sign (or (and prehash (prehash bv ctx)) bv) key ctx scheme))))

(define (make-eddsa-verifier scheme)
  (lambda (msg sig key :key (context #f))
    (define prehash (eddsa-scheme-prehash scheme))
        
    (define (verify msg sig key ctx scheme)
      (define prehash (eddsa-scheme-prehash scheme))
      (define inithash (eddsa-scheme-inithash scheme))
      (define parameter (eddsa-scheme-parameter scheme))
      (define l (eddsa-parameter-L parameter))
      (define c (eddsa-parameter-c parameter))
      (define n (eddsa-parameter-n parameter))
      (define b (eddsa-parameter-b parameter))
      (define B (eddsa-parameter-B parameter))
      (define pub-key (eddsa-public-key-data key))

      (define b/8 (div b 8))
      ;; sanity check
      (unless (and (= (bytevector-length sig) (div b 4))
		   (= (bytevector-length pub-key) b/8))
	(error 'eddsa-verifier "inconsistent"))
      (let* ((r-raw (bytevector-copy sig 0 b/8))
	     (s-raw (bytevector-copy sig b/8))
	     (R (ed-point-decode-base parameter r-raw b))
	     (S (bytevector->integer/endian s-raw (endianness little)))
	     (A (ed-point-decode-base parameter pub-key b)))
	(unless (and R A (< S l))
	  (error 'eddsa-verifier "inconsistent"))
	(let* ((h (mod (bytevector->integer/endian
			(inithash (bytevector-append r-raw pub-key msg)
				  ctx prehash)
			(endianness little))
		       l))
	       (rhs (ed-point-add parameter R (ed-point-mul parameter A h)))
	       (lhs (ed-point-mul parameter B S)))
	  (do ((i 0 (+ i 1))
	       (lhs lhs (ed-point-double parameter lhs))
	       (rhs rhs (ed-point-double parameter rhs)))
	      ((= i c) (or (ed-point=? parameter lhs rhs)
			   (error 'eddsa-verifier "inconsistent")))))))
    (let ((ctx (or context #vu8())))
      (verify (or (and prehash (prehash msg ctx)) msg) sig key ctx scheme))))

;;; Twisted Edwards curve computation
;;; TODO should we make (math ed) and export them from there
(define-vector-type ed-point
  (make-ed-point x y z t) ed-point?
  (x ed-point-x)
  (y ed-point-y)
  (z ed-point-z)
  (t ed-point-t))

;; Based on RFC 8032 Appendix A
;; field calculation (modular arith)
(define (ed-field-add p x y) (mod-add x y p))
(define (ed-field-sub p x y) (mod-sub x y p))
(define (ed-field-mul p x y) (mod-mul x y p))
;;(define (ed-field-inv p x)   (mod (mod-expt x (- p 2) p) p))
(define (ed-field-inv p x)   (mod-inverse x p))
(define (ed-field-div p x y) (mod-div x y p))
  ;; (ed-field-mul p x (ed-field-inv p y)))
(define (ed-field-zero? p x) (zero? x))
(define (ed-field-sign p x)  (mod x 2))
(define (ed-field-negate p x) (mod-negate x p))
(define (ed-field-sqrt p x) (mod-sqrt x p))
	
(define (bytevector->ed-field p bv b)
  (let ((rv (mod (bytevector->integer/endian bv (endianness little))
		 (expt 2 (- b 1)))))
    (and (< rv p) rv)))

;; Ed point calculation
(define ed-point-zero
  (let ((zero (make-ed-point 0 1 1 0)))
    (lambda (parameter)
      zero)))

(define (ed-point-add parameter x y)
  (case (eddsa-parameter-name parameter)
    ((ed25519) (ed25519-point-add parameter x y))
    ((ed448) (ed448-point-add parameter x y))
    (else (assertion-violation 'ed-point-add "Not supported"
			       (eddsa-parameter-name parameter)))))
(define (ed25519-point-add parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define d (eddsa-parameter-d parameter))
  (define xx (ed-point-x x))
  (define xy (ed-point-y x))
  (define xt (ed-point-t x))
  (define xz (ed-point-z x))
  (define yx (ed-point-x y))
  (define yy (ed-point-y y))
  (define yt (ed-point-t y))
  (define yz (ed-point-z y))
  (let* ((zcp (ed-field-mul p xz yz))
	 (A (ed-field-mul p (ed-field-sub p xy xx) (ed-field-sub p yy yx)))
	 (B (ed-field-mul p (ed-field-add p xy xx) (ed-field-add p yy yx)))
	 (C (ed-field-mul p (ed-field-add p d d) (ed-field-mul p xt yt)))
	 (D (ed-field-add p zcp zcp))
	 (E (ed-field-sub p B A))
	 (H (ed-field-add p B A))
	 (F (ed-field-sub p D C))
	 (G (ed-field-add p D C)))
    (make-ed-point (ed-field-mul p E F) (ed-field-mul p G H)
		   (ed-field-mul p F G) (ed-field-mul p E H))))
(define (ed448-point-add parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define d (eddsa-parameter-d parameter))
  (define xx (ed-point-x x))
  (define xy (ed-point-y x))
  (define xz (ed-point-z x))
  (define yx (ed-point-x y))
  (define yy (ed-point-y y))
  (define yz (ed-point-z y))
  (let* ((xcp (ed-field-mul p xx yx))
	 (ycp (ed-field-mul p xy yy))
	 (zcp (ed-field-mul p xz yz))
	 (B (ed-field-mul p zcp zcp))
	 (E (ed-field-mul p d (ed-field-mul p xcp ycp)))
	 (F (ed-field-sub p B E))
	 (G (ed-field-add p B E)))
    (make-ed-point (ed-field-mul p
		    (ed-field-mul p zcp F)
		    (ed-field-sub p
		     (ed-field-mul p 
		      (ed-field-add p xx xy)
		      (ed-field-add p yx yy))
		     (ed-field-add p xcp ycp)))
		   (ed-field-mul p
		    (ed-field-mul p zcp G) (ed-field-sub p ycp xcp))
		   (ed-field-mul p F G)
		   0)))

(define (ed-point-double parameter p)
  (case (eddsa-parameter-name parameter)
    ((ed25519) (ed25519-point-double parameter p))
    ((ed448) (ed448-point-double parameter p))
    (else (assertion-violation 'ed-point-double "Not supported"
			       (eddsa-parameter-name parameter)))))
(define (ed25519-point-double parameter e)
  (define p (eddsa-parameter-p parameter))
  (define x (ed-point-x e))
  (define y (ed-point-y e))
  (define z (ed-point-z e))
  (let* ((A (ed-field-mul p x x))
	 (B (ed-field-mul p y y))
	 (Ch (ed-field-mul p z z))
	 (C (ed-field-add p Ch Ch))
	 (H (ed-field-add p A B))
	 (xys (ed-field-add p x y))
	 (E (ed-field-sub p H (ed-field-mul p xys xys)))
	 (G (ed-field-sub p A B))
	 (F (ed-field-add p C G)))
    (make-ed-point (ed-field-mul p E F) (ed-field-mul p G H)
		   (ed-field-mul p F G) (ed-field-mul p E H))))

(define (ed448-point-double parameter e)
  (define p (eddsa-parameter-p parameter))
  (define x (ed-point-x e))
  (define y (ed-point-y e))
  (define z (ed-point-z e))
  (let* ((x1s (ed-field-mul p x x))
	 (y1s (ed-field-mul p y y))
	 (z1s (ed-field-mul p z z))
	 (xys (ed-field-add p x y))
	 (F (ed-field-add p x1s y1s))
	 (J (ed-field-sub p F (ed-field-add p z1s z1s))))
    (make-ed-point (ed-field-mul p
		    (ed-field-sub p
		     (ed-field-sub p (ed-field-mul p xys xys) x1s) y1s) J)
		   (ed-field-mul p F (ed-field-sub p x1s y1s))
		   (ed-field-mul p F J)
		   0)))

;; scalar multiplication
(define (ed-point-mul parameter p k)
  (unless (integer? k)
    (assertion-violation 'ed-point-mul "integer required for k" k))
  (do ((r (ed-point-zero parameter)
	  (if (odd? k) (ed-point-add parameter r s) r))
       (s p (ed-point-double parameter s))
       (k k (bitwise-arithmetic-shift-right k 1)))
      ((<= k 0) r)))

(define (ed-point=? parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define xx (ed-point-x x))
  (define xy (ed-point-y x))
  (define xz (ed-point-z x))
  (define yx (ed-point-x y))
  (define yy (ed-point-y y))
  (define yz (ed-point-z y))
  (let ((xn1 (ed-field-mul p xx yz))
	(xn2 (ed-field-mul p yx xz))
	(yn1 (ed-field-mul p xy yz))
	(yn2 (ed-field-mul p yy xz)))
    (and (= xn1 xn2) (= yn1 yn2))))

(define (ed-point-solve-x2 parameter x y)
  (case (eddsa-parameter-name parameter)
    ((ed25519) (ed25519-point-solve-x2 parameter x y))
    ((ed448) (ed448-point-solve-x2 parameter x y))
    (else (assertion-violation 'ed-point-solve-x2 "Not supported"
			       (eddsa-parameter-name parameter)))))
(define (ed25519-point-solve-x2 parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define d (eddsa-parameter-d parameter))
  (ed-field-div p
		(ed-field-sub p (ed-field-mul p y y) 1)
		(ed-field-add p (ed-field-mul p (ed-field-mul p d y) y) 1)))
(define (ed448-point-solve-x2 parameter x y)
  (define p (eddsa-parameter-p parameter))
  (define d (eddsa-parameter-d parameter))
  (ed-field-div p
		(ed-field-sub p (ed-field-mul p y y) 1)
		(ed-field-sub p (ed-field-mul p (ed-field-mul p d y) y) 1)))
;; encode
(define (ed-point-encode-base parameter point b)
  (define p (eddsa-parameter-p parameter))
  (define x (ed-point-x point))
  (define y (ed-point-y point))
  (define z (ed-point-z point))
  (let ((xp (ed-field-div p x z))
	(yp (ed-field-div p y z)))
    (let ((s (integer->bytevector/endian yp (endianness little) (div b 8))))
      (when (odd? xp)
	(bytevector-u8-set! s (div (- b 1) 8)
	 (bitwise-ior
	  (bytevector-u8-ref s (div (- b 1) 8))
	  (bitwise-arithmetic-shift-left 1 (mod (- b 1) 8)))))
      s)))

;; decode
(define (ed-point-decode-base parameter s b)
  (define p (eddsa-parameter-p parameter))
  (and-let* (( (= (bytevector-length s) (div b 8)) )
	     (xs (bitwise-arithmetic-shift-right
		  (bytevector-u8-ref s (div (- b 1) 8))
		  (bitwise-and (- b 1) 7)))
	     (y (bytevector->ed-field p s b))
	     (x (ed-field-sqrt p (ed-point-solve-x2 parameter s y)))
	     ( (not (and (ed-field-zero? p x)
			 (not (= xs (ed-field-sign p x))))) )
	     (x (if (= (ed-field-sign p x) xs) x (ed-field-negate p x))))
    ;; NOTE: t is only used on Ed25519, Ed448 doesn't use it
    (make-ed-point x y 1 (ed-field-mul p x y))))
    
)
