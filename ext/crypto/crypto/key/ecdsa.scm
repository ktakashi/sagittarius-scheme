;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/key/ecdsa.scm - ECDSA cipher and so
;;;
;;;  Copyright (c) 2010-2017 Takashi Kato. All rights reserved.
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
;;  - https://tools.ietf.org/html/rfc5915
;;  - https://tools.ietf.org/html/rfc5480
;;  - https://tools.ietf.org/html/rfc6090
(library (crypto key ecdsa)
    (export ECDSA
	    <ecdsa-private-key>
	    <ecdsa-public-key>

	    ;; NIST parameters
	    NIST-P-192
	    NIST-P-224
	    NIST-P-256

	    ;; SEC 2 parameters
	    sect113r1
	    sect163k1
	    secp192r1 ;; the same as NIST-P-192
	    secp224r1 ;; the same as NIST-P-224
	    secp256r1 ;; the same as NIST-P-256
	    )
    (import (rnrs)
	    (sagittarius)
	    (math)
	    (math ec)
	    (clos user)
	    (sagittarius crypto)
	    (crypto key pair)
	    (asn.1))
  (define ECDSA :ecdsa)

  (define-class <ecdsa-private-key> (<private-key>)
    ((d :init-keyword :d)		   ;; private key
     (parameter :init-keyword :parameter :init-keyword #f) ;; domain parameter
     (public-key :init-keyword :public-key :init-value #f))) ;; public key
  (define-class <ecdsa-public-key> (<public-key>)
    ((Q :init-keyword :Q)))

  (define (read-random-bits prng nbits)
    (bytevector->uinteger (read-random-bytes prng (div nbits 8))))
  (define-method generate-key-pair ((m (eql ECDSA))
				    :key (ec-parameter secp256r1)
					 (prng (secure-random RC4)))
    (let* ((n (ec-parameter-n ec-parameter))
	   (nbits (bitwise-length n))
	   (G (ec-parameter-g ec-parameter))
	   (curve (ec-parameter-curve ec-parameter)))
      (do ((d (read-random-bits prng nbits) (read-random-bits prng nbits)))
	  ((and (> d 2) (< d n))
	   (let ((pub (make <ecdsa-public-key> :Q (ec-point-mul curve G d))))
	     (make-keypair (make <ecdsa-private-key> :d d
				 :parameter ec-parameter :public-key pub)
			   pub))))))

  (define-method generate-private-key ((m (eql ECDSA)) d
				       :optional (parameter secp256r1)
						 (publick-key #f))
    (make <ecdsa-private-key> :d d :parameter parameter
	  :public-key publick-key))

  (define-method generate-public-key ((m (eql ECDSA)) x y)
    (make <ecdsa-public-key> :Q (make-ec-point x y)))
  
  (define (random-k-generator prng)
    (lambda (n d)
      (let ((bits (bitwise-length n)))
	(do ((r (read-random-bits prng bits) (read-random-bits prng bits)))
	    ((and (not (zero? r)) (< r n)) r)))))

  ;; RFC 6979
  (define (determistic-k-generator digest message)
    (error 'determistic-k-generator "not supported yet"))
  
  (define (ecdsa-sign bv key
		      :key (k-generator
			    (random-k-generator (secure-random RC4)))
			   (der-encode #t)
			   (digest :hash SHA-1))
    (define (compute-e n bv)
      (define M (hash digest bv))
      (let ((len (bitwise-length n))
	    (M-bits (* (bytevector-length M) 8)))
	(let ((e (bytevector->uinteger M)))
	  (if (< len M-bits)
	      (bitwise-arithmetic-shift-right e (- M-bits len))
	      e))))
    (define (compute-r ec n d)
      (define G (ec-parameter-g ec))
      (define curve (ec-parameter-curve ec))
      (let loop ()
	(let* ((k (k-generator n d))
	       (p (ec-point-mul curve G k))
	       (r (mod (ec-point-x p) n)))
	  (if (zero? r)
	      (loop)
	      (values r k)))))
    (define (compute-s r k e d n) (mod (* (mod-inverse k n) (+ e (* d r))) n))
    (let* ((ec-param (slot-ref key 'parameter))
	   (n (ec-parameter-n ec-param))
	   (e (compute-e n bv))
	   (d (slot-ref key 'd)))
      (let loop ()
	(let-values (((r k) (compute-r ec-param n d)))
	  (let ((s (compute-s r k e d n)))
	    (cond ((zero? s) (loop))
		  (der-encode (encode (make-der-sequence
				       (make-der-integer r)
				       (make-der-integer s))))
		  (else (bytevector-append (integer->bytevector r)
					   (integer->bytevector s)))))))))
  
  (define (ecdsa-verify . ignroe) (error 'verify "not supported"))

  
  (define-class <ecdsa-cipher-spi> (<cipher-spi>) ())
  (define-method initialize ((o <ecdsa-cipher-spi>) initargs)
    (let ((key (car initargs)))
      (slot-set! o 'name 'ECDSA)
      (slot-set! o 'key key)
      (slot-set! o 'encrypt 
		 (lambda ignore (error 'encrypt "not supported in DSA")))
      (slot-set! o 'decrypt
		 (lambda ignore (error 'decrypt "not supported in DSA")))
      (slot-set! o 'padder #f)
      (slot-set! o 'signer ecdsa-sign)
      (slot-set! o 'verifier ecdsa-verify)
      (slot-set! o 'keysize #f)))
  (register-spi ECDSA <ecdsa-cipher-spi>)
  
  )
