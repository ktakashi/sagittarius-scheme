;;; -*- Scheme -*-
;;;
;;; pkcs 12 cipher.scm - PKCS#12 library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; we do not support MAC yet
(library (rsa pkcs :12 cipher)
    (export PKCS12
	    pbe-with-sha-and3-keytripledes-cbc
	    pbe-with-sha-and-40bit-rc2-cbc
	    ;; for convenience
	    make-pbe-parameter generate-secret-key
	    <pbe-cipher-spi> <pbe-secret-key>
	    <pbe-parameter>)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (clos user)
	    (crypto)
	    (math)
	    (rsa pkcs :5))
  ;; generate-secret-keys
  ;; PBEWithSHAAnd3-KeyTripleDES-CBC
  ;; PBEWithSHAAnd2-KeyTripleDES-CBC ;; <- what is this?

  ;; type markder
  (define-class <pkcs12> () ())
  (define-class <pbe-sha1-des3-3> () ())
  (define-class <pbe-sha1-rc2-40-3> () ())
  (define PKCS12 (make <pkcs12>))
  (define pbe-with-sha-and3-keytripledes-cbc (make <pbe-sha1-des3-3>))
  (define pbe-with-sha-and-40bit-rc2-cbc (make <pbe-sha1-rc2-40-3>))

  (define-method generate-secret-key ((marker <pbe-sha1-des3-3>)
				      (password <string>))
    (make <pbe-secret-key> :password  password :hash (hash-algorithm SHA-1)
	  :scheme DES3 :iv-size 8 :length 24
	  :type PKCS12))

  (define-method generate-secret-key ((marker <pbe-sha1-rc2-40-3>)
				      (password <string>))
    (make <pbe-secret-key> :password  password :hash (hash-algorithm SHA-1)
	  :scheme RC2 :iv-size 8 :length 5
	  :type PKCS12))

  ;; PKCS#5 and PKCS#12 have defferent key derivation and iv generation
  ;; So we need to define these method

  (define-constant *key-material* 1)
  (define-constant *iv-material*  2)
  (define-constant *mac-material* 3)

  (define-method derive-key&iv ((marker <pkcs12>)
				(key <pbe-secret-key>)
				(param <pbe-parameter>))
    ;; parameters
    (define u (hash-size (slot-ref key 'hash)))
    (define v (hash-block-size (slot-ref key 'hash)))
    (define salt (slot-ref param 'salt))
    (define iteration (slot-ref param 'iteration))
    (define password (string->utf16 (string-append (slot-ref key 'password)
						   "\x0;")
				    'big))
    (define hash (slot-ref key 'hash))
    ;; key derivation
    (define (adjust a a-off b)
      (let* ((b-len (bytevector-length b))
	     (x (+ (bitwise-and (bytevector-u8-ref b (- b-len 1))
				#xFF)
		   (bitwise-and (bytevector-u8-ref a (+ a-off (- b-len 1)))
				#xFF)
		   1)))
	(bytevector-u8-set! a (+ a-off (- b-len 1)) (bitwise-and x #xFF))
	(set! x (bitwise-arithmetic-shift-right x 8))
	(do ((i (- b-len 2) (- i 1)))
	    ((< i 0))
	  (set! x (+ x (bitwise-and (bytevector-u8-ref b i) #xFF)
		     (bitwise-and (bytevector-u8-ref a (+ a-off i)) #xFF)))
	  (bytevector-u8-set! a (+ a-off i) (bitwise-and x #xFF))
	  (set! x (bitwise-arithmetic-shift-right x 8)))))
    (define (generate-derived-key in-byte n)
      (define (gen-SP bv)
	(case (bytevector-length bv)
	  ((0) #vu8())
	  (else 
	   => (lambda (bv-len)
		(let ((len (* v (div (- (+ bv-len v) 1) v))))
		  (do ((i 0 (+ i 1)) (r (make-bytevector len)))
		      ((= i len) r)
		    (bytevector-u8-set! 
		     r i
		     (bytevector-u8-ref bv (mod i bv-len)))))))))
      (let* ((D     (make-bytevector v in-byte))
	     (d-key (make-bytevector n 0))
	     (S     (gen-SP salt))
	     (P     (gen-SP password))
	     (I     (make-bytevector (+ (bytevector-length S)
					(bytevector-length P))))
	     (B     (make-bytevector v))
	     (c     (div (- (+ n u) 1) u)))
	(bytevector-copy! S 0 I 0 (bytevector-length S))
	(bytevector-copy! P 0 I (bytevector-length S) (bytevector-length P))
	(dotimes (i c)
	  (let ((A (make-bytevector u)))
	    (hash-init! hash)
	    (hash-process! hash D)
	    (hash-process! hash I)
	    (hash-done! hash A)
	    (dotimes (j (- iteration 1))
	      (hash-init! hash)
	      (hash-process! hash A)
	      (hash-done! hash A))
	    (dotimes (j (bytevector-length B))
	      (bytevector-u8-set! B j (bytevector-u8-ref A (mod j u))))
	    (dotimes (j (div (bytevector-length I) v))
	      (adjust I (* j v) B))
	    (if (= i (- c 1))
		(bytevector-copy! A 0 d-key (* i u) (- n (* i u)))
		(bytevector-copy! A 0 d-key (* i u) u))))
	d-key))
    (cons (generate-derived-key *key-material* (slot-ref key 'length))
	  (generate-derived-key *iv-material* (slot-ref key 'iv-size))))

  (register-spi pbe-with-sha-and3-keytripledes-cbc <pbe-cipher-spi>)
  (register-spi pbe-with-sha-and-40bit-rc2-cbc <pbe-cipher-spi>)

  )