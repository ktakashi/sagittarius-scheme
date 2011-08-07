;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; pkcs.scm Cryptographic library
;;; 
(library (crypto pkcs)
    (export ;; padder
	    pkcs5-padder
	    ;; encoder
	    pkcs1-emsa-pss-encode
	    mgf-1)
    (import (rnrs)
	    (rnrs r5rs)
	    (math hash)
	    (math helper)
	    (math random)
	    (sagittarius control)
	    (sagittarius crypto))

  ;; PKCS #5 padding.
  ;; reference http://www.rsa.com/rsalabs/node.asp?id=2127
  (define (pkcs5-padder bv block-size pad?)
    (if pad?
	(let* ((len (bytevector-length bv))
	       (mod (modulo len block-size))
	       (padding (- block-size mod)))
	  (when (zero? padding)
	    (set! padding 8))
	  (let ((new (make-bytevector (+ len padding) 0)))
	    ;; lazyness
	    (bytevector-fill! new padding)
	    (bytevector-copy! bv 0 new 0 len)
	    new))
	(let* ((len (bytevector-length bv))
	       (pad (bytevector-u8-ref bv (- len 1)))
	       (new (make-bytevector (- len pad) 0)))
	  (bytevector-copy! bv 0 new 0 (- len pad))
	  new)))

  (define (bytevector-xor a b)
    (let* ((len (bytevector-length a))
	   (ret (bytevector-copy a)))
      (do ((i 0 (+ i 1)))
	  ((= i len) ret)
	(bytevector-u8-set! ret i
			    (bitwise-xor (bytevector-u8-ref a i)
					 (bytevector-u8-ref b i)))))
    )

  ;; PKCS #1 EMSA-PSS-ENCODE
  ;; reference http://www.rsa.com/rsalabs/node.asp?id=2125
  (define 2^32 #x100000000)
  (define (mgf-1 mgf-seed mask-length hasher)
    (when (> mask-length 2^32)
      (raise-encode-error 'mgf-1 "mask too long"))
    (let* ((limit (- (div mask-length (hash-size hasher)) 1))
	   (len   (bytevector-length mgf-seed))
	   (buf   (make-bytevector (+ len 4) 0))
	   (T     (make-bytevector mask-length 0)))
      (bytevector-copy! mgf-seed 0 buf 0 len)
      (do ((counter 0 (+ counter 1)))
	  ((>= counter limit) T)
	(bytevector-u32-set! buf len counter 'big)
	(bytevector-copy! (hash hasher buf) 0 T (* counter len) len))))

  (define-with-key (pkcs1-emsa-pss-encode m em-bits
					  :key (algo :hash (hash-algorithm SHA-1))
					       (mgf mgf-1)
					       (salt-length #f)
					       (prng (pseudo-random RC4)))
    (unless salt-length
      (set! salt-length (hash-size algo)))
    (let ((hash-len (hash-size algo))
	  (em-len (align-size (bit em-bits))))
      ;; how can we get max hash input length?
      ;;(when (> (bytevector-length m) hash-len)
      ;; (raise-decode-error 'pkcs1-emsa-pss-encode
      ;; "message too long"))
      (when (< em-len (+ hash-len salt-length 2))
	(raise-encode-error 'pkcs1-emsa-pss-encode
			    "encode error"))
      (let* ((m-hash (hash algo m))
	     (salt (read-random-bytes prng salt-length))
	     (m-dash (make-bytevector (+ 8 hash-len salt-length) 0)))
	;; M' = (0x)00 00 00 00 00 00 00 00 || mHash || salt
	(bytevector-copy! m-hash 0 m-dash 8 hash-len)
	(bytevector-copy! salt 0 m-dash (+ 8 hash-len) salt-length)
	(let* ((H (hash algo m-dash))
	       (PS-len (- em-len salt-length hash-len 2))
	       (PS (make-bytevector PS-len 0))
	       (DB (make-bytevector (+ PS-len salt-length 1))))
	  (bytevector-copy! PS 0 DB 0 PS-len)
	  (bytevector-u8-set! DB PS-len #x01)
	  (bytevector-copy! salt 0 DB (+ PS-len 1) salt-length)
	  (let* ((db-mask (mgf H (- em-len hash-len 1) algo))
		 ;; xor
		 (masked-db (bytevector-xor DB db-mask))
		 (limit (- (* em-len 8) em-bits)))
	    (do ((i 0 (+ i 1)))
		((= i limit) #t)
	      (bytevector-u8-set! masked-db i 0))
	    (let* ((m-len (bytevector-length masked-db))
		   (h-len (bytevector-length H))
		   (EM (make-bytevector (+ m-len h-len 1))))
	      (bytevector-copy! masked-db 0 EM 0 m-len)
	      (bytevector-copy! H 0 EM m-len h-len)
	      (bytevector-u8-set! EM (+ m-len h-len) #xBC)
	      EM))))))
)
