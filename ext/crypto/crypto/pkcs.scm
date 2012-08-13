;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; pkcs.scm Cryptographic library
;;; 
#!compatible
(library (crypto pkcs)
    (export ;; padder
	    pkcs5-padder
	    ;; encoder
	    pkcs1-emsa-pss-encode
	    pkcs1-emsa-pss-verify
	    pkcs1-emsa-v1.5-encode
	    pkcs1-emsa-v1.5-verify
	    mgf-1)
    (import (rnrs)
	    (rnrs r5rs)
	    (asn.1)
	    (util bytevector)
	    (math hash)
	    (math helper)
	    (math random)
	    (sagittarius)
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

  ;; PKCS #1 EMSA-PSS-ENCODE
  ;; reference http://www.rsa.com/rsalabs/node.asp?id=2125
  (define 2^32 #x100000000)
  (define (mgf-1 mgf-seed mask-length hasher)
    (when (> mask-length 2^32)
      (raise-encode-error 'mgf-1 "mask too long"))
    (let* ((hash-len (hash-size hasher))
	   (limit (+ 1 (div mask-length hash-len)))
	   (len   (bytevector-length mgf-seed))
	   (buf   (make-bytevector (+ len 4) 0))
	   (T     (make-bytevector mask-length 0)))
      (bytevector-copy! mgf-seed 0 buf 0 len)
      (do ((counter 0 (+ counter 1)))
	  ((= counter limit) T)
	(bytevector-u32-set! buf len counter 'big)
	(let ((index (* counter hash-len)))
	  (if (> (+ index hash-len) mask-length)
	      (bytevector-copy! (hash hasher buf) 0 T index
				(- mask-length index))
	      (bytevector-copy! (hash hasher buf) 0 T index hash-len))))))

  ;; section 9.1.1
  (define (pkcs1-emsa-pss-encode m em-bits
				 :key (algo :hash (hash-algorithm SHA-1))
				      (mgf mgf-1)
				      (salt-length #f)
				      (prng (secure-random RC4)))
    (unless salt-length (set! salt-length (hash-size algo)))
    (let ((hash-len (hash-size algo))
	  (em-len (align-size (bit em-bits))))
      ;; how can we get max hash input length?
      ;;(when (> (bytevector-length m) hash-len)
      ;; (raise-decode-error 'pkcs1-emsa-pss-encode
      ;; "message too long"))
      (when (< em-len (+ hash-len salt-length 2))
	(raise-encode-error 'pkcs1-emsa-pss-encode "encode error"))
      (let* ((m-hash (hash algo m))
	     (salt (read-random-bytes prng salt-length))
	     (m-dash (make-bytevector (+ 8 hash-len salt-length) 0)))
	;; M' = (0x)00 00 00 00 00 00 00 00 || mHash || salt
	(bytevector-copy! m-hash 0 m-dash 8 hash-len)
	(bytevector-copy! salt 0 m-dash (+ 8 hash-len) salt-length)
	(let* ((H (hash algo m-dash))
	       (PS-len (- em-len salt-length hash-len 2))
	       (PS (make-bytevector PS-len 0))
	       (DB (make-bytevector (+ PS-len salt-length 1) #0x1)))
	  (bytevector-copy! PS 0 DB 0 PS-len)
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
		   (EM (make-bytevector (+ m-len h-len 1) #xBC)))
	      (bytevector-copy! masked-db 0 EM 0 m-len)
	      (bytevector-copy! H 0 EM m-len h-len)
	      EM))))))

  ;; section 9.1.2
  (define (pkcs1-emsa-pss-verify m em em-bits
				 :key (algo :hash (hash-algorithm SHA-1))
				      (mgf mgf-1)
				      (salt-length #f))
    (unless salt-length (set! salt-length (hash-size algo)))
    (let ((hash-len (hash-size algo))
	  (em-len   (align-size (bit em-bits))))
      (when (or (< em-len (+ hash-len salt-length 2))
		(not (= #xbc 
			(bytevector-u8-ref em (- (bytevector-length em) 1)))))
	(raise-decode-error 'pkcs1-emsa-pss-verify "inconsistent" 1))
      (let* ((m-hash (hash algo m))	; step 2
	     (mask-len (- em-len hash-len 1))
	     (masked-db (make-bytevector mask-len 0))
	     (H (make-bytevector hash-len 0))
	     (limit (- (* 8 em-len) em-bits)))
	;; step 5
	(bytevector-copy! em 0 masked-db 0 mask-len)
	(bytevector-copy! em mask-len H 0 hash-len)
	;; step 6
	;; check 8emLen - emBits of maskedDB
	(do ((i 0 (+ i 1)))
	    ((= i limit) #t)
	  (unless (zero? (bytevector-u8-ref masked-db i))
	    (raise-decode-error 'pkcs1-emsa-pss-verify
			    "inconsistent" 2)))
	(let* ((db-mask (mgf H mask-len algo))
	       (DB (bytevector-xor masked-db db-mask))
	       (limit2 (- em-len hash-len salt-length 2)))
	  (do ((i 0 (+ i 1)))
	      ((= i limit) #t)
	    (bytevector-u8-set! DB i 0))
	  ;; check emLen - hLen - sLen - 2 leftmost octers
	  (do ((i 0 (+ i 1)))
	      ((= i limit2) #t)
	    (unless (zero? (bytevector-u8-ref DB i))
	      (raise-decode-error 'pkcs1-emsa-pss-verify "inconsistent" 3)))
	  ;; check if position emLen - hLen - sLen - 1 have 0x01
	  (unless (= #x01 (bytevector-u8-ref DB limit2))
	    (raise-decode-error 'pkcs1-emsa-pss-verify "inconsistent" 4))
	  (let ((salt (make-bytevector salt-length 0)))
	    (bytevector-copy! DB (- (bytevector-length DB) salt-length)
			      salt 0 salt-length)
	    (let ((m-dash (make-bytevector (+ 8 hash-len salt-length) 0)))
	      (bytevector-copy! m-hash 0 m-dash 8 hash-len)
	      (bytevector-copy! salt 0 m-dash (+ 8 hash-len) salt-length)
	      (let ((h-dash (hash algo m-dash)))
		(if (bytevector=? H h-dash)
		    #t
		    (raise-decode-error 'pkcs1-emsa-pss-verify
					"inconsistent" 5)))))))))

  ;; section 9.2
  (define (pkcs1-emsa-v1.5-encode m em-bits
				  :key (algo :hash (hash-algorithm SHA-1)))
    (let ((em-len (align-size (bit em-bits)))
	  (h      (hash algo m))
	  (oid    (hash-oid algo)))
      (unless oid
	(assertion-violation 'pkcs1-emsa-v1.5-encode
			     "given hash algorithm does not have OID" algo))
      ;; compute digest info
      (let* ((digest (make-der-sequence (make-der-sequence
					 (make-der-object-identifier oid)
					 (make-der-null))
					(make-der-octet-string h)))
	     (T (encode digest))
	     (t-len (bytevector-length T)))
	(when (< em-len (+ t-len 11))
	  (raise-encode-error 'pkcs1-emsa-v1.5-encode
			      "intended encoded message length too short"
			      em-len))
	(let* ((PS-len (- em-len t-len 3))
	       ;; Initialize with PS value
	       (EM (make-bytevector (+ PS-len 3 t-len) #xFF)))
	  (bytevector-u8-set! EM 0 #x00)
	  (bytevector-u8-set! EM 1 #x01)
	  (bytevector-u8-set! EM (+ PS-len 2) #x00)
	  (bytevector-copy! T 0 EM (+ PS-len 3) t-len) 
	  EM)))
    )

  (define (pkcs1-emsa-v1.5-verify m em em-bits
				  :key (algo :hash (hash-algorithm SHA-1)))
    ;; verify is the same as encode
    (let1 EM (pkcs1-emsa-v1.5-encode m em-bits :hash algo)
      ;; to remove 0, we need to convert bv to integer and integer to bv. sucks!
      (if (= (bytevector->integer em) (bytevector->integer EM))
	  #t
	  (raise-decode-error 'pkcs1-emsa-v1.5-verify
			      "inconsistent"))
      ))

)
