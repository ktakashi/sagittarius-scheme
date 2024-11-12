;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/ciphers/asymmeteric/encodings.scm - PKCS#1 encodings
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (sagittarius crypto ciphers asymmetric encodings)
    (export oaep-encoding pkcs1-v1.5-encoding
	    mgf-1)
    (import (rnrs)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (sagittarius crypto descriptors cipher)
	    (sagittarius crypto secure)
	    (sagittarius crypto ciphers asymmetric state)
	    (util bytevector))

(define (mgf-1 mgf-seed mask-length digest)
  (when (> mask-length #x100000000) ;; 2^32
    (assertion-violation 'mgf-1 "Mask too long"))
  (let* ((hash-len (digest-descriptor-digest-size digest))
	 (md (make-message-digest digest))
	 (limit (+ 1 (div mask-length hash-len)))
	 (len   (bytevector-length mgf-seed))
	 (buf   (make-bytevector (+ len 4) 0))
	 (T     (make-bytevector mask-length 0)))
    (bytevector-copy! mgf-seed 0 buf 0 len)
    (do ((counter 0 (+ counter 1)))
	((= counter limit) T)
      (bytevector-u32-set! buf len counter (endianness big))
      (let ((index (* counter hash-len))
	    (bv (digest-message md buf)))
	(if (> (+ index hash-len) mask-length)
	    (bytevector-copy! bv 0 T index (- mask-length index))
	    (bytevector-copy! bv 0 T index hash-len))))))

;; Default is mgf1SHA1...
;; https://datatracker.ietf.org/doc/html/rfc8017#appendix-A.2.1
(define (oaep-encoding (descriptor asymmetric-cipher-descriptor?)
		       :key ((digest digest-descriptor?) *digest:sha-1*)
			    (mgf mgf-1)
			    ((mgf-digest digest-descriptor?) *digest:sha-1*)
			    ((label bytevector?) #vu8())
			    ((prng random-generator?) (secure-random-generator *prng:chacha20*))
		       :allow-other-keys)
  (define md (make-message-digest digest))
  (define hlen (digest-descriptor-digest-size digest))
  (define lhash (digest-message md label))
  ;; implementations
  (define (encode data state)
    (define k ((asymmetric-cipher-descriptor-block-size descriptor) state))
    (define ps-len (- k (bytevector-length data) (* hlen 2) 2))
    (define db-len (- k hlen 1))
    (when (> (bytevector-length data) (- k 2 (* hlen 2)))
      (assertion-violation 'oaep-encoding "Too much data for OAEP encoding"))
    (let* ((ps (make-bytevector ps-len 0))
	   (db (bytevector-append lhash ps #vu8(#x01) data))
	   (seed (random-generator-read-random-bytes prng hlen))
	   (db-mask (mgf seed db-len mgf-digest))
	   (masked-db (bytevector-xor db db-mask))
	   (seed-mask (mgf masked-db hlen mgf-digest))
	   (masked-seed (bytevector-xor seed seed-mask)))
      (bytevector-append #vu8(#x00) masked-seed masked-db)))

  (define (decode data state)
    (define k ((asymmetric-cipher-descriptor-block-size descriptor) state))
    (define db-len (- k hlen 1))
    (define (parse-em data hlen db-len)
      (define s (+ hlen 1))
      (values (bytevector-u8-ref data 0)
	      (bytevector-copy data 1 s)
	      (bytevector-copy data s (+ s db-len))))
    (define (parse-db db)
      (define (find-ps-end db)
	(do ((i hlen (+ i 1)))
	    ((= (bytevector-u8-ref db i) #x01) i)))
      (let ((ps-end (find-ps-end db)))
	(values (bytevector-copy db 0 hlen)
		(bytevector-copy db hlen ps-end)
		(bytevector-u8-ref db ps-end)
		(bytevector-copy db (+ ps-end 1)))))

    (when (> (bytevector-length data) k)
      (assertion-violation 'oaep-decoding "Too much data for OAEP decoding"))
    (let-values (((Y masked-seed masked-db) (parse-em data hlen db-len)))
      (let* ((seed-mask (mgf masked-db hlen mgf-digest))
	     (seed (bytevector-xor masked-seed seed-mask))
	     (db-mask (mgf seed db-len mgf-digest))
	     (db (bytevector-xor masked-db db-mask)))
	(let-values (((lhash-dash ps one M) (parse-db db)))
	  (unless (and (safe-bytevector=? lhash lhash-dash)
		       (zero? Y)
		       (= one #x01))
	    (error 'oaep-decoding "Invalid OAEP encoding"))
	  M))))
  (values encode decode))

(define (pkcs1-v1.5-encoding (descriptor asymmetric-cipher-descriptor?)
			     :key ((prng random-generator?) (secure-random-generator *prng:chacha20*))
			     :allow-other-keys)
  ;; Encode with
  ;;  - private key = signature  = EMSA-PKCS1-v1.5
  ;;  - public key  = encryption = RSAES-PKCS1-v1_5
  ;; Decode with
  ;;  - private key = encryption = RSAES-PKCS1-v1_5
  ;;  - public key  = signature  = EMSA-PKCS1-v1.5
  (define (encode data state)
    (define k ((asymmetric-cipher-descriptor-block-size descriptor) state))
    (define message-length (bytevector-length data))
    (when (> (+ message-length 11) k)
      (assertion-violation 'pkcs1-v1.5-encoding
			   "Too much data for PKCS#1 v1.5 encoding"))
    ;; 0x00 || 0x0(1|2) || PS || 0x00 || M
    (let* ((ps-length (- k message-length 3))
	   (bv (make-bytevector (+ 2 ps-length 1 message-length))))
      (cond ((asymmetric-state-private-key? state)
	     (bytevector-u8-set! bv 1 2) ;; PKCS1-v1.5-EME
	     (random-generator-read-random-bytes! prng bv 2 ps-length)
	     (do ((i 0 (+ i 1)))
		 ((= i ps-length) #t)
	       ;; transform zero bytes (if any) to non-zero random bytes
	       (when (zero? (bytevector-u8-ref bv (+ i 2)))
		 (do ((r (random-generator-read-random-bytes prng 1)
			 (random-generator-read-random-bytes prng 1)))
		     ((not (zero? (bytevector-u8-ref r 0)))
		      (bytevector-u8-set! bv (+ i 2)
					  (bytevector-u8-ref r 0)))))))
	    (else (bytevector-fill! bv #xFF)
		  (bytevector-u8-set! bv 1 1)))
      (bytevector-u8-set! bv 0 0)
      ;; set block-type
      (bytevector-u8-set! bv (+ 2 ps-length) 0) ;; mark end of the paddding
      (bytevector-copy! data 0 bv (+ 2 ps-length 1) message-length)
      bv))
  (define (decode data state)
    (define k ((asymmetric-cipher-descriptor-block-size descriptor) state))
    (define m-len (bytevector-length data))
    (define type (and (>= m-len 2) (bytevector-u8-ref data 1)))

    (define (search-end-padding data type)
      (define len (bytevector-length data))
      (let loop ((i 2) (valid? #t))
	(if (>= i len)
	    (values #f #f) ;; not found
	    (let ((v (bytevector-u8-ref data i)))
	      (cond ((zero? v) (values i valid?))
		    ((or (eqv? type 2) (and (eqv? type 1) (= v #xFF)))
		     (loop (+ i 1) valid?))
		    (else (loop (+ i 1) #f)))))))
    (let-values (((from valid?) (search-end-padding data type)))
      (when (or (not valid?)
		(< m-len 1)
		(not (zero? (bytevector-u8-ref data 0)))
		(not from) (>= from k) (< from 9))
	(error 'pkcs1-v1.5-decode "Invalid padding"))
      (let* ((len (- m-len from 1))
	     (bv (make-bytevector len 0)))
	(bytevector-copy! data (+ from 1) bv 0 len)
	bv)))
  (values encode decode))
)
