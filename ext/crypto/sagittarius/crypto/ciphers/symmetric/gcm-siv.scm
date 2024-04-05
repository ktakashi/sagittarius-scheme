;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/ciphers/symmetric/gcm-siv.scm - GCM-SIV
;;;
;;;   Copyright (c) 2024  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius crypto ciphers symmetric gcm-siv)
    (export *mode:gcm-siv*)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius crypto descriptors)
	    (sagittarius crypto parameters)
	    (sagittarius crypto secure)
	    (only (sagittarius crypto tomcrypt)
		  gcm-multiply!)
	    (binary io)
	    (util bytevector))

;; Here we use GHASH to implement POLYVAL as libtomcrypt provids
;; GF(2^128) of module x^128 + x^7 + x^2 + x^1
;; According to RCF 8452, POLYVAL can be implemented like this:
;; POLYVAL(H, X_1, ..., X_n) = 
;;   ByteReverse(GHASH(mulX_GHASH(ByteReverse(H)), 
;;                     ByteReverse(X_1), ..., ByteReverse(X_n)))
;; So do it

(define *block-length* 16)
(define *nonce-length* 12)
(define-record-type gcm-siv-hash
  (fields buffer			; 16 bytes buffer
	  hash				; the hash (16 bytes)
	  (mutable count)		; buffer count
	  (mutable processed)		; processed count
	  )
  (protocol
   (lambda (p)
     (lambda (hash)
       (p (make-bytevector *block-length*)
	  hash
	  0 0)))))

(define-record-type gcm-siv-state
  (fields sink				;; plain data
	  nonce				;; nonce
	  ghash				;; ghash result
	  mac				;; tag
	  aead-hash			;; for tag
	  data-hash			;; for SIV
	  cipher			;; underlying cipher
	  H				;; mulX_GHASH(ByteReverse(H))
	  (mutable aad-completed?))
  (protocol
   (lambda (p)
     (define (derive-keys cipher nonce key)
       (define input (make-bytevector *block-length* 0))
       (define buffer (make-bytevector *block-length*))
       (define mode (mode-start *mode:ecb* cipher key #f))
       (define mac-key (make-bytevector *block-length* 0))
       (define enc-key (make-bytevector (bytevector-length key) 0))

       (define (enc n dst off)
	 (bytevector-u8-set! input 0 n)
	 (mode-encrypt! mode input 0 buffer 0 *block-length*)
	 (bytevector-copy! buffer 0 dst off 8))
       ;; setup nonce block
       (bytevector-copy! nonce 0
			 input (- *block-length* *nonce-length*)
			 *nonce-length*)
       (enc 0 mac-key 0)
       (enc 1 mac-key 8)
       (enc 2 enc-key 0)
       (enc 3 enc-key 8)
       (when (= (bytevector-length enc-key) 32)
	 (enc 3 enc-key 16)
	 (enc 4 enc-key 24))
       (values mac-key enc-key))
     (lambda (cipher nonce key)
       (let-values (((mac-key enc-key) (derive-keys cipher nonce key))
		    ((ghash) (make-bytevector *block-length*)))
	 (p (open-chunked-binary-input/output-port)
	    nonce
	    ghash
	    (make-bytevector *block-length*)
	    (make-gcm-siv-hash ghash)
	    (make-gcm-siv-hash ghash)
	    (mode-start *mode:ecb* cipher enc-key #f)
	    (mulx-ghash (bytevector-reverse! mac-key))
	    #f))))))

(define (gcm-siv-start cipher key parameter)
  (define iv (cipher-parameter-iv parameter))
  (define aad (cipher-parameter-aad parameter #f))
  (unless (memv (bytevector-length key) '(16 32))
    (assertion-violation 'gcm-siv "Invalid key size"))
  (let ((state (make-gcm-siv-state cipher iv key)))
    (when aad (gcm-siv-add-aad! state aad))
    state))

(define (gcm-siv-encrypt! state pt ps ct cs len)
  (define H (gcm-siv-state-H state))
  (check-status state len)
  (put-bytevector (gcm-siv-state-sink state) pt ps len)
  (update-hash! (gcm-siv-state-data-hash state) H pt ps len)
  0)
(define (gcm-siv-decrypt! state ct cs pt ps len)
  (check-status state len)
  (put-bytevector (gcm-siv-state-sink state) ct cs len)
  0)

(define (gcm-siv-done state . oopts) )
  
(define (gcm-siv-encrypt-last! state pt ps ct cs len)
  (define (increment-counter! counter)
    (let loop ((i 0))
      (unless (= i 4)
	(let ((v (+ (bytevector-s8-ref counter i) 1)))
	  (bytevector-s8-set! counter i v)
	  (when (zero? v) (loop (+ i 1)))))))
  (define (encrypt state data counter pt ps ct cs len)
    ;; now we encrypt
    (let ((mask (make-bytevector *block-length*))
	  (block (make-bytevector *block-length*))
	  (cipher (gcm-siv-state-cipher state)))
      (let ((v (bytevector-u8-ref counter (- *block-length* 1))))
	(bytevector-u8-set! counter (- *block-length* 1) (bitwise-ior v *mask*)))
      ;; reset position
      (set-port-position! data 0)
      (let loop ((c 0))
	(mode-encrypt! cipher counter 0 mask 0 *block-length*)
	(let ((n (get-bytevector-n! data block 0 *block-length*)))
	  (bytevector-xor! block block mask)
	  (bytevector-copy! block 0 ct (+ cs c) n)
	  (increment-counter! counter)
	  (if (= n *block-length*)
	      (loop (+ c n))
	      (+ c n))))))
  (gcm-siv-encrypt! state pt ps ct cs len)
  (let* ((data (gcm-siv-state-sink state))
	 (size (port-position data)))
    (when (< len size)
      (assertion-violation 'block-cipher-encrypt-last-block!
			   "Insufficient buffer"
			   `(required ,size)))
    (let ((tag (calculate-tag! state)))
      ;; copy tag
      (bytevector-copy! tag 0 (gcm-siv-state-mac state) 0 *block-length*)
      (if (zero? size)
	  size
	  (encrypt state data tag pt ps ct cs len)))))

(define (gcm-siv-decrypt-last! state ct cs pt ps len) )

(define (gcm-siv-compute-tag! state tag start)
  (define mac (gcm-siv-state-mac state))
  (bytevector-copy! mac 0 tag start *block-length*)
  *block-length*)
(define (gcm-siv-verify-tag! state tag start) )

(define (gcm-siv-add-aad! state aad
	  :optional (start 0) (len (- start (bytevector-length aad))))
  (check-aead-status state len)
  (update-hash! (gcm-siv-state-aead-hash state)
		(gcm-siv-state-H state)
		aad start len))

(define *mode:gcm-siv*
  (make-encauth-mode-descriptor
   #f					; no tomcrypt mode value
   "GCM-SIV"
   gcm-siv-start
   gcm-siv-encrypt! gcm-siv-encrypt-last!
   gcm-siv-decrypt! gcm-siv-decrypt-last!
   gcm-siv-done
   #f #f
   (lambda (_) *block-length*)
   gcm-siv-compute-tag!
   gcm-siv-verify-tag!
   gcm-siv-add-aad!
   #f
   ))

;; internal
(define *mask* #x80)
(define *~mask* (bitwise-not *mask*))
(define *add* #xE1)
(define (mulx-ghash v)
  (define (check-mask mask v)
    (unless (zero? mask)
      (bytevector-u8-set! v 0 (bitwise-xor (bytevector-u8-ref v 0) *add*)))
    v)
  (let loop ((i 0) (mask 0))
    (if (= i *block-length*)
	(check-mask mask v)
	(let* ((b (bytevector-u8-ref v i))
	       (b/2 (div b 2)))
	  (bytevector-u8-set! v i (bitwise-ior mask (bitwise-and b/2 *~mask*)))
	  (loop (+ i 1) (if (even? b) 0 *mask*))))))
(define (ghash H M r)
  (bytevector-xor! r r M)
  (gcm-multiply! H r r))

(define (calculate-tag! state)
  (complete-hash (gcm-siv-state-data-hash state) (gcm-siv-state-H state))
  (let ((polyval (complete-polyval state))
	(nonce (gcm-siv-state-nonce state)))
    (bytevector-xor! polyval polyval nonce)
    (let* ((i (- *block-length* 1))
	   (v (bytevector-u8-ref polyval i)))
      (bytevector-u8-set! polyval i (bitwise-and (- *mask* 1) v)))
    (let ((cipher (gcm-siv-state-cipher state))
	  (result (make-bytevector *block-length*)))
      (mode-encrypt! cipher polyval 0 result 0 *block-length*)
      result)))

(define (check-status state len)
  (unless (gcm-siv-state-aad-completed? state)
    ;; complete aad hash
    (complete-hash (gcm-siv-state-aead-hash state) (gcm-siv-state-H state))
    (gcm-siv-state-aad-completed?-set! state #t)))
(define (check-aead-status state len)
  (when (gcm-siv-state-aad-completed? state)
    (error 'block-cipher-update-aad!
	   "AEAD data cannot be processed after ordinary data")))

(define (update-hash! hash H data offset len)
  (define (process-buffered)
    (define count (gcm-siv-hash-count hash))
    (let ((space (- *block-length* count)))
      (if (> count 0)
	  (if (>= len space)
	      (let ((buffer (gcm-siv-hash-buffer hash)))
		(bytevector-copy! data offset buffer count space)
		(bytevector-reverse! buffer)
		(ghash H buffer (gcm-siv-hash-hash hash))
		(gcm-siv-hash-count-set! hash 0)
		(values (+ offset space) (- len space)))
	      (values offset len))
	  (values offset len))))
  (define (store-remaining remaining processed)
    (when (> remaining 0)
      (let ((buffer (gcm-siv-hash-buffer hash))
	    (count (gcm-siv-hash-count hash)))
	(bytevector-copy! data processed buffer count remaining)
	(gcm-siv-hash-count-set! hash (+ count remaining))))
    (let ((pcount (gcm-siv-hash-processed hash)))
      (gcm-siv-hash-processed-set! hash (+ pcount len))))
  (let-values (((processed remaining) (process-buffered))
	       ((buffer) (gcm-siv-hash-buffer hash))
	       ((result) (gcm-siv-hash-hash hash)))
    (let loop ((remaining remaining) (processed processed))
      (if (< remaining *block-length*)
	  (store-remaining remaining processed)
	  (begin
	    (bytevector-copy! data processed buffer 0 *block-length*)
	    (bytevector-reverse! buffer)
	    (ghash H buffer result)
	    (loop (- remaining *block-length*) (+ processed *block-length*)))))))

(define (complete-hash hash H)
  (define count (gcm-siv-hash-count hash))
  (when (> count 0)
    (let ((buffer (gcm-siv-hash-buffer hash)))
      (bytevector-fill! buffer 0 count *block-length*)
      (bytevector-reverse! buffer)
      (ghash H buffer (gcm-siv-hash-hash hash)))))

(define (complete-polyval state)
  (define (ghash-length! state)
    (define data-hash (gcm-siv-state-data-hash state))
    (define aead-hash (gcm-siv-state-aead-hash state))
    (define endian (endianness big))
    (let ((buf (make-bytevector *block-length*)))
      ;; We are using GHASH, so using big endian and reverse order of
      ;; data and AEAD length
      ;; Original:
      ;; length_block =
      ;;    little_endian_uint64(bytelen(additional_data) * 8) ++
      ;;    little_endian_uint64(bytelen(plaintext) * 8)
      ;; ==> AEAD len little + data len little
      ;;     e.g. (8 0 0 0 0 0 0 0 16 0 0 0 0 0 0 0)
      ;; Reverse it
      ;; (0 0 0 0 0 0 0 16 0 0 0 0 0 0 0 8)
      ;; ==> data len big + AEAD len big
      (bytevector-u64-set! buf 0 (* 8 (gcm-siv-hash-processed data-hash)) endian)
      (bytevector-u64-set! buf 8 (* 8 (gcm-siv-hash-processed aead-hash)) endian)
      (ghash (gcm-siv-state-H state) buf (gcm-siv-state-ghash state))))
  (let ((r (make-bytevector *block-length*)))
    ;; processing length_block
    (ghash-length! state)
    (bytevector-copy! (gcm-siv-state-ghash state) 0 r 0 *block-length*)
    (bytevector-reverse! r)
    r))

)
