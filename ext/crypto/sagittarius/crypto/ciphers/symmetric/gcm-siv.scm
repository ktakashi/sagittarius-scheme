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
	    (util bytevector))

(define *block-length* 16)
(define *nonce-length* 12)
(define-record-type gcm-siv-hash
  (fields buffer			; 16 bytes buffer
	  hash				; the hash (16 bytes)
	  count				; buffer count
	  ))

(define-record-type gcm-siv-state
  (fields sink				;; plain data
	  aead-hash			;; for tag
	  data-hash			;; for SIV
	  cipher			;; underlying cipher
	  H				;; mulX_GHASH(ByteReverse(H))
	  )
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
	 (mode-encrypt! mode input 0 *block-length* buffer 0 *block-length*)
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
       (let-values (((mac-key enc-key) (derive-keys cipher nonce key)))
	 (p (open-output-bytevector) ;; SRFI-6 thing
	    (make-gcm-siv-hash)
	    (make-gcm-siv-hash)
	    (mode-start *mode:ecb* cipher enc-key #f)
	    (mulx-ghash (bytevector-reverse! mac-key))))))))

(define (mulx-ghash v)
  (define MASK #x80)
  (define ~MASK (bitwise-not MASK))
  (define ADD #xE1)
  (define (check-mask mask v)
    (unless (zero? mask)
      (bytevector-u8-set! v 0 (bitwise-xor (bytevector-u8-ref v 0) ADD)))
    v)
  (let loop ((i 0) (mask 0))
    (if (= i *block-length*)
	(check-mask mask v)
	(let* ((b (bytevector-u8-ref v i))
	       (b/2 (div b 2)))
	  (bytevector-u8-set! v i (bitwise-ior mask (bitwise-and b/2 ~MASK)))
	  (loop (+ i 1) (if (even? b) 0 MASK))))))

(define (ghash H M r)
  (let ((t (bytevector-xor M r)))
    (gcm-multiply! H M r)))
(define (polyval H M r)
  (ghash H (bytevector-reverse M) r))

(define (gcm-siv-start cipher key parameter) )
(define (gcm-siv-encrypt! state pt ps ct cs len) 0)
(define (gcm-siv-decrypt! state ct cs pt ps len) 0)
(define (gcm-siv-done state . oopts) )
  
(define (gcm-siv-encrypt-last! state tag start) )
(define (gcm-siv-decrypt-last! state tag start) )
(define (gcm-siv-add-aad! state aad . opt) )
(define (gcm-siv-add-iv! state iv . opt) )
  
(define *mode:gcm-siv*
  (make-encauth-mode-descriptor
   "GCM-SIV"
   ;;
   #f					; no tomcrypt mode value
   gcm-siv-start
   gcm-siv-encrypt!
   gcm-siv-decrypt!
   gcm-siv-done
   #f #f
   (lambda (_) 16)
   gcm-siv-encrypt-last!
   gcm-siv-decrypt-last!
   gcm-siv-add-aad!
   gcm-siv-add-iv!
   ))
)
