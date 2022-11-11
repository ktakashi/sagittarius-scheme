;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/kdfs.scm - KDFs
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
(library (sagittarius crypto kdfs)
    (export pbkdf-1 pbkdf-2
	    mac->prf-provider)
    (import (rnrs)
	    (sagittarius crypto digests)
	    (sagittarius crypto mac)
	    (util bytevector))

(define (pbkdf-1 P S c dk-len :key (digest *digest:sha-1*))
  (define digest-len (digest-descriptor-digest-size digest))
  (define md (make-message-digest digest))
  (when (> dk-len digest-len)
    (assertion-violation 'pbkdf-1 "Derived key too long"))
  (let* ((buf (make-bytevector digest-len))
	 (dk (make-bytevector  dk-len)))
    (message-digest-init! md)
    (message-digest-process! md P)
    (message-digest-process! md S)
    (message-digest-done! md buf)
    (do ((i 0 (+ i 1)) (c (- c 1)))
	((= i c)
	 (bytevector-copy! buf 0 dk 0 dk-len)
	 dk)
      (digest-message! md buf buf))))

;; MAC = PRF :)
;; ref: Equivalence between MAC and PRF for Blockcipher based Constructions
;;      Nilanjan Datta and Mridul Nandi
;;      https://eprint.iacr.org/2013/575.pdf
(define (mac->prf-provider scheme . opts)
  (lambda (S) (apply make-mac scheme S opts)))

(define *hmac-sha1-prf* (mac->prf-provider *mac:hmac* :digest *digest:sha-1*))

(define (pbkdf-2 P S c dk-len :key (prf *hmac-sha1-prf*))
  (define (compute mac generate-mac! left block-no stored buf0 buf1 out)
    (bytevector-fill! buf0 0)
    (bytevector-fill! buf1 0)
    (bytevector-u32-set! buf1 0 block-no (endianness big))
    ;; block-no++
    (mac-init! mac)
    (mac-process! mac S)
    (mac-process! mac buf1 0 4)
    (let ((x (mac-done! mac buf0)))
      (bytevector-copy! buf0 0 buf1 0 x)
      (do ((i 1 (+ i 1)))
	  ((= i c)
	   (let ((l (min x left)))
	     (bytevector-copy! buf1 0 out stored l)
	     l))
	(generate-mac! buf0 buf0)
	(bytevector-xor! buf1 buf1 buf0))))
  (let* ((mac (prf P))
	 (hlen (mac-mac-size mac)))
    (unless (mac? mac)
      (assertion-violation 'pbkdf-2 "Invalid PRF" mac))
    (when (> dk-len (* #xffffffff hlen))
      (assertion-violation 'pbkdf-2 "Derived key too long"))
    (let ((buf0 (make-bytevector hlen))
	  (buf1 (make-bytevector hlen))
	  (out (make-bytevector dk-len))
	  (generate-mac! (make-mac-generator mac)))
      (let loop ((left dk-len) (block-no 1) (stored 0))
	(if (zero? left)
	    out
	    (let ((l (compute mac generate-mac!
			      left block-no stored buf0 buf1 out)))
	      (loop (- left l) (+ block-no 1) (+ stored l))))))))

;; HKDF: RFC 5869
)
