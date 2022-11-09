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
  (lambda (S)
    (let ((mac (apply make-mac scheme S opts)))
      (values (lambda (m out) (generate-mac! mac m out))
	      (mac-mac-size mac)))))
(define *hmac-sha1-prf* (mac->prf-provider *mac:hmac* :digest *digest:sha-1*))

(define (pbkdf-2 P S c dk-len :key (prf *hmac-sha1-prf*))
  (define (F prf h-len P S c i)
    (define (concat bv int)
      (let* ((len (bytevector-length bv))
	     (new (make-bytevector (+ len 4)))
	     (iv  (make-bytevector 4)))
	(bytevector-u32-set! iv 0 int (endianness big))
	(bytevector-copy! bv 0 new 0 len)
	(bytevector-copy! iv 0 new len 4)
	new))
    (let ((buf (make-bytevector h-len))
	  (out (make-bytevector h-len)))
      (do ((j 0 (+ j 1)))
	  ((= j c) out)
	(cond ((zero? j)
	       (prf (concat S i) out)
	       (bytevector-copy! out 0 buf 0 h-len))
	      (else
	       (prf buf buf)
	       (bytevector-xor! out out buf))))))

  (define (finish dk-len h-len ts)
    (let ((dk (make-bytevector dk-len)))
      (let loop ((stored 0) (i 0))
	(if (= stored dk-len)
	    dk
	    (let ((count (min (- dk-len stored) h-len)))
	      (bytevector-copy! (vector-ref ts i) 0 dk stored count)
	      (loop (+ count stored) (+ i 1)))))))
  (let-values (((f h-len) (prf P)))
    (when (> dk-len (* #xffffffff h-len))
      (assertion-violation 'pbkdf-2 "Derived key too long"))
    (let* ((l (ceiling (/ dk-len h-len)))
	   (ts (make-vector l)))
      (do ((i 0 (+ i 1)))
	  ((= i l) (finish dk-len h-len ts))
	(vector-set! ts i (F f h-len P S c (+ i 1)))))))

;; HKDF: RFC 5869
)
