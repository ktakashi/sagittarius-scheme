;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; base32.scm - base32 encoding/decoding routine
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

;; ref: https://tools.ietf.org/html/rfc4648
#!nounbound
(library (rfc base32)
    (export base32-encode base32-encode-string
	    base32-decode base32-decode-string
	    open-base32-encode-output-port
	    open-base32-encode-input-port
	    open-base32-decode-output-port
	    open-base32-decode-input-port

	    make-base32-decoder make-base32-encoder
	    *base32-encode-table*
	    *base32-decode-table*

	    base32hex-encode base32hex-encode-string
	    base32hex-decode base32hex-decode-string
	    open-base32hex-encode-output-port
	    open-base32hex-encode-input-port
	    open-base32hex-decode-output-port
	    open-base32hex-decode-input-port

	    make-base32hex-decoder make-base32hex-encoder
	    *base32hex-encode-table*
	    *base32hex-decode-table*)
    (import (rnrs)
	    (rfc base-n)
	    (sagittarius))
;; minor thing
(define lshift bitwise-arithmetic-shift-left)
(define rshift bitwise-arithmetic-shift-right)
(define logior bitwise-ior)
(define logand bitwise-and)

(define *base32-encode-table*
  (vector-map
   char->integer
    ;; 0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
   #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
    ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
     #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\2 #\3 #\4 #\5 #\6 #\7
    ;; pad
     #\=)))

(define *base32-decode-table*
  (base-n-encode-table->decode-table *base32-encode-table*))

(define *base32hex-encode-table*
  (vector-map
   char->integer
    ;; 0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
   #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F
    ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
     #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
    ;; pad
     #\=)))

(define *base32hex-decode-table*
  (base-n-encode-table->decode-table *base32hex-encode-table*))

;; The same signature as Base64 version
;; Maybe we can make a framework for this
(define (encode put buffer buffer-size padding?)
  (define b0 (bytevector-u8-ref buffer 0))
  (define b1 (bytevector-u8-ref buffer 1))
  (define b2 (bytevector-u8-ref buffer 2))
  (define b3 (bytevector-u8-ref buffer 3))
  (define b4 (bytevector-u8-ref buffer 4))
  
  (define (mask v) (logand v #x1F))
  (define (pad n)
    (when padding?
      (do ((i 0 (+ i 1)))
	  ((= i n))
	(put 32))))
  (when (> buffer-size 0)
    ;; given arguments are 0 >=
    (put (mask (rshift b0 3)))
    (put (mask (logior (lshift b0 2) (rshift b1 6))))
    (cond
     ((= buffer-size 1) (pad 6))
     (else
      (put (logand (rshift b1 1) #x1F))
      (put (logand (logior (lshift b1 4) (rshift b2 4)) #x1F))
      (cond
       ((= buffer-size 2) (pad 4))
       (else
	(put (logand (logior (lshift b2 1) (rshift b3 7)) #x1F))
	(cond
	 ((= buffer-size 3) (pad 3))
	 (else
	  (put (logand (rshift b3 2) #x1F))
	  (put (logand (logior (lshift b3 3) (rshift b4 5)) #x1F))
	  (if (= buffer-size 4)
	      (pad 1)
	      (put (logand b4 #x1F)))))))))))
(define (decode oput buffer buffer-size)
  (define (put v) (oput (logand v #xFF)))
  (define (decode-impl b0 b1 b2 b3 b4 b5 b6 b7)
    (put (logior (lshift b0 3) (rshift b1 2)))
    (when b2
      (put (logior (lshift b1 6) (lshift b2 1) (rshift b3 4)))
      (when b4
	(put (logior (lshift b3 4) (rshift b4 1)))
	(when (and b5 b6)
	  (put (logior (lshift b4 7) (lshift b5 2) (rshift b6 3)))
	  (when b7
	    (put (logior (lshift b6 5) b7)))))))
  (case buffer-size
    ((0))				; easy
    ((2) (decode-impl (bytevector-u8-ref buffer 0)
		      (bytevector-u8-ref buffer 1)
		      #f #f #f #f #f #f))
    ((4) (decode-impl (bytevector-u8-ref buffer 0)
		      (bytevector-u8-ref buffer 1)
		      (bytevector-u8-ref buffer 2)
		      (bytevector-u8-ref buffer 3)
		      #f #f #f #f))
    ((5) (decode-impl (bytevector-u8-ref buffer 0)
		      (bytevector-u8-ref buffer 1)
		      (bytevector-u8-ref buffer 2)
		      (bytevector-u8-ref buffer 3)
		      (bytevector-u8-ref buffer 4)
		      #f #f #f))
    ((7) (decode-impl (bytevector-u8-ref buffer 0)
		      (bytevector-u8-ref buffer 1)
		      (bytevector-u8-ref buffer 2)
		      (bytevector-u8-ref buffer 3)
		      (bytevector-u8-ref buffer 4)
		      (bytevector-u8-ref buffer 5)
		      (bytevector-u8-ref buffer 6)
		      #f))
    ((8) (decode-impl (bytevector-u8-ref buffer 0)
		      (bytevector-u8-ref buffer 1)
		      (bytevector-u8-ref buffer 2)
		      (bytevector-u8-ref buffer 3)
		      (bytevector-u8-ref buffer 4)
		      (bytevector-u8-ref buffer 5)
		      (bytevector-u8-ref buffer 6)
		      (bytevector-u8-ref buffer 7)))
    (else (error 'base32-decode "Invalid Base32 encoding"))))

(define make-base32-encoder 
  (make-make-base-n-encoder encode 32 :encode-table *base32-encode-table*))
(define make-base32-decoder
  (make-make-base-n-decoder decode 32 :decode-table *base32-decode-table*))

(define base32-encode (make-base-n-encode make-base32-encoder))
(define base32-decode (make-base-n-decode make-base32-decoder))

(define base32-encode-string (make-base-n-encode-string base32-encode))
(define base32-decode-string (make-base-n-decode-string base32-decode))

(define open-base32-encode-output-port
  (make-base-n-encode-output-port-opener make-base32-encoder))
(define open-base32-encode-input-port
  (make-base-n-encode-input-port-opener make-base32-encoder))
(define open-base32-decode-output-port
  (make-base-n-decode-output-port-opener make-base32-decoder))
(define open-base32-decode-input-port
  (make-base-n-decode-input-port-opener make-base32-decoder))

(define make-base32hex-encoder 
  (make-make-base-n-encoder encode 32 :encode-table *base32hex-encode-table*))
(define make-base32hex-decoder
  (make-make-base-n-decoder decode 32 :decode-table *base32hex-decode-table*))

(define base32hex-encode (make-base-n-encode make-base32hex-encoder))
(define base32hex-decode (make-base-n-decode make-base32hex-decoder))

(define base32hex-encode-string (make-base-n-encode-string base32hex-encode))
(define base32hex-decode-string (make-base-n-decode-string base32hex-decode))

(define open-base32hex-encode-output-port
  (make-base-n-encode-output-port-opener make-base32hex-encoder))
(define open-base32hex-encode-input-port
  (make-base-n-encode-input-port-opener make-base32hex-encoder))
(define open-base32hex-decode-output-port
  (make-base-n-decode-output-port-opener make-base32hex-decoder))
(define open-base32hex-decode-input-port
  (make-base-n-decode-input-port-opener make-base32hex-decoder))

)

