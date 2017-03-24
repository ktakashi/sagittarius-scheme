;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/wrap.scm - RFC 3394 AES Key Wrap Algorithm
;;;
;;;  Copyright (c) 2017 Takashi Kato. All rights reserved.
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

;; ref
;;  https://tools.ietf.org/html/rfc3394
;;  https://tools.ietf.org/html/rfc3657 (Camellia)

(library (crypto wrap)
    (export make-rfc3394-key-wrap
	    make-rfc3394-key-unwrap
	    ;; actual RFC3394
	    make-aes-key-wrap
	    make-aes-key-unwrap
	    ;; 
	    make-camellia-key-wrap
	    make-camellia-key-unwrap

	    ;; condition
	    integrity-error? &integrity-error
	    )
    (import (rnrs)
	    (crypto cipher)
	    (sagittarius crypto))

  (define-condition-type &integrity-error &crypto-error
    make-integrity-error integrity-error?)
    
  (define +default-iv+ #vu8(#xa6 #xa6 #xa6 #xa6 #xa6 #xa6 #xa6 #xa6))
  (define +iv-length+ 8)
  
  (define (make-rfc3394-key-wrap cipher-name key :key (iv +default-iv+))
    ;; Because the cipher is ECB, it's thread safe
    (define cipher (make-cipher cipher-name key))
    (unless (= (bytevector-length iv) +iv-length+)
      (assertion-violation 'make-rfc3394-key-wrap "iv length must be 8"))
    ;; 2.2.1 Key Wrap
    (lambda (pt)
      (define pt-len (bytevector-length pt))
      (define n (div pt-len 8))
      (unless (zero? (mod pt-len 8))
	(assertion-violation 'rfc3394-key-wrap
			     "wrapping data length must be multiple of 8"))
      (let ((block (make-bytevector (+ pt-len +iv-length+)))
	    (buf   (make-bytevector (+ 8 +iv-length+))))
	(bytevector-copy! iv 0 block 0 +iv-length+)
	(bytevector-copy! pt 0 block +iv-length+ pt-len)
	(do ((i 0 (+ i 1)))
	    ((= i 6) block)
	  (do ((j 1 (+ j 1)))
	      ((> j n))
	    (bytevector-copy! block 0 buf 0 +iv-length+)
	    (bytevector-copy! block (* j 8) buf +iv-length+ 8)
	    ;; want cipher-encrypt! or so...
	    (let ((r (cipher-encrypt cipher buf)))
	      (bytevector-copy! r 0 buf 0 (+ 8 +iv-length+)))
	    (do ((k 1 (+ k 1))
		 (t (+ (* n i) j) (bitwise-arithmetic-shift t -8)))
		((zero? t))
	      (let* ((v (bitwise-and t #xFF))
		     (p (- +iv-length+ k))
		     (b (bytevector-u8-ref buf p)))
		(bytevector-u8-set! buf p (bitwise-xor b v))))
	    (bytevector-copy! buf 0 block 0 8)
	    (bytevector-copy! buf 8 block (* 8 j) 8))))))

  (define (make-rfc3394-key-unwrap cipher-name key :key (iv +default-iv+))
    ;; Because the cipher is ECB, it's thread safe
    (define cipher (make-cipher cipher-name key))
    (unless (= (bytevector-length iv) +iv-length+)
      (assertion-violation 'make-rfc3394-key-wrap "iv length must be 8"))
    (lambda (ct)
      (define ct-len (bytevector-length ct))
      (define n (div ct-len 8))
      (define (check a iv)
	(unless (bytevector=? a iv)
	  (raise (condition (make-integrity-error)
			    (make-who-condition 'rfc3394-key-unwrap)
			    (make-message-condition "Invalid cipher text")))))
      (unless (zero? (mod ct-len 8))
	(assertion-violation 'rfc3394-key-unwrap
			     "unwrapping data length must be multiple of 8"))
      (let ((block (make-bytevector (- ct-len +iv-length+)))
	    (a     (make-bytevector +iv-length+))
	    (buf   (make-bytevector (+ 8 +iv-length+)))
	    (n     (- n 1)))
	(bytevector-copy! ct 0 a 0 +iv-length+)
	(bytevector-copy! ct +iv-length+ block 0 (- ct-len +iv-length+))
	(do ((i 5 (- i 1)))
	    ((< i 0) (check a iv) block)
	  (do ((j n (- j 1)))
	      ((= j 0))
	    (bytevector-copy! a 0 buf 0 +iv-length+)
	    (bytevector-copy! block (* (- j 1) 8) buf +iv-length+ 8)
	    (do ((k 1 (+ k 1))
		 (t (+ (* n i) j) (bitwise-arithmetic-shift t -8)))
		((zero? t))
	      (let* ((v (bitwise-and t #xFF))
		     (p (- +iv-length+ k))
		     (b (bytevector-u8-ref buf p)))
		(bytevector-u8-set! buf p (bitwise-xor b v))))
	    ;; want cipher-decrypt! or so...
	    (let ((r (cipher-decrypt cipher buf)))
	      (bytevector-copy! r 0 buf 0 (+ 8 +iv-length+)))
	    (bytevector-copy! buf 0 a 0 8)
	    (bytevector-copy! buf 8 block (* 8 (- j 1)) 8))))))

  (define (make-aes-key-wrap key . rest)
    (apply make-rfc3394-key-wrap AES key rest))
  (define (make-aes-key-unwrap key . rest)
    (apply make-rfc3394-key-unwrap AES key rest))
  (define (make-camellia-key-wrap key . rest)
    (apply make-rfc3394-key-wrap Camellia key rest))
  (define (make-camellia-key-unwrap key . rest)
    (apply make-rfc3394-key-unwrap Camellia key rest))
)
