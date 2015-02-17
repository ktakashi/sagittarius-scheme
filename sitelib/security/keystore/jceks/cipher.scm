;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/keystore/jceks/cipher.scm - JCEKS cipher
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (security keystore jceks cipher)
    (export pbe-with-md5-and-des3)
    (import (rnrs)
	    (clos user)
	    (rsa pkcs :5)
	    (crypto)
	    (math)
	    (sagittarius control))

  ;; fxxk!!!
  ;; This is Sun's specific so there is no specification
  (define pbe-with-md5-and-des3 :pbe-with-md5-and-des3)

  (define-method derive-key&iv ((m (eql :jceks))
				(key <pbe-secret-key>)
				(param <pbe-parameter>))
    ;; if the 2 salt halves are the same, invert one of them
    (define (update-salt! salt)
      (define (check salt)
	(let loop ((i 0))
	  (cond ((= i 4) i)
		((= (bytevector-u8-ref salt i)
		    (bytevector-u8-ref salt (+ i 4)))
		 (loop (+ i 1)))
		(else i))))
      (let ((index (check salt)))
	(when (= index 4)
	  (let loop ((i 0))
	    (unless (= i 2)
	      (let ((tmp (bytevector-u8-ref salt i)))
		(bytevector-u8-set! salt i (bytevector-u8-ref salt (- 3 i)))
		;; I think it's a bug but they can't fix it anymore huh?
		(bytevector-u8-ref salt (- 3 1) tmp)
		(loop (+ i 1))))))
	salt))
    
    (let ((salt (update-salt! (slot-ref param 'salt)))
	  (count (slot-ref param 'iteration))
	  ;; should we get this from key? but kinda silly since we only
	  ;; use this for this particular algorithm...
	  (result  (make-bytevector (+ (slot-ref key 'length)
				       (slot-ref key 'iv-size))))
	  (algo (slot-ref key 'hash))
	  (pass (string->utf8 (slot-ref key 'password))))
      (dotimes (i 2)
	(let* ((salt-len (bytevector-length salt))
	       (half (/ salt-len 2))
	       (buf-len (hash-size algo))
	       (buf (make-bytevector buf-len)))
	  ;; the first data is not the same length as buf
	  (hash-init! algo)
	  (hash-process! algo salt (* i half) (+ (* i half) half))
	  (hash-process! algo pass)
	  (hash-done! algo buf)
	  (dotimes (j (- count 1))
	    (hash-init! algo)
	    (hash-process! algo buf)
	    (hash-process! algo pass)
	    (hash-done! algo buf))
	  (bytevector-copy! buf 0 result (* i 16) buf-len)))
      (values (bytevector-copy result 0 (slot-ref key 'length))
	      (bytevector-copy result (slot-ref key 'length)))))

  (define-method generate-secret-key ((marker (eql pbe-with-md5-and-des3))
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD5)
	  :scheme DES3 :iv-size 8 :length 24
	  :type :jceks))

  (register-spi pbe-with-md5-and-des3 <pbe-cipher-spi>)
)
