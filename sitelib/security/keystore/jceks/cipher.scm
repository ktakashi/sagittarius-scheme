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

#!nounbound
(library (security keystore jceks cipher)
    (export +jks-keystore-oid+
	    +pbe-with-md5-and-des3-oid+)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkcs algorithms)
	    (sagittarius crypto pkcs pbes)
	    (sagittarius crypto pkcs modules pbes)
	    (sagittarius crypto digests)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto descriptors)
	    (sagittarius crypto random)
	    (sagittarius control))

;; 1.3.6.1.4.1.42 is Sun's OID thus these are Sun's specific ones
;; geez
(define-constant +jks-keystore-oid+          "1.3.6.1.4.1.42.2.17.1.1")
(define-constant +pbe-with-md5-and-des3-oid+ "1.3.6.1.4.1.42.2.19.1")

(define-method oid->x509-algorithm-parameters-types
  ((oid (equal +pbe-with-md5-and-des3-oid+)))
  (values <pkcs-pbe-parameter> <pbe-parameter>))

;; if the 2 salt halves are the same, invert one of them
(define (derive-key&iv password salt count)
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
  
  (let ((result  (make-bytevector 32))
	(md (make-message-digest *digest:md5*))
	(pass (string->utf8 password)))
    (dotimes (i 2)
      (let* ((salt-len (bytevector-length salt))
	     (half (/ salt-len 2))
	     (buf-len (digest-descriptor-digest-size *digest:md5*))
	     (buf (make-bytevector buf-len)))
	;; the first data is not the same length as buf
	(message-digest-init! md)
	(message-digest-process! md salt (* i half) half)
	(message-digest-process! md pass)
	(message-digest-done! md buf)
	(dotimes (j (- count 1))
	  (message-digest-init! md)
	  (message-digest-process! md buf)
	  (message-digest-process! md pass)
	  (message-digest-done! md buf))
	(bytevector-copy! buf 0 result (* i 16) buf-len)))
    (values (bytevector-copy result 0 24)
	    (bytevector-copy result 24))))

;; It looks very similar to PBKDF1 but not the same...
(define-method oid->kdf ((oid (equal +pbe-with-md5-and-des3-oid+)) param)
  (let ((salt (pkcs-pbe-parameter-salt param))
	(iteration (pkcs-pbe-parameter-iteration-count param)))
    (lambda (key . ignore)
      (let-values (((dk iv) (derive-key&iv key salt iteration)))
	dk))))
(define-method oid->cipher ((oid (equal +pbe-with-md5-and-des3-oid+)) param)
  (let ((salt (pkcs-pbe-parameter-salt param))
	(iteration (pkcs-pbe-parameter-iteration-count param)))
    (lambda (key)
      (let-values (((dk iv) (derive-key&iv key salt iteration)))
	(values (make-symmetric-cipher *scheme:des3* *mode:cbc*)
		(make-iv-parameter iv))))))
)
