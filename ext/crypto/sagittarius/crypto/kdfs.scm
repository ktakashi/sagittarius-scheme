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
	    mac->prf-provider
	    hkdf
	    pkcs12-kdf
	    ;; hmmmm should these be here?
	    pkcs12-derive-iv
	    pkcs12-derive-mac)
    (import (rnrs)
	    (sagittarius crypto digests)
	    (rename (sagittarius crypto digests descriptors)
		    (tc-digest-descriptor? builtin-digest-descriptor?))
	    (sagittarius crypto mac)
	    (sagittarius crypto kdfs pbkdf-2)
	    (prefix (sagittarius crypto tomcrypt) tc:))

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

;; HKDF: RFC 5869
(define (hkdf (digest builtin-digest-descriptor?) ikm salt info dk-len)
  (tc:hkdf (tc-digest-descriptor-digest digest) ikm salt info dk-len))

(define (pkcs12-kdf (digest builtin-digest-descriptor?)
		    pw salt iteration dk-len)
  (call-tc:pkcs12-kdf digest pw salt iteration dk-len tc:*pkcs12:key-material*))
(define (pkcs12-derive-iv (digest builtin-digest-descriptor?)
			  pw salt iteration iv-len)
  (call-tc:pkcs12-kdf digest pw salt iteration iv-len tc:*pkcs12:iv-material*))
(define (pkcs12-derive-mac (digest builtin-digest-descriptor?)
			   pw salt iteration len)
  (call-tc:pkcs12-kdf digest pw salt iteration len tc:*pkcs12:mac-material*))


(define (call-tc:pkcs12-kdf digest pw salt iteration len purpose)
  (tc:pkcs12-kdf (tc-digest-descriptor-digest digest)
		 (string->utf16 (string-append pw "\x0;") (endianness big))
		 salt iteration purpose len))
)
