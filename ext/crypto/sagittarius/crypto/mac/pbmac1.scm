;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/mac/pbmac1.scm - Password-based MAC 1
;;;
;;;   Copyright (c) 2026  Sagittarius Scheme contributors
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

;; RFC 8018, Section 7.1
#!nounbound
(library (sagittarius crypto mac pbmac1)
    (export *mac:pbmac1*)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto mac types))

(define *mac:pbmac1* :pbmac1)

(define-method mac-state-initializer ((m (eql *mac:pbmac1*))
				      (password <bytevector>)
	      :key (kdf #f) (mac-provider #f)
	      :allow-other-keys)
  (unless (procedure? kdf)
    (assertion-violation 'mac-state-initializer
			 "PBMAC1 requires a KDF procedure" kdf))
  (unless (procedure? mac-provider)
    (assertion-violation 'mac-state-initializer
			 "PBMAC1 requires a MAC provider procedure" mac-provider))
  (let ((key (kdf password)))
    (unless (bytevector? key)
      (assertion-violation 'mac-state-initializer
			   "PBMAC1 KDF must return a bytevector" key))
    (let ((inner (mac-provider key)))
      (unless (mac? inner)
	(assertion-violation 'mac-state-initializer
			     "PBMAC1 provider must return a MAC" inner))
      (values (lambda () (mac-init! (mac-provider key)))
	      (mac-mac-size inner)
	      "1.2.840.113549.1.5.14"))))

(define-method mac-state-processor ((m (eql *mac:pbmac1*)))
  (lambda (inner msg . opts) (apply mac-process! inner msg opts)))
(define-method mac-state-finalizer ((m (eql *mac:pbmac1*)))
  (lambda (inner out . opts) (apply mac-done! inner out opts)))

)
