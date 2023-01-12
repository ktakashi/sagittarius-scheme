;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/mac/gmac.scm - GMAC
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
(library (sagittarius crypto mac gmac)
    (export *mac:gmac*)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto mac types)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto keys))

(define *mac:gmac* :gmac)

(define-record-type gmac-state
  (fields cipher))

(define-method mac-state-initializer ((m (eql *mac:gmac*)) (key <bytevector>)
	       :key ((cipher block-cipher-descriptor?) *scheme:aes*)
		    ((iv bytevector?) #f)
	       :allow-other-keys opts)
  (let ((skey (generate-symmetric-key cipher key))
	(bc (make-block-cipher cipher *mode:gcm* no-padding))
	(iv (make-iv-parameter iv)))
    (values (lambda ()
	      (let ((nc (block-cipher-init bc (cipher-direction encrypt)
					   skey iv)))
		(make-gmac-state nc)))
	    16 ;; GCM tag length
	    #f)))

(define-method mac-state-processor ((s (eql *mac:gmac*))) gmac-process!)
(define-method mac-state-finalizer ((s (eql *mac:gmac*))) gmac-done!)

(define (gmac-process! (s gmac-state?) (bv bytevector?) . opts)
  (apply block-cipher-update-aad! (gmac-state-cipher s) bv opts))
(define (gmac-done! (s gmac-state?) (out bytevector?)
		    :optional (start 0) (len (- (bytevector-length out) start)))
  (let ((tmp (make-bytevector len 0)))
    (block-cipher-done/tag! (gmac-state-cipher s) tmp)
    (bytevector-copy! tmp 0 out start len)))
)
