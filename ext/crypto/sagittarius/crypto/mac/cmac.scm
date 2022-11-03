;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/mac/cmac.scm - CMAC
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
(library (sagittarius crypto mac cmac)
    (export *mac:cmac*)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto mac types)
	    (sagittarius crypto descriptors cipher)
	    (prefix (sagittarius crypto tomcrypt) tc:))

(define *mac:cmac* :cmac)
(define-class <cmac-state> (<mac-state>) ())
(define (cmac-state? o) (is-a? o <cmac-state>))

(define-method mac-state-initializer ((m (eql *mac:cmac*)) (key <bytevector>)
	      :key ((cipher block-cipher-descriptor?) #f)
	      :allow-other-keys)
  (values (lambda ()
	    (make <cmac-state>
	      :state (tc:cmac-init (symmetric-cipher-descriptor-cipher cipher)
				   key)))
	  (block-cipher-descriptor-block-length cipher)))

(define-method mac-state-processor ((s (eql *mac:cmac*))) cmac-state-processor)
(define-method mac-state-finalizer ((s (eql *mac:cmac*))) cmac-state-finalizer)

(define (cmac-state-processor (state cmac-state?) (msg bytevector?)
			      :optional (start 0)
					(len (- (bytevector-length msg) start)))
  (tc:cmac-process! (mac-state-state state) msg start len))

(define (cmac-state-finalizer (state cmac-state?) (out bytevector?)
			      :optional (start 0)
					(len (- (bytevector-length out) start)))
  (tc:cmac-done! (mac-state-state state) out start len))

)
