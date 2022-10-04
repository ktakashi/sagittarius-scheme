;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/ciphers/types.scm - Cipher types
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
(library (sagittarius crypto ciphers types)
    (export cipher? <cipher>
	    cipher-scheme

	    symmetric-cipher? <symmetric-cipher>
	    symmetric-cipher-mode
	    symmetric-cipher-padder symmetric-cipher-unpadder
	    symmetric-cipher-key symmetric-cipher-key-set!
	    symmetric-cipher-direction symmetric-cipher-direction-set!
	    cipher-direction *cipher-directions*

	    asymmetric-cipher? <asymmetric-cipher>
	    asymmetric-cipher-encoder asymmetric-cipher-decoder)
    (import (rnrs)
	    (clos user)
	    (sagittarius mop immutable))

(define-class <cipher> (<immutable>) 
  ((scheme :init-keyword :scheme :reader cipher-scheme)))
(define (cipher? o) (is-a? o <cipher>))

(define-enumeration cipher-direction
  (encrypt decrypt)
  cipher-directions)
(define *cipher-directions* (enum-set-universe (cipher-directions)))

(define-class <symmetric-cipher> (<cipher>)
  ((mode :init-keyword :mode :reader symmetric-cipher-mode)
   (padder :init-keyword :padder :reader symmetric-cipher-padder)
   (unpadder :init-keyword :unpadder :reader symmetric-cipher-unpadder)
   (key :reader symmetric-cipher-key
	:writer symmetric-cipher-key-set!
	:mutable #t
	:init-value #f)
   (direction :reader symmetric-cipher-direction
	      :writer symmetric-cipher-direction-set!
	      :mutable #t
	      :init-value #f)))
(define (symmetric-cipher? o) (is-a? o <symmetric-cipher>))

(define-class <asymmetric-cipher> (<cipher>)
  ((encoder :init-keyword :encoding :reader asymmetric-cipher-encoder)
   (decoder :init-keyword :encoding :reader asymmetric-cipher-decoder)))
(define (asymmetric-cipher? o) (is-a? o <asymmetric-cipher>))

)
