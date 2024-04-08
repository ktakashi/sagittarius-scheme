;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/parameters/cipher.scm - Cipher parameters
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

;; Cipher parameters, we share as much as possible
#!nounbound
(library (sagittarius crypto parameters cipher)
    (export cipher-parameter? make-cipher-parameter
	    (rename (cipher-parameter <cipher-parameter>))

	    make-round-parameter round-parameter? cipher-parameter-rounds
	    make-iv-parameter iv-parameter? cipher-parameter-iv
	    make-counter-mode-parameter counter-mode-parameter?
	    cipher-parameter-counter-mode
	    make-tweak-parameter tweak-parameter? cipher-parameter-tweak
	    make-salt-parameter salt-parameter? cipher-parameter-salt
	    make-nonce-parameter nonce-parameter? cipher-parameter-nonce
	    make-aad-parameter aad-parameter? cipher-parameter-aad
	    make-tag-length-parameter tag-length-parameter?
	    cipher-parameter-tag-length
	    
	    make-counter-parameter counter-parameter?
	    cipher-parameter-counter

	    make-tag-parameter tag-parameter? cipher-parameter-tag
	    
	    define-cipher-parameter
	    ;; FWIW
	    make-define-cipher-parameter
	    (rename (round-parameter <round-parameter>)
		    (iv-parameter <iv-parameter>)
		    (counter-mode-parameter <counter-mode-parameter>)
		    (tweak-parameter <tweak-parameter>)
		    (salt-parameter <salt-parameter>)
		    (nonce-parameter <nonce-parameter>)
		    (aad-parameter <aad-parameter>)
		    (tag-length-parameter <tag-length-parameter>)
		    (counter-parameter <counter-parameter>)))
    (import (rnrs)
	    (sagittarius crypto parameters misc))
(define-compositable-record-type cipher-parameter)
(define-syntax define-cipher-parameter (make-define-cipher-parameter))
;; round
(define-cipher-parameter round-parameter
  make-round-parameter round-parameter?
  (rounds cipher-parameter-rounds))

(define copy-bytevector-procotol
  (lambda (p)
    (lambda (bv . opts)
      ((p) (apply bytevector-copy bv opts)))))
;; IV
(define-cipher-parameter iv-parameter
  (make-iv-parameter copy-bytevector-procotol) iv-parameter?
  (iv cipher-parameter-iv))

;; counter mode
(define-cipher-parameter counter-mode-parameter
  make-counter-mode-parameter counter-mode-parameter?
  (counter-mode cipher-parameter-counter-mode))

;; tweak
(define-cipher-parameter tweak-parameter
  (make-tweak-parameter copy-bytevector-procotol) tweak-parameter?
  (tweak cipher-parameter-tweak))

;; salt
(define-cipher-parameter salt-parameter
  (make-salt-parameter copy-bytevector-procotol) salt-parameter?
  (salt cipher-parameter-salt))

;; nonce
(define-cipher-parameter nonce-parameter
  (make-nonce-parameter copy-bytevector-procotol) nonce-parameter?
  (nonce cipher-parameter-nonce))

;; aad
(define-cipher-parameter aad-parameter
  (make-aad-parameter copy-bytevector-procotol) aad-parameter?
  (aad cipher-parameter-aad))

;; tag-length (for OCB3)
(define-cipher-parameter tag-length-parameter
  make-tag-length-parameter tag-length-parameter?
  (tag-length cipher-parameter-tag-length))

;; counter (for stream ciphers)
(define-cipher-parameter counter-parameter
  make-counter-parameter counter-parameter?
  (counter cipher-parameter-counter))

;; tag (for GCM-SIV)
(define-cipher-parameter tag-parameter
  (make-tag-parameter copy-bytevector-procotol) tag-parameter?
  (tag cipher-parameter-tag))

)
