;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/spi.scm - Cipher SPI
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

;; just for backward compatible
#!deprecated
#!nounbound
(library (crypto spi)
    (export <legacy-crypto>
	    legacy-cipher-spi? <legacy-cipher-spi>
	    legacy-builtin-cipher-spi?
	    cipher-spi-name cipher-spi-key cipher-spi-encrypt
	    cipher-spi-decrypt cipher-spi-padder cipher-spi-signer
	    cipher-spi-verifier cipher-spi-keysize cipher-spi-data
	    cipher-spi-blocksize cipher-spi-iv cipher-spi-update-aad
	    cipher-spi-tag cipher-spi-tagsize
	    make-builtin-cipher-spi
	    cipher-spi-descriptor
	    
	    legacy-cipher? <legacy-cipher>
	    cipher-spi

	    cipher-blocksize
	    lookup-cipher-spi
	    register-spi
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto descriptors)
	    (sagittarius crypto parameters)
	    (sagittarius crypto ciphers symmetric))
(define-class <legacy-crypto> () ())

(define-class <legacy-cipher-spi> (<legacy-crypto>)
  ((name :init-keyword :name :reader cipher-spi-name)
   (key :init-keyword :key :reader cipher-spi-key :init-value #f)
   (encrypt :init-keyword :encrypt :reader cipher-spi-encrypt)
   (decrypt :init-keyword :decrypt :reader cipher-spi-decrypt)
   (padder :init-keyword :padder :reader cipher-spi-padder)
   (signer :init-keyword :signer :reader cipher-spi-signer)
   (verifier :init-keyword :verifier :reader cipher-spi-verifier)
   (keysize :init-keyword :keysize :reader cipher-spi-keysize)
   (data :init-keyword :data :reader cipher-spi-data)
   (blocksize :init-keyword :data :reader cipher-spi-blocksize)
   (iv :init-keyword :iv :reader cipher-spi-iv)
   (update-aad :init-keyword :update-aad :reader cipher-spi-update-aad)
   (tag :init-keyword :tag :reader cipher-spi-tag)
   (tagsize :init-keyword :tagsize :reader cipher-spi-tagsize)))
(define (legacy-cipher-spi? o) (is-a? o <legacy-cipher-spi>))
(define-class <legacy-builtin-cipher-spi> (<legacy-cipher-spi>)
  ((descriptor :init-keyword :descriptor :reader cipher-spi-descriptor)))
(define (legacy-builtin-cipher-spi? o) (is-a? o <legacy-builtin-cipher-spi>))

(define-class <legacy-cipher> (<legacy-crypto>)
  ((spi :init-keyword :spi :reader cipher-spi)))
(define (legacy-cipher? o) (is-a? o <legacy-cipher>))

(define-generic cipher-blocksize)
(define-method cipher-blocksize ((d <block-cipher-descriptor>))
  (block-cipher-descriptor-block-length d))
(define-method cipher-blocksize ((c <legacy-builtin-cipher-spi>))
  (block-cipher-descriptor-block-length (cipher-spi-descriptor c)))
(define-method cipher-blocksize ((c <legacy-cipher>))
  (cipher-blocksize (cipher-spi c)))

(define-generic lookup-cipher-spi)
(define-method lookup-cipher-spi (o) #f)

(define (make-builtin-cipher-spi desc mode key iv rounds padder ctr-mode)
  (let ((cipher (make-block-cipher desc mode padder))
	(parameter (apply make-cipher-parameter
		    (filter values (list (and iv (make-iv-parameter iv))
					 (make-counter-mode-parameter ctr-mode)
					 (make-round-parameter rounds))))))
    ;; dummy direction... another reason why we must replace this
    ;; legacy
    (block-cipher-init! cipher (cipher-direction encrypt) key parameter)
    (make <legacy-builtin-cipher-spi>
      :descriptor desc
      :name (cipher-descriptor-name desc)
      :key key
      :iv iv ;; fake
      :encrypt (block-encrypt cipher parameter)
      :decrypt (block-decrypt cipher parameter)
      :signer (lambda ignore
		(error 'cipher-signature
		       "Symmetric cipher doesn't not support signing"))
      :verifier (lambda ignore
		  (error 'cipher-verify
			 "Symmetric cipher doesn't not support verify"))
      :keysize (lambda (ignore)
		 (block-cipher-descriptor-suggested-key-length desc))
      :data #f
      :block-size (block-cipher-descriptor-block-length desc)
      :update-aad (lambda (aad . opts)
		    (apply block-cipher-update-aad! cipher aad opts))
      :tag (lambda (tag) (block-cipher-done/tag! cipher tag))
      :tagsize (block-cipher-max-tag-length cipher))))

(define ((block-encrypt cipher parameter) bv len key)
  (slot-set! cipher 'direction (cipher-direction encrypt))
  (block-cipher-encrypt-last-block cipher bv))
(define ((block-decrypt cipher parameter) bv len key)
  (slot-set! cipher 'direction (cipher-direction decrypt))
  (block-cipher-decrypt-last-block cipher bv))

(define-syntax register-spi
  (syntax-rules ()
    ((_ name class)
     (define-method lookup-cipher-spi ((m (eql name))) class))))
)
