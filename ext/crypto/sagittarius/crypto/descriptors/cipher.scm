;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/descriptors/cipher.scm - Cipher descriptor
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
(library (sagittarius crypto descriptors cipher)
    (export cipher-descriptor? (rename (cipher-descriptor <cipher-descriptor>)) 
	    cipher-descriptor-name

	    symmetric-cipher-descriptor?
	    (rename (symmetric-cipher-descriptor <symmetric-cipher-descriptor>))
	    symmetric-cipher-descriptor-min-key-length
	    symmetric-cipher-descriptor-max-key-length

	    block-cipher-descriptor?
	    (rename (block-cipher-descriptor <block-cipher-descriptor>))
	    block-cipher-descriptor-cipher
	    block-cipher-descriptor-block-length
	    block-cipher-descriptor-default-rounds
	    block-cipher-descriptor-suggested-key-length

	    stream-cipher-descriptor?
	    (rename (stream-cipher-descriptor <stream-cipher-descriptor>))
	    stream-cipher-descriptor-aead?
	    stream-cipher-descriptor-init
	    stream-cipher-descriptor-set-iv!
	    stream-cipher-descriptor-add-aad!
	    stream-cipher-descriptor-encrypt!
	    stream-cipher-descriptor-decrypt!
	    stream-cipher-descriptor-done!
	    stream-cipher-descriptor-done/tag!
	    
	    asymmetric-cipher-descriptor? make-asymmetric-cipher-descriptor
	    (rename (asymmetric-cipher-descriptor <asymmetric-cipher-descriptor>))
	    asymmetric-cipher-descriptor-block-size
	    asymmetric-cipher-descriptor-setup
	    asymmetric-cipher-descriptor-encrypt
	    asymmetric-cipher-descriptor-decrypt
	    asymmetric-cipher-descriptor-done

	    *scheme:blowfish*
	    *scheme:x-tea*
	    *scheme:rc2* *scheme:rc5* *scheme:rc6*
	    *scheme:safer+* *scheme:safer-k64* *scheme:safer-sk64*
	    *scheme:safer-k128* *scheme:safer-sk128*
	    *scheme:aes* *scheme:aes-128* *scheme:aes-192* *scheme:aes-256*
	    *scheme:twofish*
	    *scheme:des* *scheme:des3* *scheme:desede*
	    *scheme:cast5* *scheme:cast-128*
	    *scheme:noekeon*
	    *scheme:skipjack*
	    *scheme:khazad*
	    *scheme:seed*
	    *scheme:kasumi*
	    *scheme:camellia*
	    ;; stream ciphers
	    *scheme:chacha20*
	    *scheme:chacha20-poly1305*
	    )
    (import (rnrs)
	    (clos core)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto parameters)
	    (prefix (sagittarius crypto tomcrypt) tc:))
(define-record-type cipher-descriptor
  (fields name))
(define-method write-object ((o cipher-descriptor) p)
  (format p "#<~a ~a>" (class-name (class-of o)) (cipher-descriptor-name o)))
;; A wrapper to avoid constant folding, the cipher descriptor is a
;; mere fixnum, and may change every invocation of sagittarius.
;; (shouldn't be but if I touch the order of registration for example)
;; So, I make a record to wrap this the descriptors 
(define-record-type symmetric-cipher-descriptor
  (parent cipher-descriptor)
  (fields min-key-length
	  max-key-length))
(define-record-type block-cipher-descriptor
  (parent symmetric-cipher-descriptor)
  (fields cipher ;; real cipher descriptor, a fixnum
	  block-length
	  default-rounds))

(define-record-type stream-cipher-descriptor
  (parent symmetric-cipher-descriptor)
  (fields aead?
	  initializer
	  iv-setter
	  aad-setter
	  encrypter
	  decrypter
	  finalizer))

(define (stream-cipher-descriptor-init desc key param)
  ((stream-cipher-descriptor-initializer desc) key param))
(define (stream-cipher-descriptor-set-iv! desc state param)
  (cond ((stream-cipher-descriptor-iv-setter desc) =>
	 (lambda (p) (p state param)))
	(else #f)))
(define (stream-cipher-descriptor-add-aad! desc state aad . opts)
  (cond ((stream-cipher-descriptor-aad-setter desc) =>
	 (lambda (p) (apply p state aad opts)))
	(else #f)))
(define (stream-cipher-descriptor-encrypt! desc state in ins out outs len)
  ((stream-cipher-descriptor-encrypter desc) state in ins out outs len))
(define (stream-cipher-descriptor-decrypt! desc state in ins out outs len)
  ((stream-cipher-descriptor-decrypter desc) state in ins out outs len))
(define (stream-cipher-descriptor-done! desc state)
  (cond ((not (stream-cipher-descriptor-aead? desc))
	 ((stream-cipher-descriptor-finalizer desc) state))
	(else
	 (assertion-violation 'stream-cipher-descriptor-done!
			      "AEAD stream cipher is given"
			      (cipher-descriptor-name desc)))))
(define (stream-cipher-descriptor-done/tag! desc state tag
	 :optional (start 0))
  (cond ((stream-cipher-descriptor-aead? desc)
	 ((stream-cipher-descriptor-finalizer desc) state tag start))
	(else
	 (assertion-violation 'stream-cipher-descriptor-done!
			      "Non AEAD stream cipher is given"
			      (cipher-descriptor-name desc)))))


(define (block-cipher-descriptor-suggested-key-length descriptor . opts)
  (unless (block-cipher-descriptor? descriptor)
    (assertion-violation 'block-cipher-descriptor-suggested-key-length
			 "Cipher descriptor is required" descriptor))
  (apply tc:cipher-descriptor-suggested-keysize 
	 (block-cipher-descriptor-cipher descriptor)
	 opts))

(define-record-type asymmetric-cipher-descriptor
  (parent cipher-descriptor)
  ;; fields are all procedures
  (fields block-size setup encrypt decrypt done))


;; Builtin ciphers, we don't support custom block cipher
(define-syntax build-cipher-descriptor
  (syntax-rules ()
    ((_ name)
     (let ((cipher (tc:find-cipher name)))
       (make-block-cipher-descriptor
	(tc:cipher-descriptor-name cipher)
	(tc:cipher-descriptor-min-key-length cipher)
	(tc:cipher-descriptor-max-key-length cipher)
	cipher
	(tc:cipher-descriptor-block-length cipher)
	(tc:cipher-descriptor-default-rounds cipher))))))

(define *scheme:blowfish*    (build-cipher-descriptor tc:*scheme:blowfish*   ))
(define *scheme:x-tea*       (build-cipher-descriptor tc:*scheme:x-tea*      ))
(define *scheme:rc2*         (build-cipher-descriptor tc:*scheme:rc2*        ))
(define *scheme:rc5*         (build-cipher-descriptor tc:*scheme:rc5*        ))
(define *scheme:rc6*         (build-cipher-descriptor tc:*scheme:rc6*        ))
(define *scheme:safer+*      (build-cipher-descriptor tc:*scheme:safer+*     ))
(define *scheme:safer-k64*   (build-cipher-descriptor tc:*scheme:safer-k64*  ))
(define *scheme:safer-sk64*  (build-cipher-descriptor tc:*scheme:safer-sk64* ))
(define *scheme:safer-k128*  (build-cipher-descriptor tc:*scheme:safer-k128* ))
(define *scheme:safer-sk128* (build-cipher-descriptor tc:*scheme:safer-sk128*))
(define *scheme:aes*         (build-cipher-descriptor tc:*scheme:aes*        ))
(define *scheme:aes-128*     (build-cipher-descriptor tc:*scheme:aes-128*    ))
(define *scheme:aes-192*     (build-cipher-descriptor tc:*scheme:aes-192*    ))
(define *scheme:aes-256*     (build-cipher-descriptor tc:*scheme:aes-256*    ))
(define *scheme:twofish*     (build-cipher-descriptor tc:*scheme:twofish*    ))
(define *scheme:des*         (build-cipher-descriptor tc:*scheme:des*        ))
(define *scheme:des3*        (build-cipher-descriptor tc:*scheme:des3*       ))
(define *scheme:desede*      *scheme:des3*)
(define *scheme:cast5*       (build-cipher-descriptor tc:*scheme:cast5*))
(define *scheme:cast-128*    *scheme:cast5*)
(define *scheme:noekeon*     (build-cipher-descriptor tc:*scheme:noekeon* ))
(define *scheme:skipjack*    (build-cipher-descriptor tc:*scheme:skipjack*))
(define *scheme:khazad*      (build-cipher-descriptor tc:*scheme:khazad*  ))
(define *scheme:seed*        (build-cipher-descriptor tc:*scheme:seed*    ))
(define *scheme:kasumi*      (build-cipher-descriptor tc:*scheme:kasumi*  ))
(define *scheme:camellia*    (build-cipher-descriptor tc:*scheme:camellia*))

(define *scheme:chacha20* (make-stream-cipher-descriptor
			   "chacha20" 16 32 #f
			   (lambda (key p) (tc:chacha-setup key 0)) ;; round = 0
			   (lambda (st param)
			     (let ((iv (cipher-parameter-iv param))
				   (counter (cipher-parameter-counter param)))
			       (tc:chacha-ivctr! st iv counter)))
			   #f
			   tc:chacha-crypt!
			   tc:chacha-crypt!
			   tc:chacha-done!))

(define *scheme:chacha20-poly1305*
  (make-stream-cipher-descriptor
   "chacha20-poly1305" 16 32 #t
   (lambda (key p) (tc:chacha20-poly1305-setup key))
   (lambda (st param)
     (let ((iv (cipher-parameter-iv param)))
       (tc:chacha20-poly1305-setiv! st iv)))
   tc:chacha20-poly1305-add-aad!
   tc:chacha20-poly1305-encrypt!
   tc:chacha20-poly1305-decrypt!
   tc:chacha20-poly1305-done!))
)
