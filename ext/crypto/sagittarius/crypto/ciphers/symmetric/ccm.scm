;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/ciphers/symmetric/ccm.scm - CCM mode
;;;
;;;   Copyright (c) 2024  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius crypto ciphers symmetric ccm)
    (export *mode:ccm*)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius crypto descriptors)
	    (sagittarius crypto parameters)
	    (sagittarius crypto secure)
	    (prefix (sagittarius crypto tomcrypt) tc:))

(define-record-type ccm-state
  (fields cipher
	  (mutable key)
	  (mutable state) ;; native state
	  data		  ;; plain / cipher text
	  tag-len	  ;; tag length
	  aad		  ;; aad
	  nonce))	  ;; nonce / iv
	  
(define (ccm-start cipher key parameter)
  (define iv (cipher-parameter-iv parameter))
  (define aad (cipher-parameter-aad parameter #f))
  (define tag-len (cipher-parameter-tag-length parameter))
  (let ((state (make-ccm-state cipher
			       (bytevector-copy key)
			       #f
			       (open-output-bytevector)
			       tag-len
			       (open-output-bytevector)
			       (open-output-bytevector))))
    (ccm-add-nonce! state iv)
    (when aad (ccm-add-aad! state aad))
    state))
(define (ccm-encrypt! state pt ps ct cs len)
  (let ((l (- (bytevector-length pt) ps)))
    (put-bytevector (ccm-state-data state) pt ps l))
  0)
(define (ccm-decrypt! state ct cs pt ps len)
  (let ((l (- (bytevector-length ct) cs)))
    (put-bytevector (ccm-state-data state) ct cs l))
  0)
;; remove key
(define (ccm-done state . oopts)
  (let ((k (ccm-state-key state)))
    (bytevector-fill! k 0)
    (ccm-state-key-set! state #f)))

(define (make-raw-ccm-state state)
  (define data (extract-output-bytevector (ccm-state-data state)))
  (define aad (get-output-bytevector (ccm-state-aad state)))
  (define nonce (get-output-bytevector (ccm-state-nonce state)))
  (let ((ccm-state
	(tc:ccm-init (block-cipher-descriptor-cipher (ccm-state-cipher state))
		     (ccm-state-key state)
		     (bytevector-length data)
		     (ccm-state-tag-len state)
		     (bytevector-length aad))))
    (tc:ccm-add-nonce! ccm-state nonce)
    (tc:ccm-add-aad! ccm-state aad)
    (ccm-state-state-set! state ccm-state)
    (values ccm-state data)))

;; This doesn't check ct's length
;; we need one more API to return expected size
(define (ccm-encrypt-last! state pt ps ct cs len)
  (ccm-encrypt! state pt ps ct cs len) ;; add the last block
  (let-values (((ccm-state data) (make-raw-ccm-state state)))
    (tc:ccm-encrypt! ccm-state data 0 ct cs len)))

(define (ccm-decrypt-last! state ct cs pt ps len)
  (ccm-decrypt! state ct cs pt ps len) ;; add the last block
  (let-values (((ccm-state data) (make-raw-ccm-state state)))
    (tc:ccm-decrypt! ccm-state data 0 pt ps (bytevector-length data))))

(define (ccm-compute-tag! state tag start)
  (let ((ccm-state (ccm-state-state state)))
    (unless ccm-state (assertion-violation 'mode-compute-tag! "Invalid state"))
    (tc:ccm-done! ccm-state tag start)))

(define (ccm-verify-tag! state tag start)
  (let ((ccm-state (ccm-state-state state)))
    (unless ccm-state (assertion-violation 'mode-validate-tag! "Invalid state"))
    (let* ((len (ccm-state-tag-len state))
	   (tmp (make-bytevector len)))
      (tc:ccm-done! ccm-state tmp 0)
      (unless (safe-bytevector=? tag tmp start 0 len)
	(error 'mode-validate-tag! "Tag unmatched")))))

(define (ccm-add-aad! state aad . opt)
  (when (ccm-state-state state)
    (assertion-violation 'block-cipher-update-aad! "Invalid state"))
  (apply put-bytevector (ccm-state-aad state) aad opt))
;; maybe we shouldn't accept later initialization?
(define (ccm-add-nonce! state nonce . opt)
  (when (ccm-state-state state)
    (assertion-violation 'block-cipher-update-aad! "Invalid state"))
  (apply put-bytevector (ccm-state-nonce state) nonce opt))

(define (ccm-last-block-size state size)
  (+ size (port-position (ccm-state-data state))))
(define *mode:ccm*
  (make-encauth-mode-descriptor
   ;;
   #f					; no tomcrypt mode value
   "CCM"
   ccm-start
   ccm-encrypt! ccm-encrypt-last!
   ccm-decrypt! ccm-decrypt-last!
   ccm-done
   #f #f
   ccm-last-block-size
   (lambda (_) 16)
   ccm-compute-tag!
   ccm-verify-tag!
   ccm-add-aad!
   ccm-add-nonce!))
)
