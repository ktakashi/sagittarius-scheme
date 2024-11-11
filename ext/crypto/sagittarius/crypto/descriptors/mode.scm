;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/descriptors/mode.scm - Mode descriptor
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

;; the same concept of the cipher descriptor
;; this one is a bit more complicated as we also want to
;; use modes defined in Scheme world, e.g. PBE
;; NOTE: Implementing PBE as a mode is very convenient
;;       learnt from the experience of Springkussen
#!nounbound
(library (sagittarius crypto descriptors mode)
    (export *mode:ecb* *mode:cbc* *mode:cfb* *mode:ofb*
	    *mode:ctr* *mode:lrw* *mode:f8*

	    (rename (tc:*ctr-mode:little-endian* *ctr-mode:little-endian*)
		    (tc:*ctr-mode:big-endian*    *ctr-mode:big-endian*)
		    (tc:*ctr-mode:rfc3686*       *ctr-mode:rfc3686*))
	    
	    make-mode-descriptor
	    mode-descriptor? (rename (mode-descriptor <mode-descriptor>))
	    mode-descriptor-name
	    mode-descriptor-has-set-iv!?
	    mode-descriptor-has-get-iv!?

	    *mode:eax* *mode:ocb* *mode:ocb3* *mode:gcm*

	    mode-key?
	    mode-start
	    mode-encrypt! mode-encrypt-last!
	    mode-decrypt! mode-decrypt-last!
	    mode-done!
	    mode-set-iv! mode-get-iv!
	    mode-last-block-size
	    
	    make-encauth-mode-descriptor
	    encauth-mode-descriptor?
	    mode-has-add-aad!? mode-add-aad!
	    mode-has-add-iv!? mode-add-iv!
	    mode-compute-tag!
	    mode-validate-tag!
	    mode-max-tag-length)
    (import (rnrs)
	    (sagittarius)
	    (prefix (sagittarius crypto tomcrypt) tc:)
	    (sagittarius crypto secure)
	    (sagittarius crypto descriptors cipher)
	    (sagittarius crypto parameters))

;; Using record method dispatch, using CLOS seems cleaner but
;; it's extremely slower than the record method dispatch.
;; In general, we mustn't use CLOS in a frequently called procedure
;; NOTE: encauth has its own mode
(define-record-type mode-descriptor
  (fields mode ;; internal enum value, do we need?
	  name
	  start
	  encrypt encrypt-last 
	  decrypt decrypt-last
	  done set-iv get-iv
	  last-block-size))

(define (mode-descriptor-has-set-iv!? descriptor)
  (and (mode-descriptor-set-iv descriptor) #t))
(define (mode-descriptor-has-get-iv!? descriptor)
  (and (mode-descriptor-get-iv descriptor) #t))

(define-record-type mode-key
  (fields descriptor state-key))

(define (mode-start descriptor cipher key parameter)
  (make-mode-key descriptor
		 ((mode-descriptor-start descriptor) cipher key parameter)))
(define (mode-encrypt! mode-key pt ps ct cs len)
  ((mode-descriptor-encrypt (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key) pt ps ct cs len))
(define (mode-encrypt-last! mode-key pt ps ct cs len)
  ((mode-descriptor-encrypt-last (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key) pt ps ct cs len))

(define (mode-decrypt! mode-key ct cs pt ps len)
  ((mode-descriptor-decrypt (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key) ct cs pt ps len))
(define (mode-decrypt-last! mode-key ct cs pt ps len)
  ((mode-descriptor-decrypt-last (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key) ct cs pt ps len))

(define (mode-done! mode-key . opts)
  (define desc (mode-key-descriptor mode-key))
  (define state (mode-key-state-key mode-key))
  ((mode-descriptor-done desc) state))

(define (mode-set-iv! mode-key iv . opts)
  (let* ((descriptor (mode-key-descriptor mode-key))
	 (set-iv (mode-descriptor-set-iv descriptor)))
    (unless set-iv
      (assertion-violation 'mode-set-iv!
			   (string-append (mode-descriptor-name descriptor)
					  " doesn't support set IV")))
    (apply set-iv (mode-key-state-key mode-key) iv opts)))
(define (mode-get-iv! mode-key bv . opts)
  (let* ((descriptor (mode-key-descriptor mode-key))
	 (get-iv (mode-descriptor-get-iv descriptor)))
    (unless get-iv
      (assertion-violation 'mode-get-iv!
			   (string-append (mode-descriptor-name descriptor)
					  " doesn't support get IV")))
    (apply get-iv (mode-key-state-key mode-key) bv opts)))

(define (mode-last-block-size mode-key size)
  ((mode-descriptor-last-block-size (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key) size))

(define (default-last-block-size state-key size) size)

(define-syntax build-mode-descriptor
  (lambda (x)
    (define (make-names k name)
      (define n (symbol->string (syntax->datum name)))
      (map (lambda (name) (datum->syntax k (string->symbol name)))
	   (list (string-append "tc:*mode:" n "*")
		 (string-upcase n)
		 (string-append n "-start")
		 (string-append "tc:" n "-encrypt!")
		 (string-append "tc:" n "-decrypt!")
		 (string-append "tc:" n "-done!")
		 (string-append "tc:" n "-set-iv!")
		 (string-append "tc:" n "-get-iv!"))))
    (syntax-case x ()
      ((k name)
       (with-syntax (((mode mode-name start encrypt decrypt done set-iv get-iv)
		      (make-names #'k #'name)))
	 #'(make-mode-descriptor mode (symbol->string 'mode-name) start
				 ;; in this library, last block handling
				 ;; are the same procedures, so be it
				 encrypt encrypt decrypt decrypt
				 done set-iv get-iv default-last-block-size))))))
(define (ecb-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f) 0))
  (tc:ecb-start (block-cipher-descriptor-cipher cipher) key rounds))
(define tc:ecb-set-iv! #f)
(define tc:ecb-get-iv! #f)

(define (cbc-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f) 0))
  (define iv (cipher-parameter-iv parameter))
  (tc:cbc-start (block-cipher-descriptor-cipher cipher) iv key rounds))

(define (cfb-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f) 0))
  (define iv (cipher-parameter-iv parameter))
  (tc:cfb-start (block-cipher-descriptor-cipher cipher) iv key rounds))

(define (ofb-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f) 0))
  (define iv (cipher-parameter-iv parameter))
  (tc:ofb-start (block-cipher-descriptor-cipher cipher) iv key rounds))

(define (ctr-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f) 0))
  (define iv (cipher-parameter-iv parameter))
  (define ctr-mode
    (cipher-parameter-counter-mode parameter tc:*ctr-mode:big-endian*))
  (tc:ctr-start (block-cipher-descriptor-cipher cipher)
		iv key rounds ctr-mode))

(define (lrw-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f) 0))
  (define iv (cipher-parameter-iv parameter))
  (define tweak (cipher-parameter-tweak parameter #f))
  (tc:lrw-start (block-cipher-descriptor-cipher cipher)
		iv key tweak rounds))

(define (f8-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f) 0))
  (define iv (cipher-parameter-iv parameter))
  (define salt (cipher-parameter-salt parameter #f))
  (tc:f8-start (block-cipher-descriptor-cipher cipher) iv key salt rounds))

(define *mode:ecb* (build-mode-descriptor ecb))
(define *mode:cbc* (build-mode-descriptor cbc))
(define *mode:cfb* (build-mode-descriptor cfb))
(define *mode:ofb* (build-mode-descriptor ofb))
(define *mode:ctr* (build-mode-descriptor ctr))
(define *mode:lrw* (build-mode-descriptor lrw))
(define *mode:f8*  (build-mode-descriptor f8))

;; encauth, we merge it to mode-descriptor
;; we support EAX, OCB, OCB3, GCM.
;; CCM is not online, means users need to know the length of plain
;; text or cipher test before hand, that's a bit inconvenient for us.
(define-record-type encauth-mode-descriptor
  (parent mode-descriptor)
  (fields max-tag-length
	  compute-tag ;; compute tag
	  verify-tag  ;; verify tag
	  add-aad     ;; EAX, OCB3, and GCM (EAX calls aad header)
	  add-iv      ;; GCM specific
	  ))
(define (mode-compute-tag! mode-key tag :optional (start 0))
  ((encauth-mode-descriptor-compute-tag (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key) tag start))
(define (mode-validate-tag! mode-key tag :optional (start 0))
  ((encauth-mode-descriptor-verify-tag (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key) tag start))
(define (mode-has-add-aad!? descriptor)
  (and (encauth-mode-descriptor? descriptor)
       (encauth-mode-descriptor-add-aad descriptor)))
(define (mode-add-aad! mode-key aad . opts)
  (apply (encauth-mode-descriptor-add-aad (mode-key-descriptor mode-key))
	 (mode-key-state-key mode-key) aad opts))
(define (mode-has-add-iv!? descriptor)
  (and (encauth-mode-descriptor? descriptor)
       (encauth-mode-descriptor-add-iv descriptor)))
(define (mode-add-iv! mode-key iv . opts)
  (apply (encauth-mode-descriptor-add-iv (mode-key-descriptor mode-key))
	 (mode-key-state-key mode-key) iv opts))

(define (mode-max-tag-length mode-key)
  (let ((mode (mode-key-descriptor mode-key)))
    (or (and (encauth-mode-descriptor? mode)
	     ((encauth-mode-descriptor-max-tag-length mode) mode-key))
	0)))

;; EAX
(define (eax-start cipher key parameter)
  (define nonce (cipher-parameter-nonce parameter #vu8()))
  ;; a bit different name :)
  (define header (cipher-parameter-aad parameter #vu8()))
  (tc:eax-init (block-cipher-descriptor-cipher cipher) key nonce header))
;; using EAX wrongly, but that's users' responsiblity
(define (eax-done state) (tc:eax-done! state #vu8()))
;; Don't accept any plain text
(define (eax-compute-tag! state tag start)
  ;; returns the tag length
  (tc:eax-done! state tag start))
(define (eax-verify-tag! state tag start)
  (let* ((len (- (bytevector-length tag) start))
	 (tmp (make-bytevector len)))
    (tc:eax-done! state tmp 0)
    (unless (safe-bytevector=? tag tmp start 0 len)
      (error 'mode-decrypt-last! "Tag unmatched"))))

(define *mode:eax* (make-encauth-mode-descriptor
		    tc:*encauth:eax* "EAX"
		    eax-start
		    tc:eax-encrypt! tc:eax-encrypt!
		    tc:eax-decrypt! tc:eax-decrypt!
		    eax-done #f #f
		    default-last-block-size
		    (lambda (ignore) 144)
		    eax-compute-tag! eax-verify-tag!
		    tc:eax-add-header! #f))

;; OCB
(define-record-type ocb-state (fields cipher key))
(define (ocb-start cipher key parameter)
  (define nonce (cipher-parameter-nonce parameter))
  (make-ocb-state cipher
   (tc:ocb-init (block-cipher-descriptor-cipher cipher) key nonce)))
(define (ocb-done state) #f) ;; do nothing (don't use this)
(define (ocb-block-size state)
  (block-cipher-descriptor-block-length (ocb-state-cipher state)))
(define (ocb-encrypt! state pt ps ct cs len)
  (define block-size (ocb-block-size state))
  ;; sanity check
  (unless (zero? (mod len block-size))
    (assertion-violation 'ocb-encrypt! "Invalid length of input"))
  (do ((i 0 (+ i block-size)) (s (ocb-state-key state)))
      ((= i len) len)
    (tc:ocb-encrypt! s pt (+ ps i) ct (+ cs i))))
(define (ocb-decrypt! state ct cs pt ps len)
  (define block-size
    (block-cipher-descriptor-block-length (ocb-state-cipher state)))
  ;; sanity check
  (unless (zero? (mod len block-size))
    (assertion-violation 'ocb-decrypt! "Invalid length of input"))
  (do ((i 0 (+ i block-size)) (s (ocb-state-key state)))
      ((= i len) len)
    (tc:ocb-decrypt! s ct (+ cs i) pt (+ ps i))))
(define (ocb-compute-tag! state tag start)
  (tc:ocb-done-encrypt! (ocb-state-key state) #vu8() 0 #vu8() 0 0 tag start))
(define (ocb-verify-tag! state tag start)
  (unless (tc:ocb-done-decrypt! (ocb-state-key state)
				#vu8() 0 #vu8() 0 0 tag start)
    (error 'mode-decrypt-last! "Tag unmatched")))
(define *mode:ocb* (make-encauth-mode-descriptor
		    tc:*encauth:ocb* "OCB"
		    ocb-start
		    ocb-encrypt! ocb-encrypt!
		    ocb-decrypt! ocb-decrypt!
		    ocb-done #f #f
		    default-last-block-size
		    ocb-block-size
		    ocb-compute-tag! ocb-verify-tag!
		    #f #f))

;; OCB3
(define-record-type ocb3-state
  (fields tag-len key))
(define (ocb3-start cipher key parameter)
  (define nonce (cipher-parameter-nonce parameter))
  (define tag-len (cipher-parameter-tag-length parameter))
  (make-ocb3-state tag-len
		   (tc:ocb3-init (block-cipher-descriptor-cipher cipher)
				 key nonce tag-len)))
(define (ocb3-done state)
  (define s (ocb3-state-key state))
  (let ((dummy (make-bytevector (ocb3-state-tag-len state))))
    (tc:ocb3-done! s dummy)))
(define (ocb3-encrypt state . rest)
  (define s (ocb3-state-key state))
  (apply tc:ocb3-encrypt! s rest))
(define (ocb3-decrypt state . rest)
  (define s (ocb3-state-key state))
  (apply tc:ocb3-decrypt! s rest))
(define (ocb3-compute-tag! state tag start)
  (define s (ocb3-state-key state))
  (tc:ocb3-encrypt-last! s #vu8() 0 #vu8() 0 0)
  (tc:ocb3-done! s tag start))
(define (ocb3-verify-tag! state tag start)
  (define s (ocb3-state-key state))
  (tc:ocb3-decrypt-last! s #vu8() 0 #vu8() 0 0)
  (let* ((len (- (bytevector-length tag) start))
	 (tmp (make-bytevector len)))
    (tc:ocb3-done! s tmp 0)
    (unless (safe-bytevector=? tag tmp start 0 len)
      (error 'mode-decrypt-last! "Tag unmatched"))))

(define *mode:ocb3* (make-encauth-mode-descriptor
		    tc:*encauth:ocb3* "OCB3"
		    ocb3-start
		    ocb3-encrypt ocb3-encrypt
		    ocb3-decrypt ocb3-decrypt
		    ocb3-done #f #f
		    default-last-block-size
		    ocb3-state-tag-len
		    ocb3-compute-tag! ocb3-verify-tag!
		    tc:ocb3-add-aad! #f))

;; GCM
(define (gcm-start cipher key parameter)
  (define iv (cipher-parameter-iv parameter))
  (define aad (cipher-parameter-aad parameter #f))
  (let ((state (tc:gcm-init (block-cipher-descriptor-cipher cipher) key)))
    (tc:gcm-add-iv! state iv)
    (when aad (tc:gcm-add-aad! state aad))
    state))

;; using EAX wrongly, but that's users' responsiblity
(define (gcm-done state) (tc:gcm-done! state #vu8()))
;; Don't accept any plain text
(define (gcm-compute-tag! state tag start)
  ;; returns the tag length
  (tc:gcm-done! state tag start))
(define (gcm-verify-tag! state tag start)
  (let* ((len (- (bytevector-length tag) start))
	 (tmp (make-bytevector len)))
    (tc:gcm-done! state tmp 0)
    (let ((same? (safe-bytevector=? tag tmp start 0 len))
	  ;; GCM tag can be 128, 120, 112, 104, 96, 64 and 32
	  ;; 64 and 32 are not recommended, but we accepts
	  (valid-length? (or (<= 12 len 16) (= len 8) (= len 4))))
      (unless (and same? valid-length?)
	(error 'mode-validate-tag! "Tag unmatched")))))

(define *mode:gcm* (make-encauth-mode-descriptor
		    tc:*encauth:gcm* "GCM"
		    gcm-start
		    tc:gcm-encrypt! tc:gcm-encrypt!
		    tc:gcm-decrypt! tc:gcm-decrypt!
		    gcm-done #f #f
		    default-last-block-size
		    (lambda (state) 16)
		    gcm-compute-tag! gcm-verify-tag!
		    tc:gcm-add-aad! tc:gcm-add-iv!))

)
