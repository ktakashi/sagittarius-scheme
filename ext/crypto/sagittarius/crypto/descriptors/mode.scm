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
	    
	    mode-descriptor? mode-descriptor-name
	    mode-descriptor-has-set-iv!?
	    mode-descriptor-has-get-iv!?

	    mode-key?
	    mode-start mode-encrypt! mode-decrypt! mode-done!
	    mode-set-iv! mode-get-iv!)
    (import (rnrs)
	    (prefix (sagittarius crypto tomcrypt) tc:)
	    (sagittarius crypto descriptors cipher)
	    (sagittarius crypto parameters))

;; Using record method dispatch, using CLOS seems cleaner but
;; it's extremely slower than the record method dispatch.
;; In general, we mustn't use CLOS in a frequently called procedure
;; NOTE: encauth has its own mode
(define-record-type mode-descriptor
  (fields mode ;; internal enum value, do we need?
	  name
	  start encrypt decrypt done set-iv get-iv))

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
(define (mode-decrypt! mode-key ct cs pt ps len)
  ((mode-descriptor-decrypt (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key) ct cs pt ps len))
(define (mode-done! mode-key)
  ((mode-descriptor-done (mode-key-descriptor mode-key))
   (mode-key-state-key mode-key)))
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
	 #'(make-mode-descriptor mode (symbol->string 'mode-name)
				 start encrypt decrypt done set-iv get-iv))))))
(define (ecb-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f)
	(cipher-descriptor-default-rounds cipher)))
  (tc:ecb-start (cipher-descriptor-cipher cipher) key rounds))
(define tc:ecb-set-iv! #f)
(define tc:ecb-get-iv! #f)

(define (cbc-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f)
	(cipher-descriptor-default-rounds cipher)))
  (define iv (cipher-parameter-iv parameter))
  (tc:cbc-start (cipher-descriptor-cipher cipher) iv key rounds))

(define (cfb-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f)
	(cipher-descriptor-default-rounds cipher)))
  (define iv (cipher-parameter-iv parameter))
  (tc:cfb-start (cipher-descriptor-cipher cipher) iv key rounds))

(define (ofb-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f)
	(cipher-descriptor-default-rounds cipher)))
  (define iv (cipher-parameter-iv parameter))
  (tc:ofb-start (cipher-descriptor-cipher cipher) iv key rounds))

(define (ctr-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f)
	(cipher-descriptor-default-rounds cipher)))
  (define iv (cipher-parameter-iv parameter))
  (define ctr-mode
    (cipher-parameter-counter-mode parameter tc:*ctr-mode:big-endian*))
  (tc:ctr-start (cipher-descriptor-cipher cipher) iv key rounds ctr-mode))

(define (lrw-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f)
	(cipher-descriptor-default-rounds cipher)))
  (define iv (cipher-parameter-iv parameter))
  (define tweak (cipher-parameter-tweak parameter #f))
  (tc:lrw-start (cipher-descriptor-cipher cipher) iv key tweak rounds))

(define (f8-start cipher key parameter)
  (define rounds
    (or (cipher-parameter-rounds parameter #f)
	(cipher-descriptor-default-rounds cipher)))
  (define iv (cipher-parameter-iv parameter))
  (define salt (cipher-parameter-salt parameter #f))
  (tc:f8-start (cipher-descriptor-cipher cipher) iv key salt rounds))

(define *mode:ecb* (build-mode-descriptor ecb))
(define *mode:cbc* (build-mode-descriptor cbc))
(define *mode:cfb* (build-mode-descriptor cfb))
(define *mode:ofb* (build-mode-descriptor ofb))
(define *mode:ctr* (build-mode-descriptor ctr))
(define *mode:lrw* (build-mode-descriptor lrw))
(define *mode:f8*  (build-mode-descriptor f8))

)
