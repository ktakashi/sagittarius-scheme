;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/crypto/ciphers.scm - SSH2 cryptography
;;;  
;;;   Copyright (c) 2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh crypto ciphers)
    (export make-ssh-cipher
	    ssh-cipher-descriptor)
    (import (rnrs)
	    (clos user)
	    (rfc ssh types)
	    (srfi :13 strings)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto keys))

(define (make-ssh-cipher name direction key-retriever iv-retriever)
  (let-values (((desc mode) (ssh-cipher-descriptor name)))
    ;; for now only block cipher
    (let ((key-size (block-cipher-descriptor-suggested-key-length desc))
	  (block-size (block-cipher-descriptor-block-length desc)))
      (block-cipher-init!
       (make-block-cipher desc mode no-padding)
       direction
       (generate-symmetric-key desc (key-retriever key-size))
       (make-cipher-parameter
	(make-iv-parameter (iv-retriever block-size))
	(make-counter-mode-parameter *ctr-mode:big-endian*))))))

(define-generic ssh-cipher-descriptor :class <predicate-specializable-generic>)

(define (aes? s) (string-prefix? "aes" s))
(define (extract-mode n)
  (cond ((string-suffix? "cbc" n) *mode:cbc*)
	((string-suffix? "ctr" n) *mode:ctr*)
	;; TODO counter mode
	(else (error 'ssh-cipher-descriptor "mode not supported" n))))
(define-method ssh-cipher-descriptor ((name (?? aes?)))
  (let ((mode (extract-mode name)))
    ;; I don't dare to check other AES name...
    (cond ((string-prefix? "aes128" name) (values *scheme:aes-128* mode))
	  ((string-prefix? "aes192" name) (values *scheme:aes-192* mode))
	  ((string-prefix? "aes256" name) (values *scheme:aes-256* mode)))))

(define (des? s) (string-prefix? "3des" s))
(define-method ssh-cipher-descriptor ((name (?? des?)))
  (values *scheme:desede* (extract-mode name)))

(define (blowfish? s) (string-prefix? "blowfish" s))
(define-method ssh-cipher-descriptor ((name (?? blowfish?)))
  (values *scheme:blowfish* (extract-mode name)))

)
