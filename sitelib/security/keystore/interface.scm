;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/keystore/interface.scm - Keystore interface
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (security keystore interface)
    (export <keystore> keystore?
	    keystore-get-key
	    keystore-get-certificate
	    keystore-get-certificate-chain
	    keystore-get-creation-date

	    store-keystore
	    store-keystore-to-file
	    
	    keystore-set-key!
	    keystore-set-certificate!
	    keystore-delete-entry!
	    )
    (import (rnrs) (clos user) (srfi :19))

  ;; abstract class for keystore
  (define-class <keystore> () ())
  (define (keystore? o) (is-a? o <keystore>))
  
  (define-generic keystore-get-key)
  (define-generic keystore-get-certificate)
  (define-generic keystore-get-certificate-chain)
  (define-generic keystore-get-creation-date)
  (define-method keystore-get-creation-date ((ks <keystore>) alias)
    ;; default now
    (and (or (keystore-get-key ks alias)
	     (keystore-get-certificate ks alias))
	 (current-date)))

  (define-generic store-keystore)
  (define-generic store-keystore-to-file)
  (define-method store-keystore-to-file ((ks <keystore>) file password)
    (call-with-output-file file
      (lambda (out) (store-keystore ks out password))
      :transcoder #f))
  (define-generic keystore-set-key!)
  (define-generic keystore-set-certificate!)
  (define-generic keystore-delete-entry!)

)
