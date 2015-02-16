;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/keystore/pkcs12.scm - Generic interface for PKCS#12
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

(library (security keystore pkcs12)
    (export load-pkcs12-keystore
	    make-pkcs12-keystore)
    (import (rnrs)
	    (clos user)
	    (rsa pkcs :12)
	    (security keystore interface))

  (define-method keystore-get-key ((ks <pkcs12-keystore>) alias password)
    (pkcs12-keystore-get-key ks alias))
  (define-method keystore-get-certificate ((ks <pkcs12-keystore>) alias)
    (pkcs12-keystore-get-certificate ks alias))
  (define-method keystore-get-certificate-chain ((ks <pkcs12-keystore>) alias)
    (pkcs12-keystore-get-certificate-chain ks alias))

  (define-method store-keystore ((ks <pkcs12-keystore>) out password)
    (store-pkcs12-keystore ks out password))

  (define-method keystore-set-key! ((ks <pkcs12-keystore>) alias key pw certs)
    (pkcs12-keystore-set-key! ks alias key certs))
  (define-method keystore-set-certificate! ((ks <pkcs12-keystore>) alias cert)
    (pkcs12-keystore-set-certificate! ks alias cert))
  (define-method keystore-delete-entry! ((ks <pkcs12-keystore>) alias)
    (pkcs12-keystore-delete-entry! ks alias))
  
  )
