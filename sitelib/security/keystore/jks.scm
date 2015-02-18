;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/keystore/jks.scm - JKS
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

(library (security keystore jks)
    (export <jks-keystore> jks-keystore? make-jks-keystore
	    <jceks-keystore> jceks-keystore?

	    ;; load
	    load-jks-keystore
	    load-jks-keystore-file
	    store-jks-keystore
	    store-jks-keystore-to-file
	    ;; get
	    jks-keystore-get-key
	    jks-keystore-get-certificate
	    jks-keystore-get-certificate-chain
	    jks-keystore-get-creation-date
	    jks-keystore-contains-alias?

	    ;; set
	    jks-keystore-set-key!
	    jks-keystore-set-certificate!
	    )
    (import (rnrs)
	    (clos user)
	    (security keystore jceks keystore)
	    (security keystore interface))

  (define-class <jks-keystore> (<base-jceks-keystore>) ())

  (define (jks-keystore? o) (is-a? o <jks-keystore>))
  (define (make-jks-keystore) (make <jks-keystore>))

  (define load-jks-keystore 
    (generate-load-jceks-key-store <jks-keystore> '(#xfeedfeed)))
  (define (load-jks-keystore-file file password)
    (call-with-input-file file
      (lambda (in) (load-jks-keystore in password))
      :transcoder #f))
  (define store-jks-keystore
    (generate-store-jceks-keystore jks-keystore? #xfeedfeed))
  (define (store-jks-keystore-to-file keystore file password)
    (call-with-output-file file
      (lambda (out) (store-jks-keystore keystore out password))
      :transcoder #f))
  (define jks-keystore-get-key (generate-jceks-get-key jks-keystore? #f))
  (define jks-keystore-get-certificate 
    (generate-jceks-get-certificate jks-keystore?))
  (define jks-keystore-contains-alias? 
    (generate-jceks-contains-alias? jks-keystore?))
  (define jks-keystore-get-certificate-chain
    (generate-jceks-get-certificate-chain jks-keystore?))
  (define jks-keystore-get-creation-date
    (generate-jceks-get-creation-date jks-keystore?))
  (define jks-keystore-set-key! (generate-jceks-set-key! jks-keystore? #f))
  (define jks-keystore-set-certificate!
    (generate-jceks-set-certificate! jks-keystore?))


  (define-method keystore-get-key ((ks <jks-keystore>) alias password)
    (jks-keystore-get-key ks alias password))
  (define-method keystore-get-certificate ((ks <jks-keystore>) alias)
    (jks-keystore-get-certificate ks alias))
  (define-method keystore-get-certificate-chain ((ks <jks-keystore>) alias)
    (jks-keystore-get-certificate-chain ks alias))
  (define-method keystore-contains-alias? ((ks <jks-keystore>) alias)
    (jks-keystore-contains-alias? ks alias))

  (define-method store-keystore ((ks <jks-keystore>) out password)
    (store-jks-keystore ks out password))

  (define-method keystore-set-key! ((ks <jks-keystore>) alias key pw certs)
    (jks-keystore-set-key! ks alias key pw certs))
  (define-method keystore-set-certificate! ((ks <jks-keystore>) alias cert)
    (jks-keystore-set-certificate! ks alias cert))
  (define-method keystore-delete-entry! ((ks <jks-keystore>) alias)
    (jks-keystore-delete-entry! ks alias))


)
