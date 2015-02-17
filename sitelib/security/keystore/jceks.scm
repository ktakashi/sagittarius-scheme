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

(library (security keystore jceks)
    (export <jceks-keystore> jceks-keystore? make-jceks-keystore
	    load-jceks-keystore
	    load-jceks-keystore-file
	    store-jceks-keystore
	    store-jceks-keystore-to-file
	    jceks-keystore-get-key
	    jceks-keystore-get-certificate
	    jceks-keystore-get-certificate-chain
	    jceks-keystore-get-creation-date

	    jceks-keystore-set-key!
	    jceks-keystore-set-certificate!)
    (import (rnrs)
	    (clos user)
	    (security keystore jceks keystore)
	    (security keystore interface))


  (define-class <jceks-keystore> (<base-jceks-keystore>) ())
  (define (jceks-keystore? o) (is-a? o <jceks-keystore>))
  (define (make-jceks-keystore) (make <jceks-keystore>))

  (define load-jceks-keystore 
    (generate-load-jceks-key-store <jceks-keystore> '(#xcececece #xfeedfeed)))
  (define (load-jceks-keystore-file file password)
    (call-with-input-file file
      (lambda (in) (load-jceks-keystore in password))
      :transcoder #f))
  ;; should we accept JKS keystore as well?
  ;; (we can do with checking <base-jceks-keystore>)
  (define store-jceks-keystore
    (generate-store-jceks-keystore jceks-keystore? #xcececece))
  (define (store-jceks-keystore-to-file keystore file password)
    (call-with-output-file file
      (lambda (out) (store-jceks-keystore keystore out password))
      :transcoder #f))

  (define jceks-keystore-get-key (generate-jceks-get-key jceks-keystore? #t))
  (define jceks-keystore-get-certificate 
    (generate-jceks-get-certificate jceks-keystore?))
  (define jceks-keystore-get-certificate-chain
    (generate-jceks-get-certificate-chain jceks-keystore?))
  (define jceks-keystore-get-creation-date
    (generate-jceks-get-creation-date jceks-keystore?))
  (define jceks-keystore-set-key! (generate-jceks-set-key! jceks-keystore? #t))
  (define jceks-keystore-set-certificate!
    (generate-jceks-set-certificate! jceks-keystore?))

  (define-method keystore-get-key ((ks <jceks-keystore>) alias password)
    (jceks-keystore-get-key ks alias password))
  (define-method keystore-get-certificate ((ks <jceks-keystore>) alias)
    (jceks-keystore-get-certificate ks alias))
  (define-method keystore-get-certificate-chain ((ks <jceks-keystore>) alias)
    (jceks-keystore-get-certificate-chain ks alias))

  (define-method store-keystore ((ks <jceks-keystore>) out password)
    (store-jceks-keystore ks out password))

  (define-method keystore-set-key! ((ks <jceks-keystore>) alias key pw certs)
    (jceks-keystore-set-key! ks alias key pw certs))
  (define-method keystore-set-certificate! ((ks <jceks-keystore>) alias cert)
    (jceks-keystore-set-certificate! ks alias cert))
  (define-method keystore-delete-entry! ((ks <jceks-keystore>) alias)
    (jceks-keystore-delete-entry! ks alias))

  )
