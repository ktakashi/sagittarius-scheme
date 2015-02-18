;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/keystore.scm - Generic interface for keystore
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

(library (security keystore)
    (export load-keystore load-keystore-file make-keystore
	    <keystore> keystore?
	    keystore-get-key
	    keystore-get-certificate
	    keystore-get-certificate-chain
	    keystore-get-creation-date
	    keystore-contains-alias?

	    store-keystore
	    store-keystore-to-file
	    
	    keystore-set-key!
	    keystore-set-certificate!
	    keystore-delete-entry!
	    )
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (security keystore interface))

  (define (load-keystore type in password)
    ;; TODO may want to implement forward thing?
    ;;      such as `keystore-type` procedure per library to return
    ;;      actual type?
    (let ((name (string->symbol (format "load-~a-keystore" type))))
      (eval `(,name ,in ,password)
	    (environment `(security keystore ,type)))))

  (define (load-keystore-file type file password)
    (call-with-input-file file
      (lambda (in) (load-keystore type in password))
      :transcoder #f))

  (define (make-keystore type)
    ;; Ditto
    (let ((name (string->symbol (format "make-~a-keystore" type))))
      (eval `(,name)
	    (environment `(security keystore ,type)))))

  )
