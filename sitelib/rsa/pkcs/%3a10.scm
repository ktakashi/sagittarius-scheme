;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rsa/pkcs/%3a10 - PKCS#10
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

;; ref
;; - https://datatracker.ietf.org/doc/html/rfc2986
;; - https://datatracker.ietf.org/doc/html/rfc3279
;; - https://datatracker.ietf.org/doc/html/rfc8410
#!nounbound
(library (rsa pkcs :10)
    (export <subject-public-key-info>
	    subject-public-key-info?
	    make-subject-public-key-info
	    subject-public-key-info-key-data
	    subject-public-key-info->public-key
	    
	    <algorithm-identifier>
	    algorithm-identifier?
	    make-algorithm-identifier
	    algorithm-identifier-id ;; returns OID string
	    ;; der-object-identifier
	    (rename (algorithm-identifier-algorithm
		     algorithm-identifier-object-id))
	    algorithm-identifier-parameters

	    PKCS10
	    import-public-key export-public-key
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys)
	    (sagittarius crypto pkix keys)
	    (sagittarius crypto pkix modules x509))

(define-generic make-subject-public-key-info)
(define-method make-subject-public-key-info ((bv <bytevector>))
  (make-subject-public-key-info (open-bytevector-input-port bv)))
(define-method make-subject-public-key-info ((port <port>))
  (make-subject-public-key-info (read-asn1-object port)))
(define-method make-subject-public-key-info ((s <asn1-collection>))
  (asn1-object->asn1-encodable <subject-public-key-info> s))
(define-method make-subject-public-key-info ((pk <public-key>))
  (make-subject-public-key-info
   (export-public-key pk (public-key-format subject-public-key-info))))
(define (subject-public-key-info-key-data o)
  (subject-public-key-info-subject-public-key o))

(define-generic make-algorithm-identifier)
(define-method make-algorithm-identifier ((o <der-object-identifier>))
  (make <algorithm-identifier> :algorithm o))
(define-method make-algorithm-identifier ((s <asn1-collection>))
  (asn1-object->asn1-encodable <algorithm-identifier> s))

(define-method make-algorithm-identifier ((oid <string>)
					  (param <asn1-encodable>))
  (make <algorithm-identifier> 
    :algorithm (oid-string->der-object-identifier oid)
    :parameters param))
(define-method make-algorithm-identifier ((oid <string>))
  (make-algorithm-identifier (oid-string->der-object-identifier oid)))

(define (algorithm-identifier-id id)
  (der-object-identifier->oid-string
   (algorithm-identifier-algorithm id)))

(define PKCS10 :pkcs10)
(define-method import-public-key ((m (eql PKCS10)) in)
  (make-subject-public-key-info in))

(define-method export-public-key ((m (eql PKCS10))
				  (in <subject-public-key-info>))
  (export-public-key in))

)
