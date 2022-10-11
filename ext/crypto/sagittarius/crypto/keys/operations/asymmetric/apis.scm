;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/operations/asymmetric/apis.scm - Asymmetric key op
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

#!nounbound
(library (sagittarius crypto keys operations asymmetric apis)
    (export generate-key-pair
	    generate-public-key
	    generate-private-key

	    import-public-key
	    import-private-key
	    export-public-key
	    export-private-key

	    oid->key-operation
	    
	    public-key-format
	    *public-key-formats*
	    public-key-format?

	    calculate-key-agreement
	    )
    (import (rnrs)
	    (sagittarius crypto asn1)
	    (clos user))

(define-generic generate-key-pair)
(define-generic generate-public-key)
(define-generic generate-private-key)
(define-generic import-public-key)
(define-generic import-private-key)
(define-generic export-public-key)
(define-generic export-private-key)

(define-generic oid->key-operation :class <one-of-specializable-generic>)

(define-generic calculate-key-agreement) ;; key agreement

(define-enumeration public-key-format (raw subject-public-key-info)
  public-key-formats)
(define *public-key-formats* (enum-set-universe (public-key-formats)))
(define (public-key-format? s) (enum-set-member? s *public-key-formats*))

(define-method import-public-key ((key <bytevector>)
				  (format (eq 'subject-public-key-info)))
  (import-public-key (open-bytevector-input-port key) format))
(define-method import-public-key ((key <port>)
				  (format (eq 'subject-public-key-info)))
  (import-public-key (read-asn1-object key) format))
#|
  SubjectPublicKeyInfo {PUBLIC-KEY: IOSet} ::= SEQUENCE {
      algorithm        AlgorithmIdentifier {PUBLIC-KEY, {IOSet}},
      subjectPublicKey BIT STRING
  }
|#
(define-method import-public-key ((key <der-sequence>)
				  (format (eq 'subject-public-key-info)))
  (let*-values (((aid ignore) (deconstruct-asn1-collection key))
		((oid . ignore) (deconstruct-asn1-collection aid)))
    (let ((s (oid->key-operation (der-object-identifier->oid-string oid))))
      (import-public-key s key format))))

;; TODO should we make private-key-format as well, which
;;      supports raw and private-key-info?
)
