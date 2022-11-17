;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rsa/pkcs/%3a8 - PKCS#8
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
;; - https://datatracker.ietf.org/doc/html/rfc5208
;; - https://datatracker.ietf.org/doc/html/rfc5958
#!nounbound
(library (rsa pkcs :8)
    (export (rename (<one-asymmetric-key> <private-key-info>))
	    one-asymmetric-key?
	    private-key-info?
	    make-private-key-info
	    
	    <encrypted-private-key-info>
	    encrypted-private-key-info?
	    make-encrypted-private-key-info
	    (rename (encrypted-private-key-info-encryption-algorithm
		     encrypted-private-key-info-id)
		    (encrypted-private-key-info-encrypted-data
		     encrypted-private-key-info-data))

	    <pkcs8-private-key>
	    pkcs8-private-key?
	    (rename (one-asymmetric-key->private-key
		     private-key-info->private-key)
		    (one-asymmetric-key->private-key
		     pki->private-key))
	    pkcs8-private-key->private-key

	    PKCS8
	    import-private-key export-private-key
	    generate-private-key
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys)
	    (sagittarius crypto pkcs keys)
	    (sagittarius crypto pkcs modules akp)
	    (sagittarius crypto pkix modules x509))

(define-generic make-private-key-info)
(define-method make-private-key-info ((s <asn1-collection>))
  (asn1-object->asn1-encodable <one-asymmetric-key> s))
(define-method make-private-key-info ((bv <bytevector>))
  (bytevector->asn1-encodable <one-asymmetric-key> bv))
(define-method make-private-key-info ((id <algorithm-identifier>)
				      (key <private-key>))
  (bytevector->asn1-encodable
   <one-asymmetric-key>
   (export-private-key key (private-key-format private-key-info))))
  
(define-method make-private-key-info ((key <private-key>))
  (bytevector->asn1-encodable
   <one-asymmetric-key>
   (export-private-key key (private-key-format private-key-info))))

(define-generic make-encrypted-private-key-info)
(define-method make-encrypted-private-key-info ((s <asn1-collection>))
  (asn1-object->asn1-encodable <encrypted-private-key-info> s))
(define-method make-encrypted-private-key-info ((bv <bytevector>))
  (bytevector->asn1-encodable <encrypted-private-key-info> bv))
(define-method make-encrypted-private-key-info ((id <algorithm-identifier>)
						(data <bytevector>))
  (make <encrypted-private-key-info>
    :encryption-algorithm id
    :encrypted-data (bytevector->der-octet-string data)))

;; PKCS8 private key
(define-class <pkcs8-private-key> (<private-key>)
  ((private-key-info :init-keyword :private-key-info)))
(define (pkcs8-private-key? o) (is-a? o <pkcs8-private-key>))
(define PKCS8 :pkcs8)
  ;; hmm kinda silly without cipher...
(define-method generate-private-key ((m (eql PKCS8)) (pki <one-asymmetric-key>))
  (make <pkcs8-private-key> :private-key-info pki))

(define-method import-private-key ((m (eql PKCS8)) (in <bytevector>))
  (import-private-key PKCS8 (open-bytevector-input-port in)))
(define-method import-private-key ((m (eql PKCS8)) (in <port>))
  (import-private-key PKCS8 (read-asn1-object in)))
(define-method import-private-key ((m (eql PKCS8)) (in <asn1-collection>))
  (make-private-key-info in))

(define-method export-private-key ((m (eql PKCS8)) (in <one-asymmetric-key>))
  (export-private-key in))

(define (pkcs8-private-key->private-key k)
  (one-asymmetric-key->private-key (slot-ref k 'private-key-info)))
)
