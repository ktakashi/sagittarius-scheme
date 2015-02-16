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

(library (rsa pkcs :8)
    (export <private-key-info>
	    private-key-info?
	    make-private-key-info
	    
	    <encrypted-private-key-info>
	    encrypted-private-key-info?
	    make-encrypted-private-key-info

	    <pkcs8-private-key>
	    pkcs8-private-key?
	    pki->private-key
	    pkcs8-private-key->private-key
	    )
    (import (rnrs)
	    (clos user)
	    (asn.1)
	    (crypto)
	    (rsa pkcs :10) ;; for algorithm-identifier
	    )

  (define dsa-oid (make-der-object-identifier "1.2.840.10040.4.1"))
  #|
      PrivateKeyInfo ::= SEQUENCE {
        version                   Version,
        privateKeyAlgorithm       PrivateKeyAlgorithmIdentifier,
        privateKey                PrivateKey,
        attributes           [0]  IMPLICIT Attributes OPTIONAL }

      Version ::= INTEGER

      PrivateKeyAlgorithmIdentifier ::= AlgorithmIdentifier

      PrivateKey ::= OCTET STRING

      Attributes ::= SET OF Attribute
  |#
  (define-class <private-key-info> (<asn.1-encodable>)
    ((id :init-keyword :id)
     (attributes :init-keyword :attributes)
     (private-key :init-keyword :private-key)))
  (define (private-key-info? o) (is-a? o <private-key-info>))
  (define-method make-private-key-info ((s <asn.1-sequence>))
    (unless (zero? (der-integer->integer (asn.1-sequence-get s 0)))
      (assertion-violation 'make-private-key-info
			   "wrong version for private key info"))
    (let* ((id (make-algorithm-identifier (asn.1-sequence-get s 1)))
	   (ain (open-bytevector-input-port
		 (slot-ref (asn.1-sequence-get s 2) 'string)))
	   (priv-key (read-asn.1-object ain))
	   (attributes (if (> (asn.1-sequence-size s) 3)
			   (make-der-set (asn.1-sequence-get s 3))
			   #f)))
      (make <private-key-info> :id id :attributes attributes 
	    :private-key priv-key)))
  (define-method make-private-key-info ((id <algorithm-identifier>)
					(key <private-key>))
    (make <private-key-info> :id id :attributes #f
	  :private-key (export-private-key key)))
  (define-method asn.1-encodable->asn.1-object ((o <private-key-info>))
    (make-der-sequence
     (make-der-integer 0)
     (slot-ref o 'id)
     (make-der-octet-string (slot-ref o 'private-key))))
  #|
      EncryptedPrivateKeyInfo ::= SEQUENCE {
        encryptionAlgorithm  EncryptionAlgorithmIdentifier,
        encryptedData        EncryptedData }

      EncryptionAlgorithmIdentifier ::= AlgorithmIdentifier

      EncryptedData ::= OCTET STRING
  |#
  (define-class <encrypted-private-key-info> (<asn.1-encodable>)
    ((id   :init-keyword :id)
     (data :init-keyword :data)))
  (define (encrypted-private-key-info? o) 
    (is-a? o <encrypted-private-key-info>))
  (define-method make-encrypted-private-key-info ((s <asn.1-sequence>))
    (make <encrypted-private-key-info>
      :id (make-algorithm-identifier (asn.1-sequence-get s 0))
      :data (asn.1-sequence-get s 1)))
  (define-method make-encrypted-private-key-info ((id <algorithm-identifier>)
						  (data <bytevector>))
    (make <encrypted-private-key-info> :id id 
	  :data (make-der-octet-string data)))
  (define-method asn.1-encodable->asn.1-object 
    ((o <encrypted-private-key-info>))
    (make-der-sequence (slot-ref o 'id) (slot-ref o 'data)))
  (define-method write-object ((o <encrypted-private-key-info>) (p <port>))
    (format p "#<encrypted-private-key-info~%~a~%~a>"
	    (slot-ref o 'id)
	    (slot-ref o 'data)))

  ;; PKCS8 private key
  (define-class <pkcs8-private-key> (<private-key>)
    ((private-key-info :init-keyword :private-key-info)))
  (define (pkcs8-private-key? o) (is-a? o <pkcs8-private-key>))
  (define PKCS8 :pkcs8)
  ;; hmm kinda silly without cipher...
  (define-method generate-private-key ((m (eql PKCS8)) (pki <private-key-info>))
    (make <pkcs8-private-key> :private-key-info pki))
  
  ;; for now we only support RSA
  ;; FIXME kinda silly
  (define *oid-marker*
    `(("1.2.840.113549.1.1.1" . ,RSA)))

  (define (pki->private-key pki)
    (let ((oid (algorithm-identifier-id (slot-ref pki 'id))))
      (cond ((assoc oid *oid-marker*) =>
	     (lambda (m) 
	       (import-private-key (cdr m) (slot-ref pki 'private-key))))
	    (else
	     (assertion-violation 'pki->private-key "not supported" oid)))))
  (define (pkcs8-private-key->private-key k)
    (pki->private-key (slot-ref k 'private-key-info)))
)
