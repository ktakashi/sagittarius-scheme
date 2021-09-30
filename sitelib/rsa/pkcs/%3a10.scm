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
	    algorithm-identifier-object-id ;; der-object-identifier
	    algorithm-identifier-parameters

	    PKCS10
	    import-public-key export-public-key
	    )
    (import (rnrs)
	    (clos user)
	    (asn.1)
	    (crypto)
	    (sagittarius)
	    (sagittarius object))

  ;; TODO other ASN1 modules.

  #|
   SubjectPublicKeyInfo {ALGORITHM: IOSet} ::= SEQUENCE {
        algorithm        AlgorithmIdentifier {{IOSet}},
        subjectPublicKey BIT STRING
   }
  |#
  (define-class <subject-public-key-info> (<asn.1-encodable>)
    ((algorithm-identifier :init-keyword :algorithm-identifier)
     (key-data :init-keyword :key-data)))
  (define (subject-public-key-info? o) (is-a? o <subject-public-key-info>))
  (define-generic make-subject-public-key-info)
  (define-method make-subject-public-key-info ((bv <bytevector>))
    (make-subject-public-key-info (open-bytevector-input-port bv)))
  (define-method make-subject-public-key-info ((port <port>))
    (make-subject-public-key-info (read-asn.1-object port)))
  (define-method make-subject-public-key-info ((s <asn.1-sequence>))
    ;; TODO check length
    (let ((id (asn.1-sequence-get s 0)))
      (make <subject-public-key-info>
	:algorithm-identifier (if (algorithm-identifier? id)
				  id
				  (make-algorithm-identifier id))
	:key-data (asn.1-sequence-get s 1))))
  (define-method make-subject-public-key-info ((pk <rsa-public-key>))
    (make-subject-public-key-info
     (make-der-sequence
      (make-algorithm-identifier "1.2.840.113549.1.1.1" (make-der-null))
      (make-der-bit-string (export-public-key pk)))))
  (define-method make-subject-public-key-info ((pk <ecdsa-public-key>))
    ;; for some weird reason, ecdsa key exports SPKI form, so use it
    (make-subject-public-key-info (export-public-key pk)))
  (define-method make-subject-public-key-info ((pk <eddsa-public-key>))
    (make-subject-public-key-info
     (make-der-sequence
      (make-algorithm-identifier
       (if (ed25519-key? pk) "1.3.101.112" "1.3.101.113"))
      (make-der-bit-string (export-public-key pk)))))
  (define-method asn.1-encodable->asn.1-object ((o <subject-public-key-info>))
    (make-der-sequence
     (asn.1-encodable->asn.1-object (slot-ref o 'algorithm-identifier))
     (asn.1-encodable->asn.1-object (slot-ref o 'key-data))))
  (define (subject-public-key-info-key-data o) (~ o 'key-data))
  
  #|
   AlgorithmIdentifier {ALGORITHM:IOSet } ::= SEQUENCE {
        algorithm  ALGORITHM.&id({IOSet}),
        parameters ALGORITHM.&Type({IOSet}{@algorithm}) OPTIONAL
   }
  |#
  (define-class <algorithm-identifier> (<asn.1-encodable>)
    ((object-id  :init-keyword :object-id
		 :reader algorithm-identifier-object-id)
     (parameters :init-keyword :parameters :init-value #f
		 :reader algorithm-identifier-parameters)))
  (define (algorithm-identifier? o) (is-a? o <algorithm-identifier>))
  (define-generic make-algorithm-identifier)
  (define-method make-algorithm-identifier ((o <der-object-identifier>))
    (make <algorithm-identifier> :object-id o))
  (define-method make-algorithm-identifier ((s <asn.1-sequence>))
    (let ((len (asn.1-sequence-size s)))
      (unless (<= 1 len 2)
	(assertion-violation 'make-algorithm-identifier
			     "bad sequence size" len))
      (if (= len 2)
	  (make <algorithm-identifier>
	    :object-id (asn.1-sequence-get s 0)
	    :parameters (asn.1-sequence-get s 1))
	  (make <algorithm-identifier>
	    :object-id (asn.1-sequence-get s 0)))))
  (define-method make-algorithm-identifier ((oid <string>)
					    (param <asn.1-encodable>))
    (make <algorithm-identifier> 
      :object-id (make-der-object-identifier oid)
      :parameters param))
  (define-method make-algorithm-identifier ((oid <string>))
    (make <algorithm-identifier> :object-id (make-der-object-identifier oid)))
  (define-method asn.1-encodable->asn.1-object ((o <algorithm-identifier>))
    (let ((p (slot-ref o 'parameters)))
      (if p
	  (make-der-sequence (slot-ref o 'object-id) p)
	  (make-der-sequence (slot-ref o 'object-id)))))

  (define-method write-object ((o <algorithm-identifier>) (p <port>))
    (format p "#<algorithm-identifier ~a~%~a>" (algorithm-identifier-id o)
	    (slot-ref o 'parameters)))

  (define (algorithm-identifier-id id) (~ id 'object-id 'identifier))

  (define PKCS10 :pkcs10)
  (define-method import-public-key ((m (eql PKCS10)) (in <bytevector>))
    (import-public-key PKCS10 (open-bytevector-input-port in)))
  (define-method import-public-key ((m (eql PKCS10)) (in <port>))
    (import-public-key PKCS10 (read-asn.1-object in)))
  (define-method import-public-key ((m (eql PKCS10)) (in <asn.1-sequence>))
    (make-subject-public-key-info in))

  (define-method export-public-key ((m (eql PKCS10))
				    (in <subject-public-key-info>))
    (export-public-key in))
  (define-method export-public-key ((in <subject-public-key-info>))
    (asn.1-encode (asn.1-encodable->asn.1-object in)))

  ;; FIXME loads of duplicates....
  (define (pki->rsa-public-key pki)
    (import-public-key RSA (slot-ref (slot-ref pki 'key-data) 'data)))
  (define (pki->dsa-public-key pki)
    (import-public-key DSA (slot-ref (slot-ref pki 'key-data) 'data)))
  (define *oid-marker*
    `(("1.2.840.113549.1.1.1" . ,pki->rsa-public-key)
      ("1.2.840.113549.1.1.2" . ,pki->rsa-public-key)
      ("1.2.840.113549.1.1.3" . ,pki->rsa-public-key)
      ("1.2.840.113549.1.1.4" . ,pki->rsa-public-key)
      ("1.2.840.113549.1.1.5" . ,pki->rsa-public-key)
      ("1.2.840.113549.1.1.7" . ,pki->rsa-public-key)
      ("1.2.840.113549.1.1.10" . ,pki->rsa-public-key)
      ("1.2.840.113549.1.1.11" . ,pki->rsa-public-key)
      ("1.2.840.113549.1.1.12" . ,pki->rsa-public-key)
      ("2.5.8.1.1" . ,pki->rsa-public-key)
      ("1.2.840.10040.4.1" . ,pki->dsa-public-key)
      ("1.2.840.10040.4.3" . ,pki->dsa-public-key)
      ("1.2.840.10045.2.1" .
       ,(lambda (pki)
	  ;; For some reason, old me thought importing ECPublicKey
	  ;; from SubjectPublicKeyInfo directly in the (crypto ecdsa)
	  ;; so make the pki ASN.1 object
	  (import-public-key ECDSA (asn.1-encodable->asn.1-object pki))))
      
      ;; RFC 8410
      ;; TODO X25519 and X448
      ("1.3.101.112" .
       ,(lambda (pki)
	  (import-public-key Ed25519 (slot-ref (slot-ref pki 'key-data) 'data))))
      ("1.3.101.113" .
       ,(lambda (pki)
	  (import-public-key Ed448 (slot-ref (slot-ref pki 'key-data) 'data))))
      ))
			     
  (define (subject-public-key-info->public-key spki)
    (let ((oid (algorithm-identifier-id (slot-ref spki 'algorithm-identifier))))
      (cond ((assoc oid *oid-marker*) => (lambda (s) ((cdr s) spki)))
	    (else (assertion-violation 'subject-public-key-info->public-key
				       "not supported" oid)))))
)
