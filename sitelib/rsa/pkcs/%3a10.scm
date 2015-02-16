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

(library (rsa pkcs :10)
    (export <subject-public-key-info>
	    subject-public-key-info?
	    make-subject-public-key-info
	    subject-public-key-info-key-data

	    <algorithm-identifier>
	    algorithm-identifier?
	    make-algorithm-identifier
	    algorithm-identifier-id)
    (import (rnrs)
	    (clos user)
	    (asn.1)
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
  (define-method make-subject-public-key-info ((s <asn.1-sequence>))
    ;; TODO check length
    (let ((id (asn.1-sequence-get s 0)))
      (make <subject-public-key-info>
	:algorithm-identifier (if (algorithm-identifier? id)
				  id
				  (make-algorithm-identifier id))
	:key-data (asn.1-sequence-get s 1))))
  (define (subject-public-key-info-key-data o) (~ o 'key-data))
  
  #|
   AlgorithmIdentifier {ALGORITHM:IOSet } ::= SEQUENCE {
        algorithm  ALGORITHM.&id({IOSet}),
        parameters ALGORITHM.&Type({IOSet}{@algorithm}) OPTIONAL
   }
  |#
  (define-class <algorithm-identifier> (<asn.1-encodable>)
    ((object-id  :init-keyword :object-id)
     (parameters :init-keyword :parameters :init-value #f)
     (parameters-defined? :init-keyword :defined? :init-value #f)))
  (define (algorithm-identifier? o) (is-a? o <algorithm-identifier>))
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
	    :parameters (asn.1-sequence-get s 1)
	    :defined? #t)
	  (make <algorithm-identifier>
	    :object-id (asn.1-sequence-get s 0)))))
  (define-method make-algorithm-identifier ((oid <string>)
					    (param <asn.1-encodable>))
    (make <algorithm-identifier> 
      :object-id (make-der-object-identifier oid)
      :parameters param
      :defined? #t))
  (define-method asn.1-encodable->asn.1-object ((o <algorithm-identifier>))
    (make-der-sequence (slot-ref o 'object-id)
		       (if (slot-ref o 'parameters-defined?)
			   (slot-ref o 'parameters)
			   (make-der-null))))

  (define-method write-object ((o <algorithm-identifier>) (p <port>))
    (format p "#<algorithm-identifier ~a~%~a>" (algorithm-identifier-id o)
	    (slot-ref o 'parameters)))

  (define (algorithm-identifier-id id) (~ id 'object-id 'identifier))

)
