;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/algorithms.scm - X.509 algorithm identifier
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
(library (sagittarius crypto pkix algorithms)
    (export x509-algorithm-identifier? <x509-algorithm-identifier>
	    make-x509-algorithm-identifier
	    x509-algorithm-identifier-oid
	    x509-algorithm-identifier-parameters
	    algorithm-identifier->x509-algorithm-identifier
	    x509-algorithm-identifier->algorithm-identifier

	    ;; For extension, bad names though...
	    algorithm-parameters->x509-algorithm-parameters
	    x509-algorithm-parameters->algorithm-parameters
	    x509-algorithm-parameters->keyword-parameters

	    x509-algorithm-parameters? <x509-algorithm-parameters>
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius mop immutable))

;; Should be somewhere else
(define-class <x509-algorithm-parameters> () ())
(define (x509-algorithm-parameters? o) (is-a? o <x509-algorithm-parameters>))

(define-class <x509-algorithm-identifier> (<immutable>)
  ((oid :init-keyword :oid :reader x509-algorithm-identifier-oid)
   (parameters :init-keyword :parameters
	       :reader x509-algorithm-identifier-parameters)))
(define-method write-object ((o <x509-algorithm-identifier>) p)
  (format p "#<x509-algorithm-identifier OID=~a>"
	  (x509-algorithm-identifier-oid o)))

(define (x509-algorithm-identifier? o) (is-a? o <x509-algorithm-identifier>))
(define (make-x509-algorithm-identifier (oid object-identifier-string?)
	 :optional ((parameters (or #f x509-algorithm-parameters?)) #f))
  (make <x509-algorithm-identifier> :oid oid :parameters parameters))

(define (algorithm-identifier->x509-algorithm-identifier
	 (aid algorithm-identifier?))
  (let ((oid (der-object-identifier->oid-string
	      (algorithm-identifier-algorithm aid))))
    (make <x509-algorithm-identifier>
      :oid oid
      :parameters (algorithm-parameters->x509-algorithm-parameters oid
		   (algorithm-identifier-parameters aid)))))
(define (x509-algorithm-identifier->algorithm-identifier
	 (aid x509-algorithm-identifier?))
  (make <algorithm-identifier>
    :algorithm (oid-string->der-object-identifier
		(x509-algorithm-identifier-oid aid))
    :parameters (x509-algorithm-parameters->algorithm-parameters
		 (x509-algorithm-identifier-parameters aid))))

(define (x509-algorithm-identifier->oid&parameters
	 (algorithm-identifier x509-algorithm-identifier?))
  (let ((oid (x509-algorithm-identifier-oid algorithm-identifier))
	(param (x509-algorithm-identifier-parameters algorithm-identifier)))
    (values oid param)))

(define-generic algorithm-parameters->x509-algorithm-parameters)
(define-generic x509-algorithm-parameters->algorithm-parameters)
(define-generic x509-algorithm-parameters->keyword-parameters)

(define-method algorithm-parameters->x509-algorithm-parameters (o p) #f)
(define-method x509-algorithm-parameters->algorithm-parameters (o) #f)
(define-method x509-algorithm-parameters->keyword-parameters (o) '())
)
