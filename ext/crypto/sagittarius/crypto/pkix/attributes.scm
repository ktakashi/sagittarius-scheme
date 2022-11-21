;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/attributes.scm - PKIX attributes
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
(library (sagittarius crypto pkix attributes)
    (export x509-attribute? <x509-attribute>
	    x509-attribute-type
	    x509-attribute-values
	    x509-attribute-of
	    attributes->x509-attributes
	    attribute->x509-attribute
	    x509-attribute->attribute)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509))
(define (oid? o) (or (der-object-identifier? o) (object-identifier-string? o)))
(define-class <x509-attribute> (<immutable>)
  ((type :init-keyword :type :reader x509-attribute-type)
   (values :init-keyword :values :reader x509-attribute-values)))
(define-method write-object ((o <x509-attribute>) p)
  (format p "#<x509-attribute id=~a values=~a>"
	  (x509-attribute-type o)
	  (x509-attribute-values o)))
(define (x509-attribute? o) (is-a? o <x509-attribute>))
(define (attribute->x509-attribute attribute)
  (make <x509-attribute>
    :type (der-object-identifier->oid-string (attribute-type attribute))
    :values (attribute-values attribute)))
(define (x509-attribute->attribute attribute)
  (make <attribute>
    :type (oid-string->der-object-identifier (x509-attribute-type attribute))
    :values (x509-attribute-values attribute)))

(define (x509-attribute-of (oid/string oid?))
  (let ((oid (if (der-object-identifier? oid/string)
		 (der-object-identifier->oid-string oid/string)
		 oid/string)))
    (lambda (attribute)
      (and (x509-attribute? attribute)
	   (equal? oid (x509-attribute-type attribute))))))

(define (attributes->x509-attributes (attributes attributes?))
  (map attribute->x509-attribute (attributes->list attributes)))
)
