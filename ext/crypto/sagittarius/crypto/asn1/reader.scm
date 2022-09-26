;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/asn1/reader.scm - ASN.1 reader
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
(library (sagittarius crypto asn1 reader)
    (export read-asn1-object
	    bytevector->asn1-object)
    (import (rnrs)
	    (tlv)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1 tags)
	    (sagittarius crypto asn1 types)
	    (sagittarius crypto asn1 writer))

(define (create-der-bit-string bytes)
  ;; well well
  (let ((len (bytevector-length bytes)))
    (when (zero? len)
      (assertion-violation 'create-primitive-der-object
			   "truncated BIT STRING detected"))
    (let ((pad (bytevector-u8-ref bytes 0))
	  (data (bytevector-copy bytes 1)))
      (bytevector->der-bit-string data pad))))

(define *constructors*
  `((,*asn1:bit-string*	       . ,create-der-bit-string)
    (,*asn1:bmp-string*	       . ,bytevector->der-bmp-string)
    (,*asn1:boolean*   	       . ,bytevector->der-boolean)
    (,*asn1:enumerated*        . ,bytevector->der-enumerated)
    (,*asn1:generalized-time*  . ,bytevector->der-generalized-time)
    (,*asn1:general-string*    . ,bytevector->der-general-string)
    (,*asn1:ia5-string*        . ,bytevector->der-ia5-string)
    (,*asn1:integer*           . ,bytevector->der-integer)
    (,*asn1:null*              . ,(lambda (_) (make-der-null)))
    (,*asn1:numeric-string*    . ,bytevector->der-numeric-string)
    (,*asn1:object-identifier* . ,bytevector->der-object-identifier)
    (,*asn1:octet-string*      . ,bytevector->der-octet-string)
    (,*asn1:printable-string*  . ,bytevector->der-printable-string)
    (,*asn1:graphic-string*    . ,bytevector->der-graphic-string)
    (,*asn1:t61-string*        . ,bytevector->der-t61-string)
    (,*asn1:universal-string*  . ,bytevector->der-universal-string)
    (,*asn1:utc-time*          . ,bytevector->der-utc-time)
    (,*asn1:utf8-string*       . ,bytevector->der-utf8-string)
    (,*asn1:visible-string*    . ,bytevector->der-visible-string)
    (,*asn1:videotex-string*   . ,bytevector->der-videotex-string)))

(define (create-primitive-der-object tag-no bytes)
  (let ((ctr (cond ((assv tag-no *constructors*) => cdr) (else #f))))
    (if ctr
	(ctr bytes)
	(make <der-unknown-tag> :constructed? #f :number tag-no :data bytes))))

(define (make-der-external data)
  (define (err)
    (assertion-violation 'read-asn1-object "Corrupted DER external" data))
  (let loop ((data data) (dr #f) (idr #f) (dvd #f) (obj #f))
    (cond ((null? data)
	   (unless obj (err))
	   (make <der-external>
	     :direct-reference dr
	     :indirect-reference (der-integer->integer idr)
	     :data-value-descriptor dvd
	     :encoding obj))
	  ((der-object-identifier? (car data))
	   (when dr (err))
	   (loop (cdr data) (car data) idr dvd obj))
	  ((der-integer? (car data))
	   (when idr (err))
	   (loop (cdr data) dr (car data) dvd obj))
	  ((der-tagged-object? (car data))
	   (when (or obj (not (null? (cdr data)))) (err))
	   (loop (cdr data) dr idr dvd (car data)))
	  ((asn1-object? (car data))
	   (when dvd (err))
	   (loop (cdr data) dr idr (car data) obj))
	  (else (err)))))

(define (read-tagged-object in constructed? tag)
  (cond (constructed?
	 (case (length in)
	   ((0) (make <der-tagged-object> :explicit? #t :tag-no tag :obj #f))
	   ((1) (make <der-tagged-object> :explicit? #t :tag-no tag
		      :obj (car in)))
	   (else (make <der-tagged-object>
		   :explicit? #f :tag-no tag :obj (make-der-sequence in)))))
	((zero? (bytevector-length in))
	 (make <der-tagged-object> :explicit? #f :tag-no tag
	       :obj #f))
	(else
	 (make <der-tagged-object> :explicit? #f :tag-no tag
	       :obj (bytevector->der-octet-string in)))))

(define (convert-tag b tag)
  (let ((b2 (bitwise-and b #x1F)))
    (if (= b2 #x1F)
	(bytevector->integer tag)
	b2)))
(define (object-builder b tag data constructed?)
  (when (eof-object? data)
    (assertion-violation 'read-asn1-object "EOF found during reading data"))
  (let ((tag (convert-tag b tag)))
    (cond ((not (zero? (bitwise-and b *asn1:application*)))
	   (make <der-application-specific>
	     :constructed? constructed?
	     :tag tag
	     ;; TODO Should we always make data bytevector?
	     :octets (if constructed?
			 (bytevector-concatenate
			  (map asn1-encodable->bytevector data))
			 data)))
	  ((not (zero? (bitwise-and b *asn1:tagged*)))
	   (read-tagged-object data constructed? tag))
	  (constructed?
	   (cond ((= tag *asn1:octet-string*)
		  (list->ber-octet-string
		   (map der-octet-string->bytevector data)))
		 ((= tag *asn1:sequence*) (make-der-sequence data))
		 ((= tag *asn1:set*) (make-der-set data))
		 ((= tag *asn1:external*) (make-der-external data))
		 (else (let ((ctr (cond ((assv tag *constructors*) => cdr)
					(else #f)))
			     (value (map der-octet-string->bytevector data)))
			 ;; Indefinite length value will be encoded to a list
			 ;; of DER octet string so apply matched procedure
			 ;; with unwrapped value
			 ;; TODO should we check the length so we can put
			 ;;      better error message?
			 (if ctr
			     (apply ctr value)
			     (make <der-unknown-tag>
			       :constructed? #t :number tag
			       :data (bytevector-concatenate value)))))))
	  (else (create-primitive-der-object tag data)))))

(define read-object (make-emv-tlv-parser :object-builder object-builder))

(define (read-asn1-object (in (and input-port? binary-port?))) (read-object in))
(define (bytevector->asn1-object (bv bytevector?))
  (read-asn1-object (open-bytevector-input-port bv)))
)
