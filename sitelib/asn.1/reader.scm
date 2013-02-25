;;; -*- Scheme -*-
;;;
;;; types.scm - ASN.1 binary reader
;;;
;;;   Copyright (c) 2009-2012  Takashi Kato  <ktakashi@ymail.com>
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

(library (asn.1 reader)
    (export read-asn.1-object)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (tlv)
	    (util port)
	    (asn.1 types)
	    (asn.1 der tags)
	    (asn.1 ber types))

  (define (create-der-bit-string bytes)
      ;; well well
      (let ((len (bytevector-length bytes)))
	(when (< len 1)
	  (assertion-violation 'create-primitive-der-object
			       "truncated BIT STRING detected"))
	(let ((pad (bytevector-u8-ref bytes 0))
	      (data (make-bytevector (- len 1))))
	  (bytevector-copy! bytes 1 data 0 (- len 1))
	  (make-der-bit-string data pad))))

  (define *constructors* 
    `((,BIT-STRING 	  . ,create-der-bit-string)
      (,BMP-STRING 	  . ,make-der-bmp-string)
      (,BOOLEAN    	  . ,make-der-boolean)
      (,ENUMERATED 	  . ,make-der-enumerated)
      (,GENERALIZED-TIME  . ,make-der-generalized-time)
      (,GENERAL-STRING    . ,make-der-general-string)
      (,IA5-STRING        . ,make-der-ia5-string)
      (,INTEGER           . ,make-der-integer)
      (,NULL              . ,(lambda (_) (make-der-null)))
      (,NUMERIC-STRING    . ,make-der-numeric-string)
      (,OBJECT-IDENTIFIER . ,make-der-object-identifier)
      (,OCTET-STRING      . ,make-der-octet-string)
      (,PRINTABLE-STRING  . ,make-der-printable-string)
      (,T61-STRING        . ,make-der-t61-string)
      (,UNIVERSAL-STRING  . ,make-der-universal-string)
      (,UTC-TIME          . ,make-der-utc-time)
      (,UTF8-STRING       . ,make-der-utf8-string)
      (,VISIBLE-STRING    . ,make-der-visible-string)))

  (define (create-primitive-der-object tag-no bytes)
    (let ((ctr (cond ((assv tag-no *constructors*) => cdr)
		     (else #f))))
      (if ctr
	  (ctr bytes)
	  (make-der-unknown-tag #f tag-no bytes))))

  (define (read-tagged-object ber? in constructed? tag)
    (if constructed?
	(let ((len (length in))
	      (make-tagged (if ber?
			       make-ber-tagged-object 
			       make-der-tagged-object))
	      (make-seq (if ber? make-ber-sequence make-der-sequence)))
	  (if (= len 1)
	      (make-tagged #t tag (car in))
	      (make-tagged #f tag (apply make-seq in))))
	(make-der-tagged-object #f tag
	 (make-der-octet-string 
	  (get-bytevector-all (open-bytevector-input-port in))))))

  (define (build-object b tag data constructed?)
    (when (eof-object? data)
      (assertion-violation 'build-object "EOF found during reading data"))
    (cond ((not (zero? (bitwise-and b APPLICATION)))
	   (make-der-application-specific constructed? tag data))
	  ((not (zero? (bitwise-and b TAGGED)))
	   (read-tagged-object #f data constructed? tag))
	  (constructed?
	   (cond ((= tag OCTET-STRING)
		  (apply make-ber-constructed-octet-string data))
		 ((= tag SEQUENCE)
		  (apply make-der-sequence data))
		 ((= tag SET)
		  (apply make-der-set data))
		 ((= tag EXTERNAL)
		  (apply make-der-external data))
		 (else
		  (let ((ctr (cond ((assv tag *constructors*) => cdr)
				   (else #f))))
		    (unless ctr (display tag) (newline))
		    (if ctr
			(apply ctr data)
			(apply make-der-unknown-tag #f tag data))))))
	  (else
	   (create-primitive-der-object tag data))))

  (define (convert-tag b tag)
    (let ((b2 (bitwise-and b #x1F)))
      (if (= b2 #x1F)
	  tag
	  b2)))

  (define (object-builder b tag data constructed?)
    (build-object b (convert-tag b tag) data constructed?))

  (define read-object (make-emv-tlv-parser :object-builder object-builder))

  (define (read-asn.1-object in)
    (unless (binary-port? in)
      (assertion-violation 'read-asn.1-object
			   "binary port required" in))
    (read-object in))
  )
