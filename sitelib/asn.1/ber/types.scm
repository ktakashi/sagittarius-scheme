;;; -*- Scheme -*-
;;;
;;; encode.scm - ASN.1 BER types
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

(library (asn.1 ber types)
    (export <ber-constructed-octet-string>
	    <ber-application-specific>
	    <ber-tagged-object>
	    <ber-sequence>
	    <ber-set>
	    <ber-null>

	    make-ber-constructed-octet-string
	    make-ber-application-specific
	    make-ber-tagged-object
	    make-ber-sequence
	    make-ber-set
	    make-ber-null)
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (asn.1 types)
	    (asn.1 der tags)
	    (asn.1 der encode)
	    (srfi :1 lists))

  (define *max-length* 1000)

  (define-class <ber-constructed-octet-string> (<der-octet-string>)
    ((octs :init-keyword :octs)))
  (define-method make-ber-constructed-octet-string ((b <bytevector>))
    (let* ((len (bytevector-length b))
	   (l (do ((i 0 (+ i 1)) (r '()))
		  ((= i len) (reverse! r))
		(let* ((end (if (> (+ i *max-length*) len)
				len 
				(+ i *max-length*)))
		       (nstr (make-bytevector (- end i))))
		  (bytevector-copy! b i nstr 0 (bytevector-length nstr))
		  (set! r (cons (make-der-octet-string nstr) r))))))
      (make <ber-constructed-octet-string> :string b :octs l)))
  (define-method make-ber-constructed-octet-string l
    (let ((b (call-with-bytevector-output-port
	      (lambda (p)
		(for-each (lambda (o)
			    (put-bytevector p (slot-ref o 'string))) l)))))
      (make <ber-constructed-octet-string> :string b :octs l)))
  (define-method der-encode ((o <ber-constructed-octet-string>) (p <port>))
    ;; TODO this must be separated path for DER and BER
    (put-u8 p (bitwise-ior CONSTRUCTED OCTET-STRING))
    (put-u8 p #x80)
    (for-each (lambda (o) (der-write-object o p)) (slot-ref o 'octs))
    (put-u8 p #x00)
    (put-u8 p #x00))
  (define-method write-object ((o <ber-constructed-octet-string>) (p <port>))
    (der-generic-write "ber-constructed-octet-string" (slot-ref o 'string) p))

  (define-class <ber-application-specific> (<der-application-specific>) ())
  (define-method make-ber-application-specific ((tag <integer>) . l)
    (make <ber-application-specific> :tag tag
	  (call-with-bytevector-output-port
	   (lambda (p)
	     (for-each (o) (der-encode o p) l)))))

  (define-class <ber-tagged-object> (<der-tagged-object>) ())
  (define-method make-ber-tagged-object
    ((explicit? <boolean>) (tag-no <integer>) (obj <der-encodable>))
    (make <ber-tagged-object>
      :tag-no tag-no
      :explicit? (if (is-a? obj <asn.1-choice>) #t explicit?)
      :obj obj))
  (define-method der-encode ((o <ber-tagged-object>) (p <port>))
    ;; TODO this must be separated path for DER and BER
    (der-write-tag (bitwise-ior CONSTRUCTED TAGGED) (slot-ref o 'tag-no) p)
    (put-u8 p #x80)
    (unless (slot-ref o 'empty)
      (cond ((slot-ref o 'explicit?)
	     (der-write-object (slot-ref o 'obj)))
	    (else
	     (let* ((obj (slot-ref o 'obj))
		    (e (cond 
			((is-a? obj <asn.1-octet-string>)
			 (cond ((is-a? obj <ber-constructed-octet-string>)
				(slot-ref obj 'octs))
			       (else
				(let ((bco (make-ber-constructed-octet-string
					    (slot-ref obj 'octets))))
				  (slot-ref bco 'octs)))))
			((is-a? obj <asn.1-sequence>) (slot-ref obj 'sequence))
			((is-a? obj <asn.1-set>) (slot-ref obj 'set))
			(else
			 (assertion-violation 'der-encode
					      "not implemented" obj)))))
	       (for-each (lambda (o) (der-write-object o p)) e))))))
  (define-method write-object ((o <ber-tagged-object>) (p <port>))
    (der-generic-write "ber-tagged-object" 
		   (format "[~a] ~a~a" (slot-ref o 'tag-no)
			   (slot-ref o 'explicit?)
			   (der-list->string 
			    (list (slot-ref o 'obj))))
		   p))

  (define-class <ber-sequence> (<der-sequence>) ())
  (define-method make-ber-sequence l
    (or (for-all (lambda (o) (is-a? o <der-encodable>)) l)
	(assertion-violation 'make-ber-sequcne
			     "list of <der-encodable> required" l))
    (make <ber-sequence> :sequence l))
  (define-method der-encode ((o <ber-sequence>) (p <port>))
    (put-u8 p (bitwise-ior SEQUENCE CONSTRUCTED))
    (put-u8 p #x80)
    (for-each (lambda (o) (der-write-object o p)) (slot-ref o 'sequence))
    (put-u8 p #x00)
    (put-u8 p #x00))
  (define-method write-object ((o <ber-sequence>) (p <port>))
    (der-generic-write "ber-sequence" (der-list->string (slot-ref o 'sequence)) p))

  (define-class <ber-set> (<der-set>) ())
  (define-method make-ber-set l
    (or (for-all (lambda (o) (is-a? o <der-encodable>)) l)
	(assertion-violation 'make-ber-sequcne
			     "list of <der-encodable> required" l))
    (make <ber-set> :sequence l :need-sort? #f))
  (define-method der-encode ((o <ber-set>) (p <port>))
    (put-u8 p (bitwise-ior SET CONSTRUCTED))
    (put-u8 p #x80)
    (for-each (lambda (o) (der-write-object o p)) (slot-ref o 'sequence))
    (put-u8 p #x00)
    (put-u8 p #x00)
    )
  (define-method write-object ((o <ber-set>) (p <port>))
    (der-generic-write "ber-set" (der-list->string (slot-ref o 'set)) p))

  (define-class <ber-null> (<der-null>) ())
  (define *ber-null* (make <ber-null>))
  (define-method make-ber-null () *ber-null*)
  (define-method der-encode ((o <ber-null>) (p <port>))
    (put-u8 p NULL))
  (define-method write-object ((o <ber-null>) (p <port>))
    (der-generic-write "ber-null" "" p))

  )