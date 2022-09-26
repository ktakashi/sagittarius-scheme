;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/asn1/writer.scm - ASN.1 writer
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
(library (sagittarius crypto asn1 writer)
    (export write-asn1-encodable)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1 tags)
	    (sagittarius crypto asn1 types)
	    (srfi :117 list-queues))

;; For DER object, we always encode DER as DER is a subset of BER
(define-method write-asn1-encodable ((o <der-boolean>) port type)
  (write-der-encoded *asn1:boolean*
		     (make-bytevector 1 (if (der-boolean->boolean o) #xFF 0))
		     port))

(define-method write-asn1-encodable ((o <der-integer>) port type)
  (let ((v (der-integer->integer o)))
    (write-der-encoded *asn1:integer* (sinteger->bytevector v) port)))

(define-method write-asn1-encodable ((o <der-bit-string>) port type)
  (let* ((value (asn1-simple-object-value o))
	 (len (bytevector-length value))
	 (bytes (make-bytevector (+ len 1) (der-bit-string-padding-bits o))))
    (bytevector-copy! value 0 bytes 1 len)
    (write-der-encoded *asn1:bit-string* bytes port)))

(define-method write-asn1-encodable ((o <der-octet-string>) port type)
  (let ((v (asn1-simple-object-value o)))
    (write-der-encoded *asn1:octet-string* v port)))

(define-method write-asn1-encodable ((o <der-null>) port type)
  (write-der-encoded *asn1:null* #vu8() port))

(define-method write-asn1-encodable ((o <der-object-identifier>) port type)
  (define (->oid-bytevector oid)
    (define oid-length (string-length oid))
    (define (oid-token oid s)
      ;; string-ref = O(1)
      (let loop ((i s) (l '()))
	(if (= oid-length i)
	    (values (string->number (list->string (reverse! l))) i)
	    (let ((v (string-ref oid i)))
	      (if (eqv? v #\.)
		  (values (string->number (list->string (reverse! l))) (+ i 1))
		  (loop (+ i 1) (cons v l)))))))
    (define (write-field n out)
      (let ((byte-count (div (+ (bitwise-length n) 6) 7)))
	(if (zero? byte-count) (put-u8 out 0)
	    (let ((tmp (make-bytevector byte-count 0)))
	      (do ((i (- byte-count 1) (- i 1))
		   (n n (bitwise-arithmetic-shift n -7)))
		  ((< i 0)
		   (let* ((j (- byte-count 1))
			  (v (bytevector-u8-ref tmp j)))
		     (bytevector-u8-set! tmp j (bitwise-and v #x7F))
		     (put-bytevector out tmp)))
		(bytevector-u8-set! tmp i
		 (bitwise-ior (bitwise-and n #x7F) #x80)))))))
	      
    (let*-values (((out e) (open-bytevector-output-port))
		  ;; The first 2 numbers will be encoded into one byte
		  ((n1 i) (oid-token oid 0))
		  ((n2 i) (oid-token oid i)))
      (write-field (+ (* n1 40) n2) out)
      (let loop ((i i))
	(if (= i oid-length)
	    (e)
	    (let-values (((n i) (oid-token oid i)))
	      (write-field n out)
	      (loop i))))))
  (write-der-encoded *asn1:object-identifier*
		     (->oid-bytevector (asn1-simple-object-value o)) port))

(define-method write-asn1-encodable ((o <der-external>) port type)
  (let ((dr (der-external-direct-reference o))
	(idr (der-external-indirect-reference o))
	(dvd (der-external-data-value-descriptor o))
	(obj (der-external-encoding o)))
    (let-values (((out e) (open-bytevector-output-port)))
      (when dr (write-asn1-encodable dr out))
      (when idr (write-asn1-encodable (integer->der-integer idr) out))
      (when dvd (write-asn1-encodable dvd out))
      (write-asn1-encodable obj out)
      (write-der-encoded *asn1:constructed* *asn1:external* (e) port))))

(define-method write-asn1-encodable ((o <der-enumerated>) port type)
  (write-der-encoded *asn1:enumerated*
		     (sinteger->bytevector (der-enumerated->integer o)) port))

(define (write-collection tag o port type)
  (write-der-encoded (bitwise-ior tag *asn1:constructed*)
		     (let-values (((out e) (open-bytevector-output-port)))
		       (for-each (lambda (e) (write-asn1-encodable e out type))
				 (list-queue-list (asn1-collection-elements o)))
			 (e))
		     port))
(define-method write-asn1-encodable ((o <der-sequence>) port type)
  (write-collection *asn1:sequence* o port type))
(define-method write-asn1-encodable ((o <der-set>) port type)
  (write-collection *asn1:set* o port type))

(define-method write-asn1-encodable ((o <der-numeric-string>) port type)
  (write-der-encoded *asn1:numeric-string*
		     (string->utf8 (der-numeric-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-printable-string>) port type)
  (write-der-encoded *asn1:printable-string*
		     (string->utf8 (der-printable-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-t61-string>) port type)
  (write-der-encoded *asn1:t61-string*
		     (string->utf8 (der-t61-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-videotex-string>) port type)
  (write-der-encoded *asn1:videotex-string*
		     (string->utf8 (der-videotex-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-ia5-string>) port type)
  (write-der-encoded *asn1:ia5-string*
		     (string->bytevector (der-ia5-string->string o)
					 (make-transcoder (latin-1-codec)))
		     port))

(define-method write-asn1-encodable ((o <der-utc-time>) port type)
  (write-der-encoded *asn1:utc-time*
		     (string->utf8 (der-utc-time->string o))
		     port))

(define-method write-asn1-encodable ((o <der-generalized-time>) port type)
  (write-der-encoded *asn1:generalized-time*
		     (string->utf8 (der-generalized-time->string o))
		     port))

(define-method write-asn1-encodable ((o <der-graphic-string>) port type)
  (write-der-encoded *asn1:graphic-string*
		     (string->utf8 (der-graphic-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-visible-string>) port type)
  (write-der-encoded *asn1:visible-string*
		     (string->utf8 (der-visible-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-general-string>) port type)
  (write-der-encoded *asn1:general-string*
		     (string->utf8 (der-general-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-universal-string>) port type)
  (write-der-encoded *asn1:universal-string*
		     (string->utf8 (der-universal-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-bmp-string>) port type)
  (write-der-encoded *asn1:bmp-string*
		     (string->utf16 (der-bmp-string->string o) (endianness big))
		     port))

(define-method write-asn1-encodable ((o <der-utf8-string>) port type)
  (write-der-encoded *asn1:utf8-string*
		     (string->utf8 (der-utf8-string->string o))
		     port))

(define-method write-asn1-encodable ((o <der-application-specific>) port type)
  (write-der-encoded
   (if (der-application-specific-constructed? o)
       (bitwise-ior *asn1:constructed* *asn1:application*)
       *asn1:application*)
   (der-application-specific-tag o)
   (der-application-specific-octets o)
   port))

(define-method write-asn1-encodable ((o <der-tagged-object>) port type)
  (define constructed&tag (bitwise-ior *asn1:constructed* *asn1:tagged*))
  (define (get-flag pred) (if pred constructed&tag *asn1:tagged*))
  (let ((obj (der-tagged-object-obj o)))
    (if obj
	(let ((bytes (asn1-encodable->bytevector obj)))
	  (if (der-tagged-object-explicit? o)
	      (write-der-encoded constructed&tag (der-tagged-object-tag-no o)
				 bytes port)
	      (let* ((b (bytevector-u8-ref bytes 0))
		     (flag (get-flag
			    (not (zero? (bitwise-and b *asn1:constructed*))))))
		(write-der-tag flag (der-tagged-object-tag-no o) port)
		(put-bytevector port bytes 1 (- (bytevector-length bytes) 1)))))
	(write-der-encoded (get-flag (der-tagged-object-explicit? o))
			   (der-tagged-object-tag-no o) #vu8() port))))

(define-method write-asn1-encodable ((o <der-unknown-tag>) port type)
  (write-der-encoded (if (der-unknown-tag-constructed? o)
			 *asn1:constructed*
			 0)
		     (der-unknown-tag-number o)
		     (der-unknown-tag-data o)
		     port))


(define-method write-asn1-encodable ((o <ber-octet-string>) port type)
  (case type
    ((der) (call-next-method)) ;; der
    ((ber)
     (put-u8 port (bitwise-ior *asn1:constructed* *asn1:octet-string*))
     (put-u8 port #x80)
     (for-each (lambda (o) (write-asn1-encodable o port type))
	       (ber-octet-string-octs o))
     (put-u8 port #x00)
     (put-u8 port #x00))
    (else (assertion-violation 'write-asn1-encodable "Unknown type" type))))

(define-method write-asn1-encodable ((o <ber-tagged-object>) port type)
  (case type
    ((der) (call-next-method))
    ((ber)
     (write-der-tag (bitwise-ior *asn1:constructed* *asn1:tagged*)
		    (der-tagged-object-tag-no o) port)
     (put-u8 port #x80)
     (let ((obj (der-tagged-object-obj o)))
       (if (der-tagged-object-explicit? o)
	   (write-asn1-encodable obj port type)
	   (let ((e (cond ((der-octet-string? obj)
			   (if (ber-octet-string? obj)
			       (ber-octet-string-octs obj)
			       (ber-octet-string-octs
				(bytevector->ber-octed-string
				 (der-octet-string->bytevector obj)))))
			  ((asn1-collection? obj)
			   (list-queue-list (asn1-collection-elements obj)))
			  (else
			   (assertion-violation 'write-asn1-encodable
				"Unknown BER tagged object content" obj)))))
	     (for-each (lambda (o) (write-asn1-encodable o port type)) e))))
     (put-u8 port #x00)
     (put-u8 port #x00))
    (else (assertion-violation 'write-asn1-encodable "Unknown type" type))))

(define (write-ber-collection tag o port type)
  (put-u8 port (bitwise-ior *asn1:constructed* tag))
  (put-u8 port #x80)
  (list-queue-for-each (lambda (e) (write-asn1-encodable e port type))
		       (asn1-collection-elements o))
  (put-u8 port #x00)
  (put-u8 port #x00))
(define-method write-asn1-encodable ((o <ber-sequence>) port type)
  (case type
    ((der) (call-next-method))
    ((ber) (write-ber-collection *asn1:sequence* o port type))
    (else (assertion-violation 'write-asn1-encodable "Unknown type" type))))
(define-method write-asn1-encodable ((o <ber-set>) port type)
  (case type
    ((der) (call-next-method))
    ((ber) (write-ber-collection *asn1:set* o port type))
    (else (assertion-violation 'write-asn1-encodable "Unknown type" type))))

(define write-der-encoded
  (case-lambda
   ((tag bytes output)
    (put-u8 output tag)
    (write-der-length (bytevector-length bytes) output)
    (put-bytevector output bytes))
   ((flags tag-no bytes output)
    (write-der-tag flags tag-no output)
    (write-der-length (bytevector-length bytes) output)
    (put-bytevector output bytes))))

(define rash bitwise-arithmetic-shift-right)
(define (write-der-tag flags tag-no p)
  (cond ((< tag-no 31) (put-u8 p (bitwise-ior flags tag-no)))
	(else
	 (put-u8 p (bitwise-ior flags #x1f))
	 (if (< tag-no 128)
	     (put-u8 p tag-no)
	     (let ((stack (make-bytevector 5))
		   (pos 4))
	       (bytevector-u8-set! stack pos (bitwise-and tag-no #x7f))
	       (let loop ((ntag-no (rash tag-no 7))
			  (pos (- pos 1)))
		 (bytevector-u8-set! stack pos 
				     (bitwise-ior (bitwise-and ntag-no #x7f)
						  #x80))
		 (if (> ntag-no 127)
		     (loop (rash ntag-no 7) (- pos 1))
		     (put-bytevector p stack pos))))))))

(define (write-der-length len p)
  (if (> len 127)
      (let ((size (do ((size 1 (+ size 1))
		       (val (rash len 8) (rash val 8)))
		      ((zero? val) size))))
	(put-u8 p (bitwise-ior size #x80))
	(do ((i (* (- size 1) 8) (- i 8)))
	    ((< i 0))
	  (put-u8 p (bitwise-and (rash len i) #xff))))
      (put-u8 p len)))


)
