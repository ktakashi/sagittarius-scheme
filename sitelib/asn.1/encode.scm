;;; -*- Scheme -*-
;;;
;;; encode.scm - ASN.1 encoder
;;;
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; For now we just implemented only the part which RKCS#1 v1.5 encode requires.
(library (asn.1 encode)
    (export der-encoder)
    (import (asn.1 types)
	    (rnrs)
	    (srfi :13 strings)
	    (sagittarius)
	    (sagittarius control))

  ;; Tag util
  (define *tag-class-shift* 30)
  (define *max-tag-number* #x1FFFFFFF)

  (define (get-tag-class tag)
    (bitwise-arithmetic-shift-right tag *tag-class-shift*))
  (define (get-tag-number tag)
    (bitwise-and tag *max-tag-number*))

  ;; tree verify
  (define (verify tree object)
    (check-arg asn.1-object? object verify)
    (check-arg asn.1-type? tree verify)
    ;; TODO later
    #t)

  (define (vector->bytevector v)
    ;; check range
    (vector-for-each (lambda (b)
		       (unless (and (real? b)
				    (or (<= #x-7F b #x7F)
					(<= 0 b #xFF)))
			 (assertion-violation 'vector->bytevector
					      "invalid range"
					      b v))) v)
    (let* ((len (vector-length v))
	   (bv (make-bytevector len 0)))
      (do ((i 0 (+ i 1)))
	  ((= i len) bv)
	(let ((c (vector-ref v i)))
	  (if (negative? c)
	      (bytevector-s8-set! bv i c)
	      (bytevector-u8-set! bv i c))))))


  (define (oid->ber oid)
    (define (parse-oid oid)
      (let loop ((oid oid)
		 (r '()))
	(receive (arc rest) (string-scan oid "." 'both)
	  (if (string-index rest #\.)
	      (loop rest (cons (string->number arc 10) r))
	      (reverse! (cons (string->number rest 10) (cons (string->number arc 10) r)))))))
    (define (put-integer port i)
      (put-u8 port (bitwise-and i #xFF)))
    (call-with-bytevector-output-port
     (lambda (port)
       (define (encode-arc-value value)
	 (cond ((< value 128)
		(put-integer port (bitwise-and value #x7F)))
	       ((< value #x4000) ;; 128 ^2
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 7) #x80))
		(put-integer port (bitwise-and value #x7F)))
	       ((< value #x200000) ;; 128 ^3
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 14) #x80))
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 7) #x80))
		(put-integer port (bitwise-and value #x7F)))
	       ((< value #x10000000) ;; 128 ^4
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 21) #x80))
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 14) #x80))
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 7) #x80))
		(put-integer port (bitwise-and value #x7F)))
	       (else
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 28) #x80))
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 21) #x80))
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 14) #x80))
		(put-integer port (bitwise-ior (bitwise-arithmetic-shift-right value 7) #x80))
		(put-integer port (bitwise-and value #x7F)))))
       (let ((arcs (parse-oid oid)))
	 (unless (and (pair? arcs)
		      (> (length arcs) 2))
	   (assertion-violation 'oid->ber
				"invalid oid" oid))
	 (encode-arc-value (+ (* (car arcs) 40) (cadr arcs)))
	 (let loop ((arcs (cddr arcs)))
	   (unless (null? arcs)
	     (encode-arc-value (car arcs))
	     (loop (cdr arcs))))))))

  ;; encoder returns bytevector
  ;; TODO this encoder is not implemented totally, this is only for what I need for now.
  (define-with-key (der-encoder object :key (tree #f))
    ;; returns 2 values, list of tag and tag length
    (define (get-tag tag constructed?)
      ;; Bits 8 and 7 specify the class, bit 6 indicates if the tag is constructed
      (let ((mask (bitwise-ior
		   (if constructed? TAG_CONSTRUCTIVE TAG_PRIMITIVE)
		   (bitwise-arithmetic-shift-left (get-tag-class tag) 6)))
	    (num (get-tag-number tag)))
	(if (< num 31)
	    ;; encode with the low tag number form
	    ;; bits 5-1 give the tag number.
	    `(,(bitwise-ior mask num))
	    ;; bits 5-1 are all set to 1
	    ;; write the tag number in base 128
	    (cond ((< num 128)
		   `(,(bitwise-ior mask #x1F) ,(bitwise-and num #x7F)))
		  ((< num #x4000) ;; 128 ^ 2
		   `(,(bitwise-ior mask #x1F)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 7) #x80) #xFF)
		     ,(bitwise-and num #x7F)))
		  ((< num #x200000) ;; 128 ^ 3
		   `(,(bitwise-ior mask #x1F)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 14) #x80) #xFF)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 7) #x80) #xFF)
		     ,(bitwise-and num #x7F)))
		  ((< num #x10000000) ;; 128 ^ 4
		   `(,(bitwise-ior mask #x1F)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 21) #x80) #xFF)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 14) #x80) #xFF)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 7) #x80) #xFF)
		     ,(bitwise-and num #x7F)))
		  (else
		   `(,(bitwise-ior mask #x1F)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 28) #x80) #xFF)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 21) #x80) #xFF)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 14) #x80) #xFF)
		     ,(bitwise-ior (bitwise-and (bitwise-arithmetic-shift-right num 7) #x80) #xFF)
		     ,(bitwise-and num #x7F)))))))
    ;; returns list of length
    (define (encode-length len)
      (cond ((< len 0)
	     (assertion-violation 'encode-length
				  "negative length is not allowed"))
	    ((<= len #x7F) `(,len))
	    ((<= len #xFF) `(#x81 ,len))
	    ((<= len #xFFFF) `(#x82 ,(bitwise-arithmetic-shift-right len 8)
				    ,(bitwise-and len #xFF)))
	    ((<= len #xFFFFFF) `(#x83 ,(bitwise-arithmetic-shift-right len 16)
				      ,(bitwise-arithmetic-shift-right len 8)
				      ,(bitwise-and len #xFF)))
	    (else
	     (assertion-violation 'encode-length
				  "SEQUENCE too long"))))
    (define (encode object)
      (let-syntax ((gen-encoder (er-macro-transformer
				 (lambda (f r c)
				   (let ((o (cadr f))
					 (->bytevector (caddr f))
					 (ctr? (cadddr f)))
				     `(let* ((value (asn.1-object-value ,o))
					     (tag   (asn.1-object-tag ,o))
					     (bv    (,->bytevector value)))
					(vector->bytevector `#(,@(get-tag tag ,ctr?)
							       ,@(encode-length (bytevector-length bv))
							       ,@(bytevector->u8-list bv)))))))))
	(let ((tag (asn.1-object-tag object)))
	  (cond ((= tag TAG_BOOLEAN)
		 (gen-encoder object (lambda (v) (if v #vu8(#xFF) #vu8(0))) #f))
		((= tag TAG_INTEGER)
		 (gen-encoder object integer->bytevector #f))
		((= tag TAG_NULL)
		 (vector->bytevector `#(,@(get-tag tag #f) 0)))
		((= tag TAG_OCTET_STRING)
		 (gen-encoder object (lambda (v) v) #f))
		((or (= tag TAG_UTF8_STRING)
		     (= tag TAG_PRINTABLE_STRING)
		     (= tag TAG_IA5_STRING)
		     (= tag TAG_VISIBLE_STRING))
		 (gen-encoder object string->utf8 #f))
		((= tag TAG_BMP_STRING)
		 ;; TODO little?
		 (gen-encoder object string->utf16 #f))
		((= tag TAG_OBJECT_IDENTIFIER)
		 ;; maybe we should cache the value
		 (gen-encoder object oid->ber #f))
		((= tag TAG_SEQUENCE)
		 (let ((value (asn.1-object-value object)))
		   (let ((bv (call-with-bytevector-output-port
			      (lambda (port)
				(vector-for-each (lambda (o)
						   (put-bytevector port (encode o)))
						 value)))))
		     ;; ugh...
		     (call-with-bytevector-output-port
		      (lambda (port)
			(put-bytevector port (u8-list->bytevector (get-tag tag #t)))
			(put-bytevector port (u8-list->bytevector (encode-length (bytevector-length bv))))
			(put-bytevector port bv))))))
		;; TODO the rest will be later
		(else (error 'der-encoder "not supported yet" tag object)))))
      )

    (check-arg asn.1-object? object der-encoder)
    ;; check if given object is valid.
    (when tree
      (verify tree object))
    (encode object))

)

;; Local Variables:
;; coding: utf-8
;; End: