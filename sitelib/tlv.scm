;;; -*- Scheme -*-
;;;
;;; tlv.scm - TLV parser
;;;
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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
(library (tlv)
    (export make-tlv-parser make-emv-tlv-parser
	    EMV
	    tlv-tag tlv-length tlv-data
	    <tlv>)
    (import (rnrs) (clos user)
	    (only (sagittarius) format define-constant)
	    (sagittarius control))

  ;; default TLV class
  (define-class <tlv> ()
    ((tag    :init-keyword :tag    :reader tlv-tag)
     (length :init-keyword :length :reader tlv-length)
     ;; bytevector
     (data   :init-keyword :data   :reader tlv-data)))

  (define-method write-object ((o <tlv>) p)
    (format p "#<tlv :tag ~x :length ~a :data ~a>" 
	    (tlv-tag o) (tlv-length o) (tlv-data o)))

  (define (make-tlv-unit tag data)
    (make <tlv> :tag tag :length (bytevector-length data) :data data))

  ;; marker
  (define-constant EMV 'emv)

  (define (make-tlv-parser format . opts)
    (case format
      ((emv) (apply make-emv-tlv-parser opts))
      (else (assertion-violation 'make-tlv-parser
				 "given format is not supported" format))))

  (define-syntax u8-ref
    (syntax-rules ()
      ((_ bv i) (bytevector-u8-ref bv i))))

  (define (read-tag in b)
    (if (= (bitwise-and b #x1F) #x1F)
	(let1 b (get-u8 in)
	  (when (zero? (bitwise-and b #x7F))
	    (assertion-violation 
	     'read-tag "corrupted stream - invalid high tag number found" b))
	  (do ((b2 b (get-u8 in))
	       (tag-no 0 (bitwise-arithmetic-shift 
			  (bitwise-ior tag-no (bitwise-and b2 #x7F))
			  7)))
	      ((or (eof-object? b2) (zero? (bitwise-and b2 #x80)))
	       (when (eof-object? b2)
		 (assertion-violation 'read-tag
				      "EOF found inside tag value"))
	       (bitwise-ior tag-no (bitwise-and b #x7F)))))
	b))

  (define (read-length in)
    (let1 len (get-u8 in)
      (when (eof-object? len)
	(assertion-violation 'read-length "EOF found when length expected"))
      (cond ((= len #x80) #f) ;; indefinite length. TODO correct?
	    ((> len 127)
	     (let1 size (bitwise-and len #x7F)
	       (do ((i 0 (+ i 1)) (rlen 0))
		   ((= i size)
		    (when (negative? rlen)
		      (assertion-violation
		       'read-length "corrupted stream - negative length found"))
		    rlen)
		 (let1 next (get-u8 in)
		   (when (eof-object? next)
		     (assertion-violation 'read-length 
					  "EOF found reading length"))
		   (set! rlen (+ (bitwise-arithmetic-shift rlen 8) next))))))
	    (else len))))
  
  (define (tlv-builder in first tag len)
    (make-tlv-unit tag (get-bytevector-n in len)))
  
  (define (make-emv-tlv-parser :key 
			       (object-builder tlv-builder)
			       (indefinite-handler #f))
    (lambda (in :optional (indefinite? #f))
      (let1 b (get-u8 in)
	(and (not (eof-object? b))
	     (let1 separator? (and indefinite-handler (indefinite-handler in b))
	       (cond ((and (not indefinite?) separator?)
		      (assertion-violation 'tlv-parser
					   "unexpected end-of-contents marker"
					   (get-bytevector-all in)))
		     ;; we only checks 2 bytes for end-of-content marks
		     (separator? #f)
		     (else
		      (let ((tag (read-tag in b))
			    (len (read-length in)))
			(if (not len)
			    (if indefinite-handler
				(indefinite-handler in b tag)
				(assertion-violation 
				 'tlv-parser "indefinite length found"))
			    (object-builder in b tag len))))))))))
)
