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
	    tlv-builder
	    tlv-tag tlv-length tlv-data tlv-components
	    <tlv>
	    ;; utilities
	    dump-tlv ;;encode-tlv
	    )
    (import (rnrs) (clos user)
	    (only (sagittarius)
		  format define-constant reverse! bytevector->integer)
	    (sagittarius control)
	    (srfi :26 cut))

  ;; default TLV class
  (define-class <tlv> ()
    ((tag    :init-keyword :tag    :reader tlv-tag)
     ;; bytevector
     (data   :init-keyword :data   :reader tlv-data :init-value #f)
     ;; constructed TLV
     (components :init-keyword :components :reader tlv-components
		 :init-value '())))

  (define-method write-object ((o <tlv>) p)
    (if (tlv-data o)
	(format p "#<tlv :tag ~x :data ~a>" (tlv-tag o) (tlv-data o))
	(format p "#<tlv :tag ~x :components ~a>"
		(tlv-tag o) (tlv-components o))))

  (define (make-tlv-unit tag data)
    (make <tlv> :tag tag :length (bytevector-length data) :data data))

  ;; marker
  (define-constant EMV 'emv)

  (define (make-tlv-parser format . opts)
    (case format
      ((emv) (apply make-emv-tlv-parser opts))
      (else (assertion-violation 'make-tlv-parser
				 "given format is not supported" format))))

  (define (read-tag in b)
    (if (= (bitwise-and b #x1F) #x1F)
	(let1 b2 (get-u8 in)
	  (when (zero? (bitwise-and b2 #x7F))
	    (assertion-violation 
	     'read-tag "corrupted stream - invalid high tag number found" b2))
	  (do ((b3 b2 (get-u8 in)) 
	       (r b (bitwise-ior (bitwise-arithmetic-shift r 8) b3)))
	      ((or (eof-object? b3) (zero? (bitwise-and b3 #x80)))
	       (when (eof-object? b)
		 (assertion-violation 'read-tag "EOF found inside tag value"))
	       (bitwise-ior (bitwise-arithmetic-shift r 8) b3))))
	b))

  (define (read-length in)
    (let1 len (get-u8 in)
      (when (eof-object? len)
	(assertion-violation 'read-length "EOF found when length expected"))
      (cond ((= len #x80) #f) ;; indefinite length. TODO correct?
	    ((zero? (bitwise-and len #x80)) len)
	    (else
	     (let1 size (bitwise-and len #x7F)
	       (let loop ((i 1) (rlen (get-u8 in)))
		 (if (= i size)
		     (begin 
		       (when (negative? rlen)
			 (assertion-violation 'read-length
			  "corrupted stream - negative length found"))
		       rlen)
		     (loop (+ i 1) (+ (bitwise-arithmetic-shift rlen 8)
				      (get-u8 in))))))))))
  
  (define (tlv-builder b tag data constructed?)
    (if constructed?
	(make <tlv> :tag tag :components data)
	(make <tlv> :tag tag :data data)))
  
  (define (make-emv-tlv-parser :key (object-builder tlv-builder))
    (define (parse-tlv-object-list in in-indefinite?)
      (let loop ((o (tlv-parser in in-indefinite?)) (r '()))
	(if o
	    (loop (tlv-parser in in-indefinite?) (cons o r))
	    (reverse! r))))

    (define (handle-indefinite b tag in)
      (object-builder b tag (parse-tlv-object-list in #t) #t))

    ;; separator is null TLV object
    ;; = tag 0 length 0 data empty
    (define (tlv-parser in :optional (in-indefinite? #f))
      (let1 b (get-u8 in)
	(cond ((eof-object? b) #f)
	      ((and in-indefinite? (zero? b) (zero? (lookahead-u8 in)) 
		    (get-u8 in))
	       #f)
	      (else
	       (let ((tag (read-tag in b))
		     (not-constructed? (zero? (bitwise-and #x20 b)))
		     (len (read-length in)))
		 (cond (len
			(let1 data (get-bytevector-n in len)
			  (when (< (bytevector-length data) len)
			    (assertion-violation 'tlv-parser "corrupted data"))
			  (if not-constructed?
			      (object-builder b tag data #f)
			      (object-builder 
			       b
			       tag 
			       (call-with-port 
				   (open-bytevector-input-port data)
				 (cut parse-tlv-object-list <> in-indefinite?))
			       #t))))
		       (not-constructed?
			(assertion-violation 'tlv-parser
					     "indefinite length found" tag))
		       (else
			(handle-indefinite b tag in))))))))
    tlv-parser)

  (define (dump-tlv tlv :optional (out (current-output-port)))
    (define (print-indent indent)
      (dotimes (i indent)
	(display #\space out)))
    (define (print-tag tag)
      (format out "[Tag] ~X" tag))

    (define (dump-data data indent)
      (newline out)
      (print-indent (+ indent 2))
      (display "[Data]" out)
      (dotimes (i (bytevector-length data))
	(format out " ~X" (bytevector-u8-ref data i))))
    (define (dump-components tlv indent)
      (unless (zero? indent) (newline out))
      (print-indent indent)
      (print-tag (tlv-tag tlv))
      (let1 components (tlv-components tlv)
	(if (null? components)
	    (dump-data (tlv-data tlv) indent)
	    (for-each (cut dump-components <> (+ indent 2)) components))))
    (dump-components tlv 0))

;;  (define (encode-tlv tlv :optional (out #f))
;;    (define (value-length tlv)
;;      (let1 components (tlv-components tlv)
;;	(if (null? components)
;;	    (bytevector-length (tlv-data tlv))
;;	    (fold (^(seed tlv) (+ seed (value-length tlv))) 0 components))))
;;    (define (encode-length len)
;;      (if (< value #x80)
;;	  1
;;	  (implementation-restriction-violation 'encode-tlv 
;;						"not supported yet")))
;;    (if out
;;	(let1 len (value-length tlv)
;;	  len)))
	      
)
