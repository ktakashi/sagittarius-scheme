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
	    (util port)
	    (asn.1 types)
	    (asn.1 der tags)
	    (asn.1 ber types))

  ;; kinda ugly solution
  (define (read-octet-strings in)
    (define (skip-0 in)
      (do ((b (lookahead-u8 in) (lookahead-u8 in)))
	  ((> b 0) #t)
	(get-u8 in)))
    (call-with-bytevector-output-port
     (lambda (out)
       (do ((tag (get-u8 in) (get-u8 in))
	    (i 0 (+ i 1)))
	   ((zero? tag) (skip-0 in))
	 (let ((tag-no (read-tag-number in tag))
	       (len    (read-length in)))
	   (put-bytevector out (get-bytevector-n in len)))))))

  (define (do-indefinite-length in tag tag-no constructed?)
    (unless constructed?
      (assertion-violation 'do-indefinite-length
			   "indefinite length primitive encoding encountered"))
    (cond ((not (zero? (bitwise-and tag APPLICATION)))
	   (apply make-ber-application-specific tag-no (read-objects in #t)))
	  ((not (zero? (bitwise-and tag TAGGED)))
	   (make-ber-tagged-object 
		     constructed? tag-no
		     (read-tagged-object #t in constructed? tag-no)))
	  ((= tag-no OCTET-STRING)
	   ;; TODO correct?
	   (make-ber-constructed-octet-string (read-octet-strings in)))
	  ((= tag-no SEQUENCE)
	   (apply make-ber-sequence (read-objects in #t)))
	  ((= tag-no SET)
	   (apply make-ber-set (read-objects in #t)))
	  ((= tag-no EXTERNAL)
	   ;; TODO this is actually not correct, but for now.
	   (apply make-der-external (read-objects in)))
	  ;; TODO there are other tags that may be constructed (e.g. BIT-STRING)
	  (else
	   (assertion-violation 'do-indefinite-length
				"unknown BER object encountered" tag-no))))

  (define (read-objects in :optional (skip? #f))
    (port-fold-right cons '() (lambda () (let ((r (read-object in skip?)))
					   (if r r (eof-object))))))

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
    (cond (constructed? 
	   (let* ((objs (read-objects in ber?))
		  (len  (length objs))
		  (make-tagged (if ber?
				   make-ber-tagged-object 
				   make-der-tagged-object))
		  (make-seq (if ber? make-ber-sequence make-der-sequence)))
	     (cond ((= len 1)
		    (make-tagged #t tag (car objs)))
		   (else
		    (make-tagged #f tag (apply make-seq objs))))))
	  (else 
	   (make-der-tagged-object 
	    #f tag
	    (make-der-octet-string (get-bytevector-all in))))))

  (define (build-object tag tag-no len in)
    (let ((constructed? (not (zero? (bitwise-and tag CONSTRUCTED))))
	  (data (get-bytevector-n in len)))
      (when (and (not (zero? len)) (eof-object? data))
	(assertion-violation 'build-object
			     "EOF found during reading data"))
      (when (and (not (zero? len)) (< (bytevector-length data) len))
	(assertion-violation 'build-object "corrupted data"))
      (cond ((not (zero? (bitwise-and tag APPLICATION)))
	     (make-der-application-specific constructed? tag-no data))
	    ((not (zero? (bitwise-and tag TAGGED)))
	     (read-tagged-object #f (open-bytevector-input-port data)
				 constructed? tag-no))
	    (constructed?
	     (cond ((= tag-no OCTET-STRING)
		    (apply make-ber-constructed-octet-string
			   (read-objects (open-bytevector-input-port data))))
		   ((= tag-no SEQUENCE)
		    (if (zero? len)
			(make-der-sequence)
			(apply make-der-sequence
			       (read-objects
				(open-bytevector-input-port data)))))
		   ((= tag-no SET)
		    (if (zero? len)
			(make-der-set)
			(apply make-der-set
			       (read-objects 
				(open-bytevector-input-port data)))))
		   ((= tag-no EXTERNAL)
		    (apply make-der-external
			   (read-objects (open-bytevector-input-port data))))
		   (else
		    (make-der-unknown-tag #t tag-no data))))
	    (else
	     (create-primitive-der-object tag-no data)))))

  (define (read-tag-number in tag)
    (let ((tag-no (bitwise-and tag #x1f)))
      (when (= tag-no #x1f)
	(set! tag-no 0)
	(let ((b (get-u8 in)))
	  (when (zero? (bitwise-and b #x7f))
	    (assertion-violation
	     'read-tag-number
	     "corrupted stream - invalud high tag number found" b))
	  (do ((b b (get-u8 in)))
	      ((or (eof-object? b) (zero? (bitwise-and b #x80))))
	    (set! tag-no (bitwise-ior tag-no (bitwise-and b #x7f)))
	    (set! tag-no (bitwise-arithmetic-shift tag-no 7)))
	  (when (eof-object? b)
	    (assertion-violation 'read-tag-number
				 "EOF found inside tag value"))
	  (set! tag-no (bitwise-ior tag-no (bitwise-and b #x7f)))))
      tag-no))

  (define (read-length in)
    (let ((len (get-u8 in)))
      (when (eof-object? len)
	(assertion-violation 'read-length
			     "EOF found when length expected"))
      (cond ((= len #x80) -1)
	    ((> len 127)
	     (let ((size (bitwise-and len #x7f))
		   (len  0))
	       (when (> size 4)
		 (assertion-violation 'read-length
				      "DER length more than 4 bytes" size))
	       (dotimes (i size)
		 (let ((next (get-u8 in)))
		   (when (eof-object? next)
		     (assertion-violation 'read-length
					  "EOF found reading length"))
		   (set! len (+ (bitwise-arithmetic-shift len 8) next))))
	       (when (negative? len)
		 (assertion-violation 'read-length
				      "corrupted stream - negative length found"))
	       len))
	    (else len))
      ))

  (define (read-object in :optional (skip? #f))
    (let ((tag (get-u8 in)))
      (cond ((eof-object? tag) #f)
	    ((and (not skip?) (zero? tag))
	     (assertion-violation 'read-asn.1-object
				  "unexpected enf-of-contents marker"
				  (get-bytevector-all in)))
	    ((and skip? (zero? tag)) #f)
	    (else
	     (let* ((tag-no (read-tag-number in tag))
		    (constructed? (not (zero? (bitwise-and tag CONSTRUCTED))))
		    (len (read-length in)))
	       (if (negative? len)
		   (do-indefinite-length in tag tag-no constructed?)
		   (build-object tag tag-no len in)))))))

  (define (read-asn.1-object in)
    (unless (binary-port? in)
      (assertion-violation 'read-asn.1-object
			   "binary port required" in))
    (read-object in)
    )
  )