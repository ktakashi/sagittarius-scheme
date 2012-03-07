;;; -*- Scheme -*-
;;;
;;; types.scm - ASN.1 der encoder
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

(library (asn.1 der encode)
    (export der-write-object
	    der-write-encoded
	    der-write-tag
	    der-encode)
    (import (rnrs)
	    (clos user)
	    (asn.1 der tags))
  (define-generic der-encode)
  (define (write-null p)
    (put-u8 p NULL)
    (put-u8 p #x00))

  (define (write-length len p)
    (cond ((> len 127)
	   (let ((size (do ((size 1 (+ size 1))
			    (val (bitwise-arithmetic-shift-right len 8)
				 (bitwise-arithmetic-shift-right val 8)))
			   ((zero? val) size))))
	     (put-u8 p (bitwise-ior size #x80))
	     (do ((i (* (- size 1) 8) (- i 8)))
		 ((< i 0))
	       (put-u8 p (bitwise-and (bitwise-arithmetic-shift-right len i)
				      #xff)))))
	  (else (put-u8 p len))))

  (define-method der-write-object ((o <object>) (p <port>))
    (if (null? o)			;; TODO '() or #f?
	(write-null p)
	(der-encode o p)))

  (define-method der-write-encoded
    ((tag <integer>) (bytes <bytevector>) (p <port>))
    ;; assume tag is u8
    (put-u8 p tag)
    (write-length (bytevector-length bytes) p)
    (put-bytevector p bytes))

  (define-method der-write-encoded
    ((flags <integer>) (tag-no <integer>) (bytes <bytevector>) (p <port>))
    (der-write-tag flags tag-no p)
    (write-length (bytevector-length bytes) p)
    (put-bytevector p bytes))

  (define (der-write-tag flags tag-no p)
    (cond ((< tag-no 31) (put-u8 p (bitwise-ior flags tag-no)))
	  (else
	   (put-u8 p (bitwise-ior flags #x1f))
	   (cond ((< tag-no 128) (put-u8 p tag-no))
		 (else 
		  (let ((stack (make-bytevector 5))
			(pos 4))
		    (bytevector-u8-set! stack pos (bitwise-and tag-no #x7f))
		    (do ((tag-no (bitwise-arithmetic-shift-right tag-no 7)
				 (bitwise-arithmetic-shift-right tag-no 7))
			 (pos (- pos 1) (- pos 1)))
			((<= tag-no 127)
			 (put-bytevector p stack
					 (- (bytevector-length stack) pos)))
		      (bytevector-u8-set! stack pos 
					  (bitwise-ior (bitwise-and tag-no #x7f)
						       #x80)))))))))
  )