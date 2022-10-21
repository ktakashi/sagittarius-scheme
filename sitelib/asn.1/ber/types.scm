;;; -*- mode:scheme; coding:utf-8; -*-
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
    (export (rename (<ber-octet-string> <ber-constructed-octet-string>)
		    (<der-null> <ber-null>))
	    <ber-application-specific>
	    <ber-tagged-object>
	    <ber-sequence>
	    <ber-set>

	    make-ber-constructed-octet-string
	    make-ber-application-specific
	    make-ber-tagged-object
	    
	    (rename (ber-sequence make-ber-sequence)
		    (ber-set make-ber-set)
		    (make-der-null make-ber-null)))
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1))

  (define-generic make-ber-constructed-octet-string)
  (define-generic make-ber-application-specific)
  (define-generic make-ber-tagged-object)
  

  (define-method make-ber-constructed-octet-string ((b <bytevector>))
    (bytevector->ber-octed-string b))
  (define-method make-ber-constructed-octet-string l
    (list->ber-octet-string l))

  (define-method make-ber-application-specific ((tag <integer>) . l)
    (make <ber-application-specific> :tag tag
	  :constructed? #f
	  :octets (bytevector-concatenate (map asn1-encodable->bytevector l))))

  (define-method make-ber-tagged-object
    ((explicit? <boolean>) (tag-no <integer>) (obj <asn1-encodable>))
    (make <ber-tagged-object> :tag-no tag-no
	  :explicit? explicit?
	  :obj obj))
  )
