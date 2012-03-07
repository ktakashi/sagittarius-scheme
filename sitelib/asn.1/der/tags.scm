;;; -*- Scheme -*-
;;;
;;; types.scm - ASN.1 der/ber tags
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

(library (asn.1 der tags)
    (export BOOLEAN
	    INTEGER
	    BIT-STRING
	    OCTET-STRING
	    NULL
	    OBJECT-IDENTIFIER
	    EXTERNAL
	    ENUMERATED
	    SEQUENCE
	    SEQUENCE-OF
	    SET
	    SET-OF
	    NUMERIC-STRING
	    PRINTABLE-STRING
	    T61-STRING
	    VIDEOTEX-STRING
	    IA5-STRING
	    UTC-TIME
	    GENERALIZED-TIME
	    GRAPHIC-STRING
	    VISIBLE-STRING
	    GENERAL-STRING
	    UNIVERSAL-STRING
	    BMP-STRING
	    UTF8-STRING
	    CONSTRUCTED		
	    APPLICATION
	    TAGGED)
    (import (sagittarius))
  ;; from DERTags
  (define-constant BOOLEAN		#x01)
  (define-constant INTEGER		#x02)
  (define-constant BIT-STRING		#x03)
  (define-constant OCTET-STRING		#x04)
  (define-constant NULL			#x05)
  (define-constant OBJECT-IDENTIFIER	#x06)
  (define-constant EXTERNAL		#x08)
  (define-constant ENUMERATED		#x0a)
  (define-constant SEQUENCE		#x10)
  (define-constant SEQUENCE-OF		#x10) ; for completeness
  (define-constant SET			#x11)
  (define-constant SET-OF		#x11) ; for completeness

  (define-constant NUMERIC-STRING	#x12)
  (define-constant PRINTABLE-STRING	#x13)
  (define-constant T61-STRING		#x14)
  (define-constant VIDEOTEX-STRING	#x15)
  (define-constant IA5-STRING		#x16)
  (define-constant UTC-TIME		#x17)
  (define-constant GENERALIZED-TIME	#x18)
  (define-constant GRAPHIC-STRING	#x19)
  (define-constant VISIBLE-STRING	#x1a)
  (define-constant GENERAL-STRING	#x1b)
  (define-constant UNIVERSAL-STRING	#x1c)
  (define-constant BMP-STRING		#x1e)
  (define-constant UTF8-STRING		#x0c)

  (define-constant CONSTRUCTED		#x20)
  (define-constant APPLICATION		#x40)
  (define-constant TAGGED		#x80))