;;; -*- Scheme -*-
;;;
;;; asn.1.scm - ASN.1 library
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
(library (asn.1)
    (export
     TAG_BOOLEAN
     TAG_INTEGER
     TAG_BIT_STRING
     TAG_OCTET_STRING
     TAG_NULL
     TAG_OBJECT_IDENTIFIER
     TAG_OBJECT_DESCRIPTOR
     TAG_EXTERNAL
     TAG_REAL
     TAG_ENUMERATED
     TAG_UTF8_STRING
     TAG_RELATIVE_OID
     TAG_SEQUENCE
     TAG_SET
     TAG_NUMERIC_STRING
     TAG_PRINTABLE_STRING
     TAG_TELETEX_STRING
     TAG_VIDEOTEX_STRING
     TAG_IA5_STRING
     TAG_UTC_TIME
     TAG_GENERALIZED_TIME
     TAG_GRAPHIC_STRING
     TAG_VISIBLE_STRING
     TAG_GENERAL_STRING
     TAG_CHARACTER_STRING
     TAG_BMP_STRING

     CLASS_UNIVERSAL
     CLASS_APPLICATION
     CLASS_CONTEXT
     CLASS_PRIVATE

     TAG_PRIMITIVE
     TAG_CONSTRUCTIVE

     ;; type
     make-asn.1-type
     asn.1-type-name        asn.1-type-name-set!
     asn.1-type-tag	    asn.1-type-tag-set!
     asn.1-type-type	    asn.1-type-type-set!
     asn.1-type-child	    asn.1-type-child-set!
     asn.1-type-loop	    asn.1-type-loop-set!
     asn.1-type-optional    asn.1-type-optional-set!
     asn.1-type-defined-by  asn.1-type-defined-by-set!
     ;; util
     tag-explicit!
     tag-constructive!
     ;; condition
     &asn.1-error asn.1-error?
     &asn.1-lexical-error asn.1-lexical-error?
     condition-asn.1-lexical-token
     ;; parser/lexer
     asn.1-parser
     make-lexer

     ;; interface for normal use
     parse-asn.1
     ;; interface for expert(?) use
     verify compile
     ;; asn.1-object
     make-asn.1-object      asn.1-object?
     asn.1-object-tag
     asn.1-object-value     asn.1-object-value-set!
     asn.1-object-tagging
     asn.1-object-tag-class
     ;; constructors
     make-asn.1-boolean
     make-asn.1-integer
     make-asn.1-bit-string
     make-asn.1-octet-string
     make-asn.1-string
     make-asn.1-null
     make-asn.1-oid
     make-asn.1-real
     make-asn.1-enum
     make-asn.1-relative-oid
     make-asn.1-sequence
     make-asn.1-set
     make-asn.1-object-descriptor
     make-asn.1-utf8-string
     make-asn.1-numeric-string
     make-asn.1-teletex-string
     make-asn.1-t61-string
     make-asn.1-videotex-string
     make-asn.1-ia5-string
     make-asn.1-utc-time
     make-asn.1-generalized-time
     make-asn.1-graphic-string
     make-asn.1-general-string
     make-asn.1-visible-string
     make-asn.1-iso64-string
     make-asn.1-character-string
     make-asn.1-universal-string
     make-asn.1-printable-string
     make-asn.1-bmp-string
     make-asn.1-bcd-string
     ;; for convenience
     asn.1-sequence
     asn.1-set
     ;; encode
     encode
     )
    (import (asn.1 types)
	    (asn.1 lexer)
	    (asn.1 parser)
	    (asn.1 compiler)
	    ;;(asn.1 converter)
	    (asn.1 encode)
	    (rnrs)
	    (sagittarius control))

  (define (default-error-handler msg token)
    (error 'parse-asn.1
	   msg
	   (if (lexical-token? token)
	       (lexical-token-value token)
	       token)))

  (define-with-key (parse-asn.1 inport :key (error-handler default-error-handler))
    (let ((tree (compile (verify (asn.1-parser (make-lexer inport) error-handler)))))
      tree))


  (define-with-key (encode value :key (encoder der-encoder)
			              (tree #f))
    (encoder value :tree tree))
)

;; Local Variables:
;; coding: utf-8
;; End: