;;; -*- Scheme -*-
;;;
;;; types.scm - ASN.1 basic types
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
(library (asn.1 types)
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
     make-asn.1-type        asn.1-type?
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
     encode-tag
     ;; condition
     &asn.1-error asn.1-error?
     raise-asn.1-error
     ;; misc
     *base-types*
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
     ;;
     asn.1-sequence
     asn.1-set
     )
    (import (rnrs)
	    (srfi :19 time)
	    (sagittarius)
	    (sagittarius control))

  ;; ASN.1 tag values
  (define TAG_BOOLEAN		#x01)
  (define TAG_INTEGER		#x02)
  (define TAG_BIT_STRING	#x03)
  (define TAG_OCTET_STRING	#x04)
  (define TAG_NULL		#x05)
  (define TAG_OBJECT_IDENTIFIER	#x06)
  (define TAG_OBJECT_DESCRIPTOR #x07)
  (define TAG_EXTERNAL		#x08)
  (define TAG_REAL		#x09)
  (define TAG_ENUMERATED	#x0a)
  (define TAG_UTF8_STRING	#x0c)
  (define TAG_RELATIVE_OID	#x0d)
  (define TAG_SEQUENCE		#x10)
  (define TAG_SET		#x11)
  (define TAG_NUMERIC_STRING	#x12)
  (define TAG_PRINTABLE_STRING	#x13)
  (define TAG_TELETEX_STRING	#x14)
  (define TAG_VIDEOTEX_STRING	#x15)
  (define TAG_IA5_STRING	#x16)
  (define TAG_UTC_TIME		#x17)
  (define TAG_GENERALIZED_TIME	#x18)
  (define TAG_GRAPHIC_STRING	#x19)
  (define TAG_VISIBLE_STRING	#x1a)
  (define TAG_GENERAL_STRING	#x1b)
  (define TAG_CHARACTER_STRING	#x1c)
  (define TAG_BMP_STRING	#x1e)

  ;; ASN.1 tag classes
  (define CLASS_UNIVERSAL	#x00)
  (define CLASS_APPLICATION	#x40)
  (define CLASS_CONTEXT		#x80)
  (define CLASS_PRIVATE		#xc0)

  ;; primitive or constructive
  (define TAG_PRIMITIVE		#x00)
  (define TAG_CONSTRUCTIVE	#x20)

  (define-record-type asn.1-type
    (fields (mutable name)
	    (mutable tag)
	    (mutable type)
	    (mutable child)
	    (mutable loop)
	    (mutable optional)
	    (mutable defined-by))
    (protocol (lambda (p)
		(lambda args
		  (let-keywords args ((name #f)
				      (tag -1)
				      (type #f)
				      (child #f)
				      (defined-by #f)
				      (loop #f)
				      (optional #f))
		    (p name tag type child loop optional defined-by))))))

  (define (tag-explicit! type)
    (let ((new-op (make-asn.1-type :type (asn.1-type-type type)
				   :child (asn.1-type-child type)
				   :loop (asn.1-type-loop type)
				   :optional #f)))
      (asn.1-type-type-set! type "SEQUENCE")
      (asn.1-type-child-set! type (list new-op))
      (asn.1-type-loop-set! type #f)
      type))

  (define (tag-constructive! self)
    (when (and (asn.1-type-tag self)
	       (zero? (bitwise-and (asn.1-type-tag self)
				   TAG_CONSTRUCTIVE)))
      (asn.1-type-tag-set! self (bitwise-ior (asn.1-type-tag self)
					     TAG_CONSTRUCTIVE)))
    self)

  (define (encode-tag tag-class val)
    (define classes `(,CLASS_UNIVERSAL ,CLASS_APPLICATION ,CLASS_CONTEXT ,CLASS_PRIVATE))
    (unless (memv tag-class classes)
      (raise-asn.1-error 'encode-tag "Bad tag class" (number->string tag-class 16)))
    (unless (zero? (bitwise-and val #xffe00000))
      (raise-asn.1-error 'encode-tag "Tag value too big" (number->string val 16)))
    ;; bits: tttt tttt vvvv vvvv vvvv vvvv vvvv vvvv
    ;; t: class tag
    ;; v: value
    ;; for now we don't do any thing about encoding just merge
    ;; tag class is 1 byte
    (bitwise-and (bitwise-arithmetic-shift-left tag-class 24)
		 val))

  (define-condition-type &asn.1-error &error
    make-asn.1-error asn.1-error?)

  (define (raise-asn.1-error who message . irritants)
    (raise
     (apply condition
	    (filter values
		    (list (make-asn.1-error)
			  (and who (make-who-condition who))
			  (make-message-condition message)
			  (make-irritants-condition irritants))))))

  ;; for compiler
  (define *base-types*
    `(("BOOLEAN"      	   ,TAG_BOOLEAN           . BOOLEAN)
      ("INTEGER"      	   ,TAG_INTEGER           . INTEGER)
      ("BIT-STRING"   	   ,TAG_BIT_STRING        . BIT-STRING)
      ("OCTET-STRING" 	   ,TAG_OCTET_STRING      . STRING)
      ("STRING"       	   ,TAG_OCTET_STRING      . STRING)
      ("NULL"         	   ,TAG_NULL              . NULL)
      ("OBJECT-IDENTIFIER" ,TAG_OBJECT_DESCRIPTOR . OBJECT-IDENTIFIER)
      ("REAL"              ,TAG_REAL              . REAL)
      ("ENUMERATED"        ,TAG_ENUMERATED        . INTEGER)
      ("ENUM"              ,TAG_ENUMERATED        . INTEGER)
      ("RELATIVE-OID"      ,TAG_RELATIVE_OID      . ROID)

      ("SEQUENCE"          ,(bitwise-ior TAG_SEQUENCE TAG_CONSTRUCTIVE) . SEQUENCE)
      ("SET"               ,(bitwise-ior TAG_SET      TAG_CONSTRUCTIVE) . SET)

      ("ObjectDescriptor"  ,TAG_OBJECT_DESCRIPTOR . STRING)
      ("UTF8String"        ,TAG_UTF8_STRING       . STRING)
      ("NumericString"     ,TAG_NUMERIC_STRING    . STRING)
      ("TeletexString"     ,TAG_TELETEX_STRING    . STRING)
      ("T61String"         ,TAG_TELETEX_STRING    . STRING)
      ("VideotexString"    ,TAG_VIDEOTEX_STRING   . STRING)
      ("IA5String"         ,TAG_IA5_STRING        . STRING)
      ("UTCTime"           ,TAG_UTC_TIME          . UTIME)
      ("GeneralizedTime"   ,TAG_GENERALIZED_TIME  . GTIME)
      ("GraphicString"     ,TAG_GRAPHIC_STRING    . STRING)
      ("VisibleString"     ,TAG_VISIBLE_STRING    . STRING)
      ("ISO64String"       ,TAG_VISIBLE_STRING    . STRING)
      ("GeneralString"     ,TAG_GENERAL_STRING    . STRING)
      ("CharacterString"   ,TAG_CHARACTER_STRING  . STRING)
      ("UniversalString"   ,TAG_CHARACTER_STRING  . STRING)
      ("PrintableString"   ,TAG_PRINTABLE_STRING  . STRING)
      ("BMPString"         ,TAG_BMP_STRING        . STRING)
      ("BCDString"         ,TAG_OCTET_STRING      . BCD)

      ("CHOICE" #f . CHOICE)
      ("ANY"    #f . ANY)))

  ;; asn.1-object
  (define-record-type asn.1-object
    (fields tag
	    (mutable value)
	    ;; do we need this?
	    tagging
	    tag-class)
    (protocol
     (lambda (p)
       (lambda (tag value . opts)
	 (let-keywords opts ((tagging 'IMPLICIT)
			     (tag-class CLASS_UNIVERSAL))
	   (unless (or (eq? tagging 'IMPLICIT)
		       (eq? tagging 'EXPLICIT))
	     (assertion-violation 'make-asn.1-object
				  "invalid tagging mode" tagging))
	   (unless (or (= tag-class CLASS_UNIVERSAL)
		       (= tag-class CLASS_PRIVATE)
		       (= tag-class CLASS_CONTEXT)
		       (= tag-class CLASS_APPLICATION))
	     (assertion-violation 'make-asn.1-object
				  "invalid tag class" tag-class))
	   (p tag value tagging tag-class))))))

  ;; constructors for primitive types
  (define-syntax constructor-generator
    (lambda (x)
      (syntax-case x ()
	((k type validator tag)
	 (with-syntax ((name (datum->syntax #'k (string->symbol
						 (format "make-asn.1-~a" (syntax->datum #'type))))))
	   #'(define (name value . opts)
	       (check-arg validator value name)
	       (apply make-asn.1-object tag value opts)))))))

  (constructor-generator boolean      boolean?    TAG_BOOLEAN)
  (constructor-generator integer      integer?    TAG_INTEGER)
  (constructor-generator bit-string   string?     TAG_BIT_STRING)
  (constructor-generator octet-string bytevector? TAG_OCTET_STRING)
  (constructor-generator string       string?     TAG_STRING)
  (constructor-generator null         null?       TAG_NULL)
  ;; TODO OID check method
  (constructor-generator oid          string?     TAG_OBJECT_IDENTIFIER)
  (constructor-generator real         real?       TAG_REAL)
  (constructor-generator enum         positive?   TAG_ENUMERATED)
  ;; TODO correct?
  (constructor-generator relative-oid string?     TAG_RELATIVE_OID)

  ;; contstructive
  ;; to avoid unnecessary calculation
  ;;(define sequence_tag (bitwise-ior TAG_SEQUENCE TAG_CONSTRUCTIVE))
  ;;(define set_tag      (bitwise-ior TAG_SET      TAG_CONSTRUCTIVE))
  (define (check-vector-contents v)
    (check-arg vector? v check-vector-contents)
    (let ((l (vector->list v)))
      (for-all asn.1-object? l)))
  (constructor-generator sequence check-vector-contents TAG_SEQUENCE)
  (constructor-generator set      check-vector-contents TAG_SET)

  ;; for convenience
  (define-syntax asn.1-sequence
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((args (cdr form)))
	 `(make-asn.1-sequence (vector ,@args))))))

  (define-syntax asn.1-set
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((args (cdr form)))
	 `(make-asn.1-set (vector ,@args))))))

  ;; pre-defined string
  (constructor-generator object-descriptor string? TAG_OBJECT_DESCRIPTOR)
  (constructor-generator utf8-string       string? TAG_UTF8_STRING)
  (constructor-generator numeric-string    string? TAG_NUMERIC_STRING)
  (constructor-generator teletex-string    string? TAG_TELETEX_STRING)
  (constructor-generator t61-string        string? TAG_TELETEX_STRING)
  (constructor-generator videotex-string   string? TAG_VIDEOTEX_STRING)
  (constructor-generator ia5-string        string? TAG_IA5_STRING)
  (constructor-generator utc-time          time?   TAG_UTC_TIME)
  (constructor-generator generalized-time  time?   TAG_GENERALIZED_TIME)
  (constructor-generator general-string    string? TAG_GENERAL_STRING)
  (constructor-generator graphic-string    string? TAG_GRAPHIC_STRING)
  (constructor-generator visible-string    string? TAG_VISIBLE_STRING)
  (constructor-generator iso64-string      string? TAG_VISIBLE_STRING)
  (constructor-generator character-string  string? TAG_CHARACTER_STRING)
  (constructor-generator universal-string  string? TAG_CHARACTER_STRING)
  (constructor-generator printable-string  string? TAG_PRINTABLE_STRING)
  (constructor-generator bmp-string        string? TAG_BMP_STRING)
  (constructor-generator bcd-string        string? TAG_OCTET_STRING)
  ;; TODO do we need choice and any?

)

;; Local Variables:
;; coding: utf-8
;; End: