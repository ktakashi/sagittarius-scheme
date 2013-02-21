;;; -*- Scheme -*-
;;;
;;; types.scm - ASN.1 basic types
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

;; based on bouncycastle implementation
;; For now we just implemented only the part which RKCS#1 v1.5 encode requires.
(library (asn.1 types)
    (export <der-encodable>
	    <asn.1-encodable>
	    <der-object>
	    <asn.1-object>
	    <asn.1-string>
	    <asn.1-tagged-object>
	    <asn.1-sequence>
	    <asn.1-null>
	    <asn.1-octet-string>
	    <asn.1-set>
	    <asn.1-choice>

	    <der-application-specific>
	    <der-string>
	    <der-bit-string>
	    <der-bmp-string>
	    <der-octet-string>
	    <der-general-string>
	    <der-ia5-string>
	    <der-numeric-string>
	    <der-printable-string>
	    <der-t61-string>
	    <der-universal-string>
	    <der-utf8-string>
	    <der-visible-string>
	    <der-boolean>
	    <der-enumerated>
	    <der-integer>
	    <der-object-identifier>
	    <der-sequence>
	    <der-set>
	    <der-null>
	    <der-generalized-time>
	    <der-utc-time>
	    <der-tagged-object>
	    <der-external>
	    <der-unknown-tag>
	    ;; constructors
	    make-der-application-specific
	    make-der-bit-string
	    make-der-bmp-string
	    make-der-octet-string
	    make-der-general-string
	    make-der-ia5-string
	    make-der-numeric-string
	    make-der-printable-string
	    make-der-t61-string
	    make-der-universal-string
	    make-der-utf8-string
	    make-der-visible-string
	    make-der-boolean
	    make-der-enumerated
	    make-der-integer
	    make-der-object-identifier
	    make-der-sequence
	    make-der-set
	    make-der-null
	    make-der-generalized-time
	    make-der-utc-time
	    make-der-tagged-object
	    make-der-external
	    make-der-unknown-tag

	    ;;
	    asn.1-object?

	    ;; methods
	    der-encodable->der-object
	    asn.1-encodable->asn.1-object
	    asn.1-string->string
      	    asn.1-sequence-add
	    asn.1-sequence-get
	    asn.1-sequence-size
	    asn.1-set-add
	    asn.1-set-get
	    asn.1-set-size
	    der-integer->integer
	    der-encode
	    der-boolean->boolean
	    der-time->date

	    ;; misc
	    *current-indent*
	    (rename (generic-write der-generic-write))
	    der-list->string
	    )
    (import (except (rnrs) bytevector->string)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius time) ;; for <date>
	    (asn.1 der tags)
	    (asn.1 der encode)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))
  ;; for write-object
  (define *current-indent* (make-parameter #f))
  (define (generic-write class value p)
    (let ((indent (*current-indent*)))
      (parameterize ((*current-indent* (if indent (+ indent 2) 0)))
	(dotimes (i (*current-indent*))
	  (display #\space p))
	(format p "#<~a ~a>" class value))))
  ;; explicitly convert to string
  (define (bytevector->string b) 
    (list->string (map (lambda (u)
			 (integer->char (bitwise-and u #xff)))
		       (bytevector->u8-list b))))

  ;; the class hierarchy is base on bouncycastle.

  ;; DEREncodable and ASN1Encodable (abstract)
  ;; TODO I don't think we need this encodable class.
  (define-class <der-encodable> () ())
  (define-method der-encodable->der-object ((o <der-encodable>))
    (assertion-violation 
     'der-encodable->der-object
     "subclass must implement der-encodable->der-object method" o))

  (define-class <asn.1-encodable> (<der-encodable>) ())
  (define-method asn.1-encodable->asn.1-object ((o <asn.1-encodable>))
    (assertion-violation 
     'asn.1-encodable->asn.1-object
     "subclass must implement asn.1-encodable->asn.1-object method" o))
  (define-method der-encodable->der-object ((o <asn.1-encodable>))
    (asn.1-encodable->asn.1-object o))
  (define-method asn.1-encodable-encoded ((o <asn.1-encodable>))
    (call-with-bytevector-output-port
     (lambda (p)
       (der-write-object o p))))
  (define-method asn.1-encodable-encoded ((o <asn.1-encodable>)
					  (encoding <string>))
    ;; on bouncycastle I didn't see any difference. why?
    (asn.1-encodable-encoded o))
  

  ;; DERObject (abstract)
  (define-class <der-object> (<asn.1-encodable>) ())
  (define-method asn.1-encodable->asn.1-object ((o <der-object>)) o)
  (define-method der-encode ((o <der-object>) (p <port>))
    (assertion-violation 'der-encode
			 "subclass must implement der-encode method" o))

  ;; base class for all DER and BER objects
  (define-class <asn.1-object> (<der-object>) ())
  (define (asn.1-object? o) (is-a? o <asn.1-object>))
  
  ;; DER classes are the super classes for all ASN1 classes.
  ;; TODO: this hierarchy is weird for me, re-consider it.

  ;; DERApplicationSpecific
  (define-class <der-application-specific> (<asn.1-object>)
    ((constructed? :init-keyword :constructed?)
     (tag          :init-keyword :tag)
     (octets       :init-keyword :octets)))

  (define-method make-der-application-specific
    ((constructed? <boolean>) (tag <integer>) (octets <bytevector>))
    (make <der-application-specific>
      :constructed? constructed? :tag tag :octets octets))

  (define-method make-der-application-specific
    ((tag <integer>) (octets <bytevector>))
    (make-der-application-specific #f tag octets))

  (define-method make-der-application-specific
    ((explicit <boolean>) (tag <integer>) (object <der-encodable>))
    (let ((data (der-encode (der-encodable->der-object object))))
      (define (get-length-of-length data)
	(do ((count 2 (+ count 1))) ; TODO assumes only a 1 byte tag number
	    ((zero? (bitwise-and (bytevector-u8-ref data (- count 1)) #x80))
	     count)))
      (make <der-application-specific>
	:constructed? explicit :tag tag
	:octets (if explicit
		    data
		    (let* ((len (get-length-of-length data))
			   (tmp (make-bytevector
				 (- (bytevector-length data) len))))
		      (bytevector-copy! data len tmp 0 (bytevector-length tmp))
		      tmp)))
      ))
    (define-method make-der-application-specific
      ((tag <integer>) (object <der-encodable>))
      (make-der-application-specific #t tag object))

    (define-method der-encode ((o <der-application-specific>) (p <port>))
      ;; assume p is binary port
      (let ((class-bits APPLICATION))
	(when (slot-ref o 'constructed?)
	  (set! class-bits (bitwise-ior class-bits CONSTRUCTED)))
	(der-write-encoded class-bits (slot-ref o 'tag) (slot-ref o 'octets)))
      )

    ;; ASN1String
    (define-class <asn.1-string> () ())
    (define-method asn.1-string->string ((o <asn.1-string>))
      (assertion-violation 
       'asn.1-string->string
       "subclass must implement asn.1-string->string method" o))
    (define-class <der-string> (<asn.1-string>) ())

    ;; ASN1TaggedObject
    (define-class <asn.1-tagged-object> (<asn.1-object>)
      ((tag-no 	  :init-keyword :tag-no)
       (empty? 	  :init-keyword :empty? :init-value #f)
       (explicit? :init-keyword :explicit? :init-value #t)
       (obj       :init-keyword :obj)))

    ;; ASN1Sequence
    (define-class <asn.1-sequence> (<asn.1-object>)
      ((sequence :init-keyword :sequence :init-value '())))
    (define-method asn.1-sequence-add ((self <asn.1-sequence>)
				       (o <der-encodable>))
      (slot-set! self 'sequence
		 (append (slot-ref self 'sequence) (list o))))
    (define-method asn.1-sequence-get ((self <asn.1-sequence>)
				       (i <integer>))
      (list-ref (slot-ref self 'sequence) i))
    (define-method asn.1-sequence-size ((self <asn.1-sequence>))
      (length (slot-ref self 'sequence)))

    ;; ASN1Null
    (define-class <asn.1-null> (<asn.1-object>) ())

    ;; ASN1OctetString
    (define-class <asn.1-octet-string> (<asn.1-object>)
      ((string :init-keyword :string))) ;; bytevector

    ;; ASN1Set
    (define-class <asn.1-set> (<asn.1-object>)
      ((set :init-keyword :set :init-value '())))
    (define-method asn.1-set-add ((self <asn.1-set>)
				  (o <der-encodable>))
      (slot-set! self 'set (append (slot-ref self 'set) (list o))))
    (define-method asn.1-set-get ((self <asn.1-set>)
				       (i <integer>))
      (list-ref (slot-ref self 'set) i))
    (define-method asn.1-set-size ((self <asn.1-set>))
      (length (slot-ref self 'set)))

    ;; ASN1Choice
    ;; This is just a marker interface
    (define-class <asn.1-choice> () ())

    ;; DERBitString
    (define-class <der-bit-string> (<asn.1-object> <der-string>)
      ((data         :init-keyword :data)
       (padding-bits :init-keyword :padding-bits)))
    (define-method make-der-bit-string ((data <bytevector>) (pad <integer>))
      (make <der-bit-string> :data data :padding-bits pad))
    (define-method make-der-bit-string ((data <bytevector>))
      (make-der-bit-string data 0))
    (define-method make-der-bit-string ((obj <asn.1-tagged-object>)
					(explicit <boolean>))
      (let ((o (der-encodable->der-object obj)))
	(if (or explicit (is-a? o <der-bit-string>))
	    o
	    (make-der-bit-string (slot-ref o 'string)))))
    ;; encode
    (define-method der-encode ((o <der-bit-string>) (p <port>))
      (let* ((data (slot-ref o 'data))
	     (len  (bytevector-length data))
	     (bytes (make-bytevector (+ len 1))))
	(bytevector-u8-set! bytes 0 (slot-ref o 'padding-bits))
	(bytevector-copy! data 0 bytes 1 len)
	(der-write-encoded BIT-STRING bytes p)))

    (define-constant *table* "0123456789ABCDEF")
    (define-method asn.1-string->string ((o <der-bit-string>))
      (let ((string (call-with-bytevector-output-port
		     (lambda (p)
		       (der-write-object o p)))))
	(call-with-string-output-port
	 (lambda (p)
	   (dotimes (i (bytevector-length string))
	     (let ((b (bytevector-u8-ref string i)))
	       (put-char p (string-ref
			    *table*
			    (bitwise-and (bitwise-arithmetic-shift-right b 4)
					 #xf)))
	       (put-char p (string-ref *table* (bitwise-and b #xf)))))))))
    (define-method write-object ((o <der-bit-string>) (p <port>))
      (generic-write "der-bit-string" (asn.1-string->string o) p))

    
    ;; DERBMPString (assume this is UTF16)
    (define-class <der-bmp-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define-method make-der-bmp-string ((bytes <bytevector>))
      (make <der-bmp-string> :string (utf16->string bytes 'big)))
    (define-method make-der-bmp-string ((string <string>))
      (make <der-bmp-string> :string string))
    (define-method der-encode ((o <der-bmp-string>) (p <port>))
      (der-write-encoded BMP-STRING (string->utf16 (slot-ref o 'string)) p))
    (define-method asn.1-string->string ((o <der-bmp-string>))
      (slot-ref o 'string))
    (define-method write-object ((o <der-bmp-string>) (p <port>))
      (generic-write "der-bmp-string" (asn.1-string->string o) p))

    ;; DEROctetString
    (define-class <der-octet-string> (<asn.1-octet-string>) ())
    (define-method make-der-octet-string ((b <bytevector>))
      (make <der-octet-string> :string b))
    (define-method make-der-octet-string ((o <der-encodable>))
      (make-der-octet-string (asn.1-encodable-encoded
			      (asn.1-encodable->asn.1-object o))))
    (define-method der-encode ((o <der-octet-string>) (p <port>))
      (der-write-encoded OCTET-STRING (slot-ref o 'string) p))
    (define-method write-object ((o <der-octet-string>) (p <port>))
      (generic-write "der-octet-string" (slot-ref o 'string) p))

    ;; DERGeneralString
    (define-class <der-general-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define-method make-der-general-string ((s <string>))
      (make <der-general-string> :string s))
    (define-method make-der-general-string ((b <bytevector>))
      (make <der-general-string> :string (utf8->string b)))
    (define-method der-encode ((o <der-general-string>) (p <port>))
      (der-write-encoded GENERAL-STRING (string->utf8 (slot-ref o 'string))
			 p))
    (define-method asn.1-string->string ((o <der-general-string>))
      (slot-ref o 'string))
    (define-method write-object ((o <der-general-string>) (p <port>))
      (generic-write "der-general-string" (slot-ref o 'string) p))

    ;; DERIA5String
    (define-class <der-ia5-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define (ia5-string? s)
      (for-all (lambda (ch) (> (char->integer ch) #x007f))
	       (string->list s)))
    (define-method make-der-ia5-string ((s <string>))
      (make-der-ia5-string s #f))
    (define-method make-der-ia5-string ((s <string>) (validate? <boolean>))
      (when (and validate? (not (ia5-string? s)))
	(assertion-violation 'make-der-ia5-string
			     "string contains illegal characters" s))
      (make <der-ia5-string> :string s))
    (define-method make-der-ia5-string ((b <bytevector>))
      ;; this is actually incorrect, we need to convert all bytes to char,
      ;; as if bytes are latin-1. but for now
      (make-der-ia5-string (utf8->string b) #f))
    (define-method der-encode ((o <der-ia5-string>) (p <port>))
      (der-write-encoded IA5-STRING (string->utf8 (slot-ref o 'string)) p))
    (define-method asn.1-string->string ((o <der-ia5-string>))
      (slot-ref o 'string))
    (define-method write-object ((o <der-ia5-string>) (p <port>))
      (generic-write "der-ia5-string" (slot-ref o 'string) p))

    ;; DERNumericString
    (define-class <der-numeric-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define (numeric-string? s)
      (for-all (lambda (ch) (or (char<=? #\0 ch #\9)
				(char=? ch #\space)))
	       (string->list s)))
    (define-method make-der-numeric-string ((s <string>))
      (make-der-numeric-string s #f))
    (define-method make-der-numeric-string ((s <string>) (validate? <boolean>))
      (when (and validate? (not (numeric-string? s)))
	(assertion-violation 'make-der-numeric-string
			     "string contains illegal characters" s))
      (make <der-numeric-string> :string s))
    (define-method make-der-numeric-string ((b <bytevector>))
      (make-der-numeric-string (utf8->string b) #f))
    (define-method der-encode ((o <der-numeric-string>) (p <port>))
      (der-write-encoded NUMERIC-STRING (string->utf8 (slot-ref o 'string)) p))
    (define-method asn.1-string->string ((o <der-numeric-string>))
      (slot-ref o 'string))
    (define-method write-object ((o <der-numeric-string>) (p <port>))
      (generic-write "der-numeric-string" (slot-ref o 'string) p))

    ;; DERPrintableString
    (define-class <der-printable-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define (printable-string? s)
      (for-all (lambda (ch)
		 (char-set-contains? char-set:printing ch))
	       (string->list s)))
    (define-method make-der-printable-string ((s <string>))
      (make-der-printable-string s #f))
    (define-method make-der-printable-string ((s <string>)
					      (validate? <boolean>))
      (when (and validate? (not (printable-string? s)))
	(assertion-violation 'make-der-printable-string
			     "string contains illegal characters" s))
      (make <der-printable-string> :string s))
    (define-method make-der-printable-string ((b <bytevector>))
      (make-der-printable-string (utf8->string b) #f))
    (define-method der-encode ((o <der-printable-string>) (p <port>))
      (der-write-encoded PRINTABLE-STRING 
			 (string->utf8 (slot-ref o 'string)) p))
    (define-method asn.1-string->string ((o <der-printable-string>))
      (slot-ref o 'string))
    (define-method write-object ((o <der-printable-string>) (p <port>))
      (generic-write "der-printable-string" (slot-ref o 'string) p))

    ;; DERUTF16String
    (define-class <der-t61-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define-method make-der-t61-string ((s <string>))
      (make <der-t61-string> :string s))
    (define-method make-der-t61-string ((b <bytevector>))
      (make <der-t61-string> :string (utf8->string b)))
    (define-method der-encode ((o <der-t61-string>) (p <port>))
      (der-write-encoded T61-STRING (string->utf8 (slot-ref o 'string)) p))
    (define-method asn.1-string->string ((o <der-t61-string>))
      (slot-ref o 'string))
    (define-method write-object ((o <der-t61-string>) (p <port>))
      (generic-write "der-t61-string" (slot-ref o 'string) p))

    ;; DERUniversalString
    (define-class <der-universal-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define-method make-der-universal-string ((b <bytevector>))
      (make <der-universal-string> :string b))
    (define-method der-encode ((o <der-universal-string>) (p <port>))
      (der-write-encoded UNIVERSAL-STRING (slot-ref o 'string) p))
    (define-method asn.1-string->string ((o <der-universal-string>))
      (let ((string (call-with-bytevector-output-port
		     (lambda (p)
		       (der-write-object o p)))))
	(call-with-string-output-port
	 (lambda (p)
	   (dotimes (i (bytevector-length string))
	     (let ((b (bytevector-u8-ref string i)))
	       (put-char p (string-ref
			    *table*
			    (bitwise-and (bitwise-arithmetic-shift-right b 4)
					 #xf)))
	       (put-char p (string-ref *table* (bitwise-and b #xf)))))))))
    (define-method write-object ((o <der-universal-string>) (p <port>))
      (generic-write "der-universal-string" (asn.1-string->string o) p))

    ;; DERUTF8String
    (define-class <der-utf8-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define-method make-der-utf8-string ((s <string>))
      (make <der-utf8-string> :string s))
    (define-method make-der-utf8-string ((b <bytevector>))
      (make <der-utf8-string> :string (utf8->string b)))
    (define-method der-encode ((o <der-utf8-string>) (p <port>))
      (der-write-encoded UTF8-STRING (string->utf8 (slot-ref o 'string)) p))
    (define-method asn.1-string->string ((o <der-utf8-string>))
      (slot-ref o 'string))
    (define-method write-object ((o <der-utf8-string>) (p <port>))
      (generic-write "der-utf8-string" (slot-ref o 'string) p))

    ;; DERVisibleString
    (define-class <der-visible-string> (<asn.1-object> <der-string>)
      ((string :init-keyword :string)))
    (define-method make-der-visible-string ((s <string>))
      (make <der-visible-string> :string s))
    (define-method make-der-visible-string ((b <bytevector>))
      (make <der-visible-string> :string (utf8->string b)))
    (define-method der-encode ((o <der-visible-string>) (p <port>))
      (der-write-encoded VISIBLE-STRING (string->utf8 (slot-ref o 'string)) p))
    (define-method asn.1-string->string ((o <der-visible-string>))
      (slot-ref o 'string))
    (define-method write-object ((o <der-visible-string>) (p <port>))
      (generic-write "der-visible-string" (slot-ref o 'string) p))

    ;; DERBoolean
    (define-class <der-boolean> (<asn.1-object>)
      ((value :init-keyword :value)))
    (define-method make-der-boolean ((b <boolean>))
      (make <der-boolean> :value (if b #xff 0)))
    (define-method make-der-boolean ((b <bytevector>))
      (unless (= (bytevector-length b) 1)
	(assertion-violation 'make-der-boolean
			     "byte value should have 1 byte" b))
      (make <der-boolean> :value (bytevector-u8-ref b 0)))
    (define-method der-encode ((o <der-boolean>) (p <port>))
      (let ((bytes (make-bytevector 1 (slot-ref o 'value))))
	(der-write-encoded BOOLEAN bytes p)))
    (define-method write-object ((o <der-boolean>) (p <port>))
      (generic-write "der-boolean" (if (zero? (slot-ref o 'value)) #f #t) p))
    (define-method der-boolean->boolean ((o <der-boolean>))
      (not (= (slot-ref o 'value) 0)))

    ;; DEREnumerated
    (define-class <der-enumerated> (<asn.1-object>) 
      ((bytes :init-keyword :bytes)))
    (define-method make-der-enumerated ((bytes <bytevector>))
      (make <der-enumerated> :bytes bytes))
    (define-method make-der-enumerated ((value <integer>))
      (make-der-enumerated (integer->bytevector)))
    (define-method der-encode ((o <der-enumerated>) (p <port>))
      (der-write-encoded ENUMERATED (slot-ref o 'bytes) p))
    (define-method write-object ((o <der-enumerated>) (p <port>))
      (generic-write "der-enumerated" (slot-ref o 'bytes) p))

    ;; DERInteger
    (define-class <der-integer> (<asn.1-object>)
      ((bytes :init-keyword :bytes)))
    (define-method make-der-integer ((i <integer>))
      (make <der-integer> :bytes 
	    (if (zero? i) #vu8(0) (integer->bytevector i))))
    (define-method make-der-integer ((b <bytevector>))
      (make <der-integer> :bytes b))
    (define-method make-der-integer ((obj <asn.1-tagged-object>)
				     (explicit <boolean>))
      (let ((o (der-encodable->der-object obj)))
	;; assume it's <der-integer> even if it's explicit
	(cond ((or explicit (is-a? o <der-integer>)) o) 
	      ((is-a? o <asn.1-octet-string>)
	       (make-der-integer (slot-ref o 'string)))
	      (else
	       (assertion-violation 'make-der-integer
				    "invalid object was given" obj)))))

    (define-method der-integer->integer ((o <der-integer>))
      (bytevector->integer (slot-ref o 'bytes)))
    (define-method der-encode ((o <der-integer>) (p <port>))
      (der-write-encoded INTEGER (slot-ref o 'bytes) p))
    (define-method write-object ((o <der-integer>) (p <port>))
      (generic-write "der-integer"
		     (bytevector->integer (slot-ref o 'bytes)) p))

    ;; DERObjectIdentifier
    ;; helper
    (define-class <oid-tokenizer> ()
      ((oid   :init-keyword :oid)
       (index :init-keyword :index :init-value 0)))
    (define (make-oid-tokenizer s)
      (make <oid-tokenizer> :oid s))
    (define-method has-more-tokens? ((o <oid-tokenizer>))
      (slot-ref o 'index))
    (define-method next-token ((o <oid-tokenizer>))
      (let ((index (slot-ref o 'index))
	    (oid   (slot-ref o 'oid)))
	(cond ((not index) #f)
	      (else
	       (let ((end (string-index oid #\. index)))
		 (if end
		     (let ((token (substring oid index end)))
		       (slot-set! o 'index (+ end 1))
		       token)
		     (let ((token (substring oid index (string-length oid))))
		       (slot-set! o 'index #f)
		       token)))))))

    (define-class <der-object-identifier> (<asn.1-object>)
      ((identifier :init-keyword :identifier)))
    (define-method make-der-object-identifier ((s <string>))
      (define (valid-identifier? s)
	(cond ((or (< (string-length s) 3)
		   (not (char=? (string-ref s 1) #\.))) #f)
	      ((or (char<? (string-ref s 0) #\0)
		   (char>? (string-ref s 0) #\2)) #f)
	      (else
	       (let loop ((i (- (string-length s) 1))
			  (period-allowed? #f))
		 (cond ((< i 2) period-allowed?)
		       (else
			(let ((ch (string-ref s i)))
			  (cond ((char<=? #\0 ch #\9)
				 (loop (- i 1) #t))
				((char=? ch #\.)
				 (if period-allowed?
				     (loop (- i 1) #f)
				     #f))
				(else #f)))))))
	      ))
      (unless (valid-identifier? s)
	(assertion-violation 'make-der-object-identifier
			     "given string is not OID" s))
      (make <der-object-identifier> :identifier s))

    (define-method make-der-object-identifier ((bytes <bytevector>))
      (make <der-object-identifier>
	:identifier (call-with-string-output-port
		     (lambda (p)
		       (let ((value 0)
			     (first #t))
			 (dotimes (i (bytevector-length bytes))
			   (let ((b (bitwise-and (bytevector-u8-ref bytes i)
						 #xff)))
			     (set! value (+ (* value 128)
					    (bitwise-and b #x7f)))
			     (when (zero? (bitwise-and b #x80))
			       (when first
				 (case (div value 40)
				   ((0) (put-char p #\0))
				   ((1)
				    (put-char p #\1)
				    (set! value (- value 40)))
				   (else
				    (put-char p #\2)
				    (set! value (- value 80))))
				 (set! first #f))
			       (put-char p #\.)
			       (put-string p (number->string value))
			       (set! value 0)))))))))

    (define-method der-encode ((o <der-object-identifier>) (p <port>))
      (define (write-field f p)
	(let ((byte-count (div (+ (bitwise-length f) 6) 7)))
	  (cond ((zero? byte-count) (put-u8 p 0))
		(else 
		 (let ((tmp (make-bytevector byte-count)))
		   (do ((i (- byte-count 1) (- i 1))
			(f f (bitwise-arithmetic-shift-right f 7)))
		       ((< i 0) #t)
		     (bytevector-u8-set! 
		      tmp i (bitwise-ior (bitwise-and f #x7f) #x80)))
		   (bytevector-u8-set!
		    tmp (- byte-count 1)
		    (bitwise-and (bytevector-u8-ref tmp (- byte-count 1)) #x7f))
		   (put-bytevector p tmp)
		   ))))
	)
      (der-write-encoded
       OBJECT-IDENTIFIER
       (call-with-bytevector-output-port
	(lambda (p)
	  (let ((tok (make-oid-tokenizer (slot-ref o 'identifier))))
	    (write-field (+ (* (string->number (next-token tok)) 40)
			    (string->number (next-token tok))) p)
	    (let loop ()
	      (when (has-more-tokens? tok)
		(let ((token (next-token tok)))
		  (write-field (string->number token) p))
		(loop)))
	    )
	  ))
       p))
    (define-method write-object ((o <der-object-identifier>) (p <port>))
      (generic-write "der-object-identifier" (slot-ref o 'identifier)  p))
    (define-method object-equal? ((x <der-object-identifier>)
				  (y <der-object-identifier>))
      (string=? (slot-ref x 'identifier) (slot-ref y 'identifier)))

    ;; DERSequence
    (define-class <der-sequence> (<asn.1-sequence>) ())
    (define-method make-der-sequence () (make <der-sequence>))    
    (define-method make-der-sequence ((o <der-encodable>))
      (make <der-sequence> :sequence (list o)))
    (define-method make-der-sequence l
      (or (for-all (lambda (o) (is-a? o <der-encodable>)) l)
	  (assertion-violation 'make-der-sequcne
			       "list of <der-encodable> required" l))
      (make <der-sequence> :sequence l))
    (define-method der-encode ((o <der-sequence>) (p <port>))
      (der-write-encoded
       (bitwise-ior SEQUENCE CONSTRUCTED)
       (call-with-bytevector-output-port
	(lambda (p)
	  (let loop ((l (slot-ref o 'sequence)))
	    (unless (null? l)
	      (der-write-object (car l) p)
	      (loop (cdr l))))))
       p))
    (define (der-list->string dl)
      (call-with-string-output-port
       (lambda (p)
	 (define (pl o) (display o p) (newline p))
	 (let ((indent (*current-indent*)))
	   (parameterize ((*current-indent* (if indent (+ indent 2) 0)))
	     (newline p)
	     (for-each pl dl)
	     (dotimes (i (*current-indent*)) (display #\space p)))))))
    
    (define-method write-object ((o <der-sequence>) (p <port>))
      (generic-write "der-sequence"
		     (der-list->string (slot-ref o 'sequence)) p))

    ;; DERSet
    (define-class <der-set> (<asn.1-set>) ())
    (define-method initialize ((self <der-set>) initargs)
      (define (get-encoded encodable)
	(call-with-bytevector-output-port
	 (lambda (p)
	   (der-write-object encodable p))))
      ;; sort
      (call-next-method)
      (when (get-keyword :need-sort? initargs #t)
	(let ((set (slot-ref self 'set)))
	  (slot-set! self 'set
		     (list-sort (lambda (a b)
				  (let* ((ea (get-encoded a))
					 (eb (get-encoded b))
					 (len (min (bytevector-length ea)
						   (bytevector-length eb))))
				    (let loop ((i 0))
				      (cond ((= i len)
					     (= len (bytevector-length ea)))
					    ((not (= (bytevector-u8-ref ea i)
						     (bytevector-u8-ref eb i)))
					     (< (bytevector-u8-ref ea i)
						(bytevector-u8-ref eb i)))
					    (else (loop (+ i 1))))
				      ))) set)))))
    (define-method make-der-set () (make <der-set>))
    (define-method make-der-set ((o <der-encodable>))
      (make <der-set> :set (list o)))
    (define-method make-der-set l
      (or (for-all (lambda (o) (is-a? o <der-encodable>)) l)
	  (assertion-violation 'make-der-set
			       "list of <der-encodable> required" l))
      (make <der-set> :set l))
    (define-method der-encode ((o <der-set>) (p <port>))
      (der-write-encoded
       (bitwise-ior SET CONSTRUCTED)
       (call-with-bytevector-output-port
	(lambda (p)
	  (let loop ((l (slot-ref o 'set)))
	    (unless (null? l)
	      (der-write-object (car l) p)
	      (loop (cdr l))))))
       p))
    (define-method write-object ((o <der-set>) (p <port>))
      (generic-write "der-set" (der-list->string (slot-ref o 'set)) p))

    ;; DERNull
    (define-class <der-null> (<asn.1-null>) ())
    (define *der-null* (make <der-null>))
    (define-method make-der-null () *der-null*)
    (define-method der-encode ((o <der-null>) (p <port>))
      (der-write-encoded NULL #vu8() p))
    (define-method write-object ((o <der-null>) (p <port>))
      (generic-write "der-null" "" p))

    ;; DERGeneralizedTime
    (define-class <der-generalized-time> (<asn.1-object>)
      ((time :init-keyword :time)))
    ;; helper
    (define (get-time s) #t) ;; dummy
    (define-method make-der-generalized-time ((s <string>))
      (unless (get-time s)
	(assertion-violation 'make-der-generalized-time
			     "invalid time format" s))
      (make <der-generalized-time> :time s))
    (define-method make-der-generalized-time ((b <bytevector>))
      (let ((s (utf8->string b)))
	(unless (get-time s)
	  (assertion-violation 'make-der-generalized-time
			       "invalid time format" s))
	(make <der-generalized-time> :time s)))
    (define-method make-der-generalized-time ((t <date>))
      (make <der-generalized-time>
	:time (date->string t "~Y~m~d~H~M~S'~z'")))
    (define-method der-encode ((o <der-generalized-time>) (p <port>))
      (der-write-encoded GENERALIZED-TIME (string->utf8 (slot-ref o 'time)) p))
    (define-method write-object ((o <der-generalized-time>) (p <port>))
      (generic-write "der-generalized-time" (slot-ref o 'time) p))
    (define-method der-time->date ((o <der-generalized-time>))
      (define (has-fraction-seconds time)
	(let ((i (string-index time #\.)))
	  (if i
	      (= i 14)
	      #f)))
      (define (get-format time)
	(cond ((string-suffix? "Z" time)
	       (if (has-fraction-seconds time)
		   "~Y~m~d~H~M~S.~~~~~~'~z'"
		   "~Y~m~d~H~M~S'~z'"))
	      ((or (string-index time #\-)
		   (string-index time #\+))
	       (assertion-violation 'der-time->date
				    "not supported yet"))
	      (else
	       (if (has-fraction-seconds time)
		   "~Y~m~d~H~M~S.~~~~~~"
		   "~Y~m~d~H~M~S"))))
      (let ((time (slot-ref o 'time)))
	(string->date time (get-format time))))

    ;; DERUTCTime
    (define-class <der-utc-time> (<asn.1-object>)
      ((time :init-keyword :time)))
    (define-method make-der-utc-time ((s <string>))
      (string->date s "~Y~m~d~H~M~S~z") ;; check
      (make <der-utc-time> :time s))
    (define-method make-der-utc-time ((d <date>))
      (make <der-utc-time> :time (date->string d "~Y~m~d~H~M~S~z")))
    (define-method make-der-utc-time ((b <bytevector>))
      (make <der-utc-time> :time (bytevector->string b)))
    (define-method der-encode ((o <der-utc-time>) (p <port>))
      (der-write-encoded UTC-TIME (string->utf8 (slot-ref o 'time)) p))
    (define-method write-object ((o <der-utc-time>) (p <port>))
      (generic-write "der-utc-time" (slot-ref o 'time) p))
    (define-method der-time->date ((o <der-utc-time>))
      (string->date (slot-ref o 'time) "~y~m~d~H~M~S~z"))


    ;; Tagged object
    ;; TODO should we create object parser?
    (define-class <der-tagged-object> (<asn.1-tagged-object>) ())
    (define-method make-der-tagged-object
      ((explicit? <boolean>) (tag-no <integer>) (obj <der-encodable>))
      (make <der-tagged-object> 
	:tag-no tag-no
	:explicit? (if (is-a? obj <asn.1-choice>) #t explicit?)
	:obj obj))
    (define-method make-der-tagged-object
      ((tag-no <integer>) (obj <der-encodable>))
      (make <der-tagged-object> :tag-no tag-no :obj obj))
    (define-method make-der-tagged-object ((tag-no <integer>))
      (make <der-tagged-object> 
	:explicit? #f :tag-no tag-no :obj (make-der-sequence)))
    (define-method der-encode ((o <der-tagged-object>) (p <port>))
      (cond ((slot-ref o 'empty?)
	     (der-write-encoded (bitwise-ior CONSTRUCTED TAGGED)
				(slot-ref o 'tag-no) #vu8() p))
	    (else
	     (let* ((obj (slot-ref o 'obj))
		    (bytes (asn.1-encodable-encoded
			    (der-encodable->der-object obj))))
	       (cond ((slot-ref o 'explicit?)
		      (der-write-encoded (bitwise-ior CONSTRUCTED TAGGED)
					 (slot-ref o 'tag-no) bytes p))
		     (else
		      (let ((flag (cond ((zero? (bitwise-and
						 (bytevector-u8-ref bytes 0)
						 CONSTRUCTED))
					 (bitwise-ior CONSTRUCTED TAGGED))
					(else TAGGED))))
			(der-write-tag flag (slot-ref o 'tag-no) p)
			(put-bytevector p bytes 1 (- (bytevector-length bytes)
						     1)))
		      ))))))
    (define-method write-object ((o <der-tagged-object>) (p <port>))
      (generic-write "der-tagged-object" 
		     (format "[~a] ~a~a" (slot-ref o 'tag-no)
					 (slot-ref o 'explicit?)
					 (der-list->string 
					  (list (slot-ref o 'obj))))
		     p))
    (define-method der-encodable->der-object ((o <der-tagged-object>))
      (asn.1-encodable->asn.1-object (slot-ref o 'obj)))

    ;; DERExternal
    ;; TODO direct-reference, indirect-reference and data-value-descriptor
    ;;      can be NULL.
    (define-class <der-external> (<asn.1-object>)
      ((direct-reference      :init-keyword :direct-reference)
       (indirect-reference    :init-keyword :indirect-reference)
       (data-value-descriptor :init-keyword :data-value-descriptor)
       (encoding              :init-keyword :encoding)
       (external-content      :init-keyword :external-content)))
    (define-method make-der-external
      ((dr <der-object-identifier>) (idr <der-integer>) (dvd <asn.1-object>)
       (ed <der-tagged-object>))
      (make-der-external dr idr dvd (slot-ref ed 'tag-no)
			 (der-encodable->der-object ed)))
    (define-method make-der-external
      ((dr <der-object-identifier>) (idr <der-integer>) (dvd <asn.1-object>)
       (encoding <integer>) (ed <der-object>))
      (make <der-external>
	:direct-reference dr
	:indirect-reference idr
	:data-value-descriptor dvd
	:encoding encoding
	:external-content (der-encodable->der-object ed)))

    (define-method der-encode ((o <der-external>) (p <port>))
      (let ((dr  (slot-ref o 'direct-reference))
	    (idr (slot-ref o 'indirect-reference))
	    (dvd (slot-ref o 'data-value-descriptor))
	    (obj (make-der-tagged-object (slot-ref o 'encoding)
					 (slot-ref o 'external-content))))
	(der-write-encoded CONSTRUCTED EXTERNAL
			   (call-with-bytevector-output-port
			    (lambda (p)
			      (der-encode dr p)
			      (der-encode idr p)
			      (der-encode dvd p)
			      (der-encode obj p)))
			   p)))
    (define-method write-object ((o <der-external>) (p <port>))
      (generic-write "der-external" 
		     (der-list->string (list (slot-ref o 'direct-reference)
					     (slot-ref o 'indirect-reference)
					     (slot-ref o 'data-value-descriptor)
					     (slot-ref o 'encoding)
					     (slot-ref o 'external-content)))
		     p))

    ;; unknown tag. we insert one of these when we find a tag we don't know
    (define-class <der-unknown-tag> (<der-object>)
      ((constructed? :init-keyword :constructed?)
       (tag          :init-keyword :tag)
       (data         :init-keyword :data)))
    (define-method make-der-unknown-tag ((b <boolean>) (t <integer>)
					 (data <bytevector>))
      (make <der-unknown-tag> :constructed? b :tag t :data data))
    (define-method make-der-unknown-tag ((t <integer>) (data <bytevector>))
      (make <der-unknown-tag> :constructed? #f :tag t :data data))
    (define-method der-encode ((o <der-unknown-tag>) (p <port>))
      (der-write-encoded (if (slot-ref o 'constructed?)
			     CONSTRUCTED
			     0)
			 (slot-ref o 'tag)
			 (slot-ref o 'data)
			 p))
    (define-method write-object ((o <der-unknown-tag>) (p <port>))
      (generic-write "der-unknown-tag" 
		     (format "~a ~a ~a" (slot-ref o 'constructed?)
					(slot-ref o 'tag)
					(slot-ref o 'data))
		     p))
    )

;; Local Variables:
;; coding: utf-8
;; End: