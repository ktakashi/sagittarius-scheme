;;; -*- Scheme -*-
;;;
;;; types.scm - ASN.1 basic types
;;;
;;;   Copyright (c) 2009-2022  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (asn.1 types)
    (export (rename (<asn1-encodable> <der-encodable>)
		    (<asn1-encodable> <asn.1-encodable>)
		    (<asn1-object> <der-object>)
		    (<asn1-object> <asn.1-object>)
		    (<asn1-string> <asn.1-string>)
		    ;; a bit weird
		    (<ber-tagged-object> <asn.1-tagged-object>)
		    (<ber-sequence> <asn.1-sequence>)
		    (<der-null> <asn.1-null>)
		    (<ber-octet-string> <asn.1-octet-string>)
		    (<ber-set> <asn.1-set>))
	    ;; dummy, never used it anyway
	    <asn.1-choice>
	    ;; re-exported classes
	    <der-application-specific>
	    (rename (<asn1-string> <der-string>))
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
	    ;; unfortunately no choice... will be removed or deprecated
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
	    (rename (generic-make-der-integer make-der-integer))
	    make-der-object-identifier
	    (rename (der-sequence make-der-sequence)
		    (der-set make-der-set))
	    make-der-null
	    make-der-generalized-time
	    make-der-utc-time
	    make-der-tagged-object
	    make-der-external
	    make-der-unknown-tag

	    ;;
	    (rename (asn1-object? asn.1-object?)
		    (asn1-encodable? asn.1-encodable?))

	    ;; methods
	    (rename (asn1-encodable->asn1-object der-encodable->der-object)
		    (asn1-encodable->asn1-object asn.1-encodable->asn.1-object)
		    (asn1-string->string asn.1-string->string)
		    (asn1-collection-add! asn.1-sequence-add)
		    (asn1-collection-ref asn.1-sequence-get)
		    (asn1-collection-length asn.1-sequence-size)
		    (asn1-collection-add! asn.1-set-add)
		    (asn1-collection-ref asn.1-set-get)
		    (asn1-collection-length asn.1-set-size))

	    der-integer->integer
	    (rename (asn1-encodable->asn1-object der-encode))
	    der-boolean->boolean
	    (rename (asn1-time->date der-time->date))

	    (rename (ber-octet-string->bytevector der-octet-string-octets))

	    (rename (asn1-collection-find asn.1-collection-find)
		    (asn1-collection-find-tag asn.1-collection-find-tag))
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius control)
	    (sagittarius time) ;; for <date>
	    (sagittarius crypto asn1)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))

  (define-class <asn.1-choice> (<asn1-encodable>) ())
  
  (define-generic make-der-application-specific)
  (define-generic make-der-bit-string)
  (define-generic make-der-bmp-string)
  (define-generic make-der-octet-string)
  (define-generic make-der-general-string)
  (define-generic make-der-ia5-string)
  (define-generic make-der-numeric-string)
  (define-generic make-der-printable-string)
  (define-generic make-der-t61-string)
  (define-generic make-der-universal-string)
  (define-generic make-der-utf8-string)
  (define-generic make-der-visible-string)
  (define-generic make-der-boolean)
  (define-generic make-der-enumerated)
  (define-generic make-der-integer)
  (define-generic make-der-object-identifier)
  (define-generic make-der-generalized-time)
  (define-generic make-der-utc-time)
  (define-generic make-der-tagged-object)
  (define-generic make-der-external)
  (define-generic make-der-unknown-tag)

  ;;fuck...
  (define-generic generic-make-der-integer)
  (define-method generic-make-der-integer ((i <integer>))
    (integer->der-integer i))
  (define-method generic-make-der-integer ((b <bytevector>))
    (bytevector->der-integer b))
  (define-method generic-make-der-integer ((obj <ber-tagged-object>) explicit?)
    (let ((o (asn1-encodable->asn1-object obj)))
      ;; assume it's <der-integer> even if it's explicit
      (cond ((or explicit? (is-a? o <der-integer>)) o) 
	    ((is-a? o <ber-octet-string>)
	     (make-der-integer (ber-octet-string->bytevector o)))
	    (else
	     (assertion-violation 'make-der-integer
				  "invalid object was given" obj)))))
  
  (define-method make-der-application-specific
    ((constructed? <boolean>) (tag <integer>) (octets <bytevector>))
    (make <der-application-specific>
      :constructed? constructed? :tag tag :octets octets))

  (define-method make-der-application-specific
    ((tag <integer>) (octets <bytevector>))
    (make-der-application-specific #f tag octets))

  (define-method make-der-application-specific
    ((explicit <boolean>) (tag <integer>) (object <asn1-encodable>))
    (let ((data (asn1-encodable->bytevector object)))
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
		      tmp)))))
  (define-method make-der-application-specific
    ((tag <integer>) (object <asn1-encodable>))
    (make-der-application-specific #t tag object))

  (define-method make-der-bit-string ((data <bytevector>) (pad <integer>))
    (bytevector->der-bit-string data pad))
  (define-method make-der-bit-string ((data <bytevector>))
    (bytevector->der-bit-string data))
  (define-method make-der-bit-string ((obj <ber-tagged-object>)
				      (explicit <boolean>))
    (let ((o (asn1-encodable->asn1-object obj)))
      (if (or explicit (is-a? o <der-bit-string>))
	  o
	  (bytevector->der-bit-string
	   ;; not sure why it's working...
	   (der-octet-string->bytevector o)))))

  (define-method make-der-bmp-string ((bytes <bytevector>))
    (bytevector->der-bmp-string bytes))
  (define-method make-der-bmp-string ((string <string>))
    (string->der-bmp-string string))

  (define-method make-der-octet-string ((b <bytevector>))
    (bytevector->der-octet-string b))
  (define-method make-der-octet-string ((o <asn1-encodable>))
    (bytevector->der-octet-string (asn1-encodable->bytevector o)))

  (define-method make-der-general-string ((s <string>))
    (string->der-general-string s))
  (define-method make-der-general-string ((b <bytevector>))
    (bytevector->der-generalized-time b))

  (define-method make-der-ia5-string ((s <string>))
    (string->der-ia5-string s))
  (define-method make-der-ia5-string ((b <bytevector>))
    (bytevector->der-ia5-string b))

  (define-method make-der-numeric-string ((s <string>))
    (string->der-numeric-string s))
  (define-method make-der-numeric-string ((b <bytevector>))
    (bytevector->der-numeric-string b))

  (define-method make-der-printable-string ((s <string>))
    (string->der-printable-string s))
  (define-method make-der-printable-string ((b <bytevector>))
    (bytevector->der-printable-string b))
  (define-method make-der-printable-string ((s <string>) validate?)
    (string->der-printable-string s))

  (define-method make-der-t61-string ((s <string>))
    (string->der-t61-string s))
  (define-method make-der-t61-string ((b <bytevector>))
    (bytevector->der-t61-string b))

  (define-method make-der-universal-string ((b <bytevector>))
    (bytevector->der-universal-string b))

  (define-method make-der-utf8-string ((s <string>))
    (string->der-utf8-string s))
  (define-method make-der-utf8-string ((b <bytevector>))
    (bytevector->der-utf8-string b))

  (define-method make-der-visible-string ((s <string>))
    (string->der-visible-string s))
  (define-method make-der-visible-string ((b <bytevector>))
    (bytevector->der-visible-string b))

  (define-method make-der-boolean ((b <boolean>))
    (boolean->der-boolean b))
  (define-method make-der-boolean ((b <bytevector>))
    (bytevector->der-boolean b))

  (define-method make-der-enumerated ((bytes <bytevector>))
    (bytevector->der-enumerated bytes))
  (define-method make-der-enumerated ((value <integer>))
    (integer->der-enumerated value))

  (define-method make-der-integer ((i <integer>))
    (integer->der-integer i))
  (define-method make-der-integer ((b <bytevector>))
    (bytevector->der-integer b))
  (define-method make-der-integer ((obj <ber-tagged-object>)
				   (explicit <boolean>))
    (let ((o (asn1-encodable->asn1-object obj)))
      ;; assume it's <der-integer> even if it's explicit
      (cond ((or explicit (is-a? o <der-integer>)) o) 
	    ((is-a? o <ber-octet-string>)
	     (bytevector->der-integer
	      (ber-octet-string->bytevector o)))
	    (else
	     (assertion-violation 'make-der-integer
				  "invalid object was given" obj)))))

  (define-method make-der-object-identifier ((s <string>))
    (oid-string->der-object-identifier s))

  (define-method make-der-object-identifier ((bytes <bytevector>))
    (bytevector->der-object-identifier bytes))

 ;; dummy
  (define-method make-der-generalized-time ((s <string>))
    (string->der-generalized-time s))
  (define-method make-der-generalized-time ((b <bytevector>))
    (bytevector->der-generalized-time b))
  (define-method make-der-generalized-time ((t <date>))
    (date->der-generalized-time t))

  (define-method make-der-utc-time ((s <string>))
    (string->der-utc-time s))
  (define-method make-der-utc-time ((d <date>))
    (date->der-utc-time d))
  (define-method make-der-utc-time ((b <bytevector>))
    (bytevector->der-utc-time b))

  (define-method make-der-tagged-object
    ((explicit? <boolean>) (tag-no <integer>) (obj <asn1-encodable>))
    (make <der-tagged-object> 
      :tag-no tag-no
      :explicit? explicit?
      :obj obj))
  (define-method make-der-tagged-object
    ((tag-no <integer>) (obj <asn1-encodable>))
    (make <der-tagged-object> :tag-no tag-no :obj obj :explicit? #f))
  (define-method make-der-tagged-object ((tag-no <integer>))
    (make <der-tagged-object> 
      :explicit? #f :tag-no tag-no :obj (der-sequence)))
  ;; why...
  (define-method asn1-encodable->asn1-object ((o <ber-tagged-object>))
    (ber-tagged-object-obj o))

  (define-method make-der-external
    ((dr <der-object-identifier>) (idr <der-integer>) (dvd <asn1-object>)
     (ed <der-tagged-object>))
    (make-der-external dr idr dvd (slot-ref ed 'tag-no) ed))
  (define-method make-der-external
    ((dr <der-object-identifier>) (idr <der-integer>) (dvd <asn1-object>)
     (encoding <integer>) (ed <asn1-encodable>))
    (make <der-external>
      :direct-reference dr
      :indirect-reference idr
      :data-value-descriptor dvd
      :encoding ed))

  (define-method make-der-unknown-tag ((b <boolean>) (t <integer>)
				       (data <bytevector>))
    (make <der-unknown-tag> :constructed? b :number t :data data))
  (define-method make-der-unknown-tag ((t <integer>) (data <bytevector>))
    (make <der-unknown-tag> :constructed? #f :number t :data data))

  )

;; Local Variables:
;; coding: utf-8
;; End:
