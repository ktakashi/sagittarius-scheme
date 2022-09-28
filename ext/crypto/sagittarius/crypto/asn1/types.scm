;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/asn1/types.scm - ASN.1 types
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius crypto asn1 types)
    (export asn1-encodable? <asn1-encodable>
	    write-asn1-encodable asn1-encodable->bytevector
	    asn1-encodable->asn1-object
	    asn1-encode-type asn1-encode-type?

	    asn1-object? <asn1-object>

	    asn1-simple-object? <asn1-simple-object> asn1-simple-object-value
	    asn1-string? <asn1-string> asn1-string->string
	    asn1-collection? <asn1-collection> asn1-collection-elements
	    asn1-collection-ref
	    asn1-collection-add!

	    asn1-time? <asn1-time>
	    asn1-time->date
	    
	    der-boolean? <der-boolean>
	    boolean->der-boolean der-boolean->boolean
	    bytevector->der-boolean
	    
	    der-integer? <der-integer>
	    integer->der-integer der-integer->integer
	    bytevector->der-integer

	    der-bit-string? <der-bit-string> der-bit-string-padding-bits
	    bytevector->der-bit-string
	    der-bit-string->bytevector

	    der-octet-string? <der-octet-string>
	    bytevector->der-octet-string der-octet-string->bytevector

	    der-null? <der-null>
	    make-der-null

	    der-object-identifier? <der-object-identifier>
	    bytevector->der-object-identifier
	    oid-string->der-object-identifier
	    der-object-identifier->oid-string

	    der-external? <der-external>
	    der-external-direct-reference
	    der-external-indirect-reference
	    der-external-data-value-descriptor
	    der-external-encoding

	    der-enumerated? <der-enumerated>
	    bytevector->der-enumerated
	    integer->der-enumerated
	    der-enumerated->integer

	    der-sequence? <der-sequence>
	    make-der-sequence der-sequence
	    der-sequence-of
	    der-sequence-add!

	    der-set? <der-set>
	    make-der-set der-set
	    der-set-of
	    der-set-add!

	    der-numeric-string? <der-numeric-string>
	    string->der-numeric-string
	    der-numeric-string->string
	    bytevector->der-numeric-string

	    der-printable-string? <der-printable-string>
	    string->der-printable-string
	    der-printable-string->string
	    bytevector->der-printable-string

	    der-t61-string? <der-t61-string>
	    string->der-t61-string
	    der-t61-string->string
	    bytevector->der-t61-string

	    der-videotex-string? <der-videotex-string>
	    string->der-videotex-string
	    der-videotex-string->string
	    bytevector->der-videotex-string

	    der-ia5-string? <der-ia5-string>
	    string->der-ia5-string
	    der-ia5-string->string
	    bytevector->der-ia5-string

	    der-utc-time? <der-utc-time>
	    string->der-utc-time
	    date->der-utc-time
	    bytevector->der-utc-time
	    der-utc-time->string
	    der-utc-time->date

	    der-generalized-time? <der-generalized-time>
	    string->der-generalized-time
	    date->der-generalized-time
	    bytevector->der-generalized-time
	    der-generalized-time->string
	    der-generalized-time->date
	    
	    der-graphic-string? <der-graphic-string>
	    string->der-graphic-string
	    der-graphic-string->string
	    bytevector->der-graphic-string

	    der-visible-string? <der-visible-string>
	    string->der-visible-string
	    der-visible-string->string
	    bytevector->der-visible-string

	    der-general-string? <der-general-string>
	    string->der-general-string
	    der-general-string->string
	    bytevector->der-general-string

	    der-universal-string? <der-universal-string>
	    string->der-universal-string
	    der-universal-string->string
	    bytevector->der-universal-string

	    der-bmp-string? <der-bmp-string>
	    string->der-bmp-string
	    der-bmp-string->string
	    bytevector->der-bmp-string

	    der-utf8-string? <der-utf8-string>
	    string->der-utf8-string
	    der-utf8-string->string
	    bytevector->der-utf8-string

	    der-application-specific? <der-application-specific>
	    der-application-specific-constructed?
	    der-application-specific-tag
	    der-application-specific-octets
	    
	    der-tagged-object? <der-tagged-object>
	    der-tagged-object-tag-no
	    der-tagged-object-explicit?
	    der-tagged-object-obj

	    der-unknown-tag? <der-unknown-tag>
	    der-unknown-tag-constructed?
	    der-unknown-tag-number
	    der-unknown-tag-data

	    ber-octet-string? <ber-octet-string>
	    ber-octet-string-octs
	    bytevector->ber-octed-string
	    list->ber-octet-string

	    ber-tagged-object? <ber-tagged-object>

	    <ber-sequence> ber-sequence?
	    make-ber-sequence
	    ber-sequence

	    <ber-set> ber-set?
	    make-ber-set
	    ber-set)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :19 time)
	    (srfi :117 list-queues))

;; For other objects, like X.509 certificate
(define-enumeration asn1-encode-type (der ber)
  asn1-encode-types)
(define *asn1-encode-types* (enum-set-universe (asn1-encode-types)))
(define (asn1-encode-type? o) (enum-set-member? o *asn1-encode-types*))

(define-class <asn1-encodable> () ())
(define (asn1-encodable? o) (is-a? o <asn1-encodable>))
(define-generic write-asn1-encodable)
(define-generic asn1-encodable->asn1-object)
(define-method write-asn1-encodable ((o <asn1-encodable>))
  (write-asn1-encodable o (current-output-port)))
(define-method write-asn1-encodable ((o <asn1-encodable>) (p <port>))
  (write-asn1-encodable o p 'der))
(define-method write-asn1-encodable ((o <asn1-encodable>) (p <port>) type)
  (write-asn1-encodable (asn1-encodable->asn1-object o 'der) p type))
(define (asn1-encodable->bytevector (encodable asn1-encodable?)
				    :optional ((type asn1-encode-type?) 'der))
  (let-values (((out e) (open-bytevector-output-port)))
    (write-asn1-encodable encodable out type)
    (e)))

;; Actual DER / BER object
(define-class <asn1-object> (<asn1-encodable> <immutable>) ())
(define (asn1-object? o) (is-a? o <asn1-object>))
(define-method asn1-encodable->asn1-object ((o <asn1-object>) type) o)

;; Simple value, not a container
(define-class <asn1-simple-object> (<asn1-object>)
  ((value :init-keyword :value :reader asn1-simple-object-value)))
(define (asn1-simple-object? o) (is-a? o <asn1-simple-object>))
(define-method object-equal? ((a <asn1-simple-object>) (b <asn1-simple-object>))
  (equal? (asn1-simple-object-value a) (asn1-simple-object-value b)))

;; ASN.1 string, e.g. IA5
(define-class <asn1-string> (<asn1-simple-object>) ())
(define (asn1-string? o) (is-a? o <asn1-string>))
(define-generic asn1-string->string)
(define (make-bytevector->asn1-string ctr :optional (conv utf8->string))
  (lambda ((bv bytevector?)) (ctr (conv bv))))

;; Base collection
(define-class <asn1-collection> (<asn1-object>)
  ;; We use list queue for the collection elements
  ((elements :init-keyword :elements :reader asn1-collection-elements
	     :init-thunk list-queue)))
(define (asn1-collection? o) (is-a? o <asn1-collection>))
(define (asn1-collection-subtype? class) (subtype? class <asn1-collection>))
(define-method object-equal? ((a <asn1-collection>) (b <asn1-collection>))
  (for-all equal?
	   (list-queue-list (asn1-collection-elements a))
	   (list-queue-list (asn1-collection-elements b))))
  
(define (((asn1-collection-of? (class asn1-collection-subtype?)) pred) v)
  (and (is-a? v class)
       (for-all pred (list-queue-list (asn1-collection-elements v)))))
;; due to the comparator we need this to make it generic
;; but better not to use this for performance
(define-generic asn1-collection-add!)
(define (asn1-collection-ref (collection asn1-collection?) (index integer?))
  (list-ref (list-queue-list (asn1-collection-elements collection)) index))

;;; Boolean
(define-class <der-boolean> (<asn1-simple-object>) ())
(define (der-boolean? o) (is-a? o <der-boolean>))
(define (boolean->der-boolean (value boolean?))
  (make <der-boolean> :value value))
(define (bytevector->der-boolean (bv bytevector?))
  (unless (= (bytevector-length bv) 1)
    (assertion-violation 'bytevector->der-boolean
			 "Bytevector length must be one" bv))
  (make <der-boolean> :value (not (zero? (bytevector-u8-ref bv 0)))))
(define (der-boolean->boolean (der-boolean der-boolean?))
  (asn1-simple-object-value der-boolean))
  
;;; Integer
(define-class <der-integer> (<asn1-simple-object>) ())
(define (der-integer? o) (is-a? o <der-integer>))
(define (integer->der-integer (value integer?))
  (make <der-integer> :value value))
(define (der-integer->integer (der-integer der-integer?))
  (asn1-simple-object-value der-integer))
(define (bytevector->der-integer (bv bytevector?))
  (make <der-integer> :value (bytevector->sinteger bv)))

;; Bit string
(define-class <der-bit-string> (<asn1-string>)
  ((padding-bits :init-keyword :padding-bits
		 :reader der-bit-string-padding-bits)))
(define (der-bit-string? o) (is-a? o <der-bit-string>))
(define (bytevector->der-bit-string (bv bytevector?)
				    :optional ((pad integer?) 0))
  (make <der-bit-string> :value (bytevector-copy bv) :padding-bits pad))
(define (der-bit-string->bytevector (bit-string der-bit-string?))
  (asn1-simple-object-value bit-string))
(define-method object-equal? ((a <der-bit-string>) (b <der-bit-string>))
  (and (eqv? (der-bit-string-padding-bits a) (der-bit-string-padding-bits b))
       (call-next-method)))

;; Octet string
(define-class <der-octet-string> (<asn1-simple-object>) ())
(define (der-octet-string? o) (is-a? o <der-octet-string>))
(define (bytevector->der-octet-string (bv bytevector?))
  (make <der-octet-string> :value (bytevector-copy bv)))
(define (der-octet-string->bytevector (der-octet-string der-octet-string?))
  (asn1-simple-object-value der-octet-string))

;; Null
(define-class <der-null> (<asn1-object>) ())
(define (der-null? o) (is-a? o <der-null>))
(define *der-null* (make <der-null>))
(define (make-der-null) *der-null*)
(define-method object-equal? ((a <der-null>) (b <der-null>)) #t)

;; Object identifier
(define-class <der-object-identifier> (<asn1-simple-object>) ())
(define (der-object-identifier? o) (is-a? o <der-object-identifier>))
(define (bytevector->der-object-identifier (bv bytevector?))
  (define len (bytevector-length bv))
  (let-values (((out e) (open-string-output-port)))
    (let loop ((value 0) (first 0) (i 0))
      (if (= i len)
	  (make <der-object-identifier> :value (e))
	  (let* ((b (bitwise-and (bytevector-u8-ref bv i) #xFF))
		 (value (+ (* value 128) (bitwise-and b #x7F))))
	    (if (zero? (bitwise-and b #x80))
		(let ((value (if first
				 (case (div value 40)
				   ((0) (put-char out #\0) value)
				   ((1) (put-char out #\1) (- value 40))
				   (else (put-char out #\2) (- value 80)))
				 value)))
		  (put-char out #\.)
		  (put-string out (number->string value))
		  (loop 0 #f (+ i 1)))
		(loop value first (+ i 1))))))))
(define (oid-string->der-object-identifier (oid string?))
  (make <der-object-identifier> :value oid))
(define (der-object-identifier->oid-string (der-oid der-object-identifier?))
  (asn1-simple-object-value der-oid))

;; External
(define-class <der-external> (<asn1-object>)
  ((direct-reference :init-keyword :direct-reference
		     :reader der-external-direct-reference)
   (indirect-reference :init-keyword :indirect-reference
		       :reader der-external-indirect-reference)
   (data-value-descriptor :init-keyword :data-value-descriptor
			  :reader der-external-data-value-descriptor)
   (encoding :init-keyword :encoding :reader der-external-encoding)))
(define (der-external? o) (is-a? o <der-external>))
(define-method object-equal? ((a <der-external>) (b <der-external>))
  (and (equal? (der-external-direct-reference a)
	       (der-external-direct-reference b))
       (equal? (der-external-indirect-reference a)
	       (der-external-indirect-reference b))
       (equal? (der-external-data-value-descriptor a)
	       (der-external-data-value-descriptor b))
       (equal? (der-external-encoding a) (der-external-encoding b))))

;; Enumerated
(define-class <der-enumerated> (<asn1-simple-object>) ())
(define (der-enumerated? o) (is-a? o <der-enumerated>))
(define (bytevector->der-enumerated (bv bytevector?))
  (make <der-enumerated> :value (bytevector->sinteger bv)))
(define (integer->der-enumerated (integer integer?))
  (make <der-enumerated> :value integer))
(define (der-enumerated->integer (der-enumerated der-enumerated?))
  (asn1-simple-object-value der-enumerated))

;;; Collections
(define ((list-of pred) list) (for-all pred list))
(define list-of-der-encodable? (list-of asn1-encodable?))

;; Sequence
(define-class <der-sequence> (<asn1-collection>) ())
(define (der-sequence? o) (is-a? o <der-sequence>))
(define (make-der-sequence (elements list-of-der-encodable?))
  (make <der-sequence> :elements (make-list-queue elements)))
(define (der-sequence . e*) (make-der-sequence e*))
(define der-sequence-of (asn1-collection-of? <der-sequence>))
(define (der-sequence-add! (sequence der-sequence?) (e asn1-encodable?))
  (list-queue-add-back! (asn1-collection-elements sequence) e)
  sequence)
(define-method asn1-collection-add! ((s <der-sequence>) (e <asn1-encodable>))
  (der-sequence-add! s e))

;; Set
(define default-set-equal? equal?)
(define-class <der-set> (<asn1-collection>)
  ((=? :init-keyword :comparator :reader der-set=?
       ;; FIXME
       :init-value default-set-equal?)))
(define (der-set? o) (is-a? o <der-set>))
(define (make-der-set (elements list-of-der-encodable?)
		      :optional (=? default-set-equal?))
  (make <der-set> :elements (make-list-queue (delete-duplicates elements =?))
	:comparator =?))
(define (der-set . e*) (make-der-set e* default-set-equal?))
(define der-set-of (asn1-collection-of? <der-set>))
(define (der-set-add! (set der-set?) (e asn1-encodable?))
  (let ((q (asn1-collection-elements set)))
    (if (list-queue-empty? q)
	(list-queue-add-back! q e)
	(let ((e* (list-queue-remove-all! q)))
	  (for-each (lambda (e) (list-queue-add-back! q e))
		    (delete-duplicates! (cons e e*) (der-set=? set))))))
  set)
(define-method asn1-collection-add! ((s <der-set>) (e <asn1-encodable>))
  (der-set-add! s e))
(define-method object-equal? ((a <der-set>) (b <der-set>))
  (and (eq? (der-set=? a) (der-set=? b))
       (call-next-method)))


;; Numeric string
(define-class <der-numeric-string> (<asn1-string>) ())
(define (der-numeric-string? o) (is-a? o <der-numeric-string>))
(define (string->der-numeric-string (s string?))
  ;; TODO check string format
  (make <der-numeric-string> :value s))
(define (der-numeric-string->string (numeric-string der-numeric-string?))
  (asn1-simple-object-value numeric-string))
(define bytevector->der-numeric-string
  (make-bytevector->asn1-string string->der-numeric-string))

;; Printable string
(define-class <der-printable-string> (<asn1-string>) ())
(define (der-printable-string? o) (is-a? o <der-printable-string>))
(define (string->der-printable-string (s string?))
  ;; TODO check string format
  (make <der-printable-string> :value s))
(define (der-printable-string->string (printable-string der-printable-string?))
  (asn1-simple-object-value printable-string))
(define bytevector->der-printable-string
  (make-bytevector->asn1-string string->der-printable-string))

;; T61 string
(define-class <der-t61-string> (<asn1-string>) ())
(define (der-t61-string? o) (is-a? o <der-t61-string>))
(define (string->der-t61-string (s string?))
  ;; TODO check string format
  (make <der-t61-string> :value s))
(define (der-t61-string->string (t61-string der-t61-string?))
  (asn1-simple-object-value t61-string))
(define bytevector->der-t61-string
  (make-bytevector->asn1-string string->der-t61-string))

;; Videotex string
(define-class <der-videotex-string> (<asn1-string>) ())
(define (der-videotex-string? o) (is-a? o <der-videotex-string>))
(define (string->der-videotex-string (s string?))
  ;; TODO check string format
  (make <der-videotex-string> :value s))
(define (der-videotex-string->string (videotex-string der-videotex-string?))
  (asn1-simple-object-value videotex-string))
(define bytevector->der-videotex-string
  (make-bytevector->asn1-string string->der-videotex-string))

;; Ia5 string
(define-class <der-ia5-string> (<asn1-string>) ())
(define (der-ia5-string? o) (is-a? o <der-ia5-string>))
(define (string->der-ia5-string (s string?))
  ;; TODO check string format
  (make <der-ia5-string> :value s))
(define (der-ia5-string->string (ia5-string der-ia5-string?))
  (asn1-simple-object-value ia5-string))
(define bytevector->der-ia5-string
  (make-bytevector->asn1-string string->der-ia5-string))

;; ASN.1 time
(define-class <asn1-time> (<asn1-simple-object>)
  ((format :init-keyword :format :reader asn1-time-format)))

(define (asn1-time? o) (is-a? o <asn1-time>))
(define-method initialize :after ((o <asn1-time>) ignore)
  (string->date (asn1-simple-object-value o)
		(asn1-time-format o)) ;; format check
  o)
(define (asn1-time->date (asn1-time asn1-time?))
  (string->date (asn1-simple-object-value asn1-time?)
		(asn1-time-format asn1-time)))

;; UTC time
(define-class <der-utc-time> (<asn1-time>) ())
(define (der-utc-time? o) (is-a? o <der-utc-time>))
(define (string->der-utc-time (s string?))
  (make <der-utc-time> :value s :format "~y~m~d~H~M~S~z"))
(define (date->der-utc-time (date date?))
  (string->der-utc-time (date->string "~y~m~d~H~M~S~z")))
(define (bytevector->der-utc-time (bv bytevector?))
  (string->der-utc-time (utf8->string bv)))
(define (der-utc-time->string (utc-time der-utc-time?))
  (asn1-simple-object-value utc-time))
(define (der-utc-time->date (utc-time der-utc-time?))
  (asn1-time->date utc-time))

;; Generalized time
(define-class <der-generalized-time> (<asn1-time>) ())
(define (der-generalized-time? o) (is-a? o <der-generalized-time>))
(define (string->der-generalized-time (s string?))
  ;; TODO better to parse the string
  (define (get-format s)
    (define (has-fraction-seconds time)
      (let ((i (string-index time #\.)))
	(and i (= i 14))))
    (cond ((or (string-suffix? "Z" s)
	       (string-index s #\-)
	       (string-index s #\+))
	   (if (has-fraction-seconds s)
	       "~Y~m~d~H~M~S.~~~~~~~z"
	       "~Y~m~d~H~M~S~z"))
	  (else
	   (if (has-fraction-seconds s)
	       "~Y~m~d~H~M~S.~~~~~~~"
	       "~Y~m~d~H~M~S"))))
  (let ((format (get-format s)))
    (make <der-generalized-time> :value s :format format)))
(define (date->der-generalized-time (date date?))
  ;; For now ignore nanosecond
  (make <der-generalized-time> :value (date->string date "~Y~m~d~H~M~S~z")
	:format "~Y~m~d~H~M~S~z"))
(define (bytevector->der-generalized-time (bv bytevector?))
  (string->der-generalized-time (utf8->string bv)))
(define (der-generalized-time->string (generalized-time der-generalized-time?))
  (asn1-simple-object-value generalized-time))
(define (der-generalized-time->date (generalized-time der-generalized-time?))
  (asn1-time->date generalized-time))

;; Graphic string
(define-class <der-graphic-string> (<asn1-string>) ())
(define (der-graphic-string? o) (is-a? o <der-graphic-string>))
(define (string->der-graphic-string (s string?))
  ;; TODO check string format
  (make <der-graphic-string> :value s))
(define (der-graphic-string->string (graphic-string der-graphic-string?))
  (asn1-simple-object-value graphic-string))
(define bytevector->der-graphic-string
  (make-bytevector->asn1-string string->der-graphic-string))

;; Visible string
(define-class <der-visible-string> (<asn1-string>) ())
(define (der-visible-string? o) (is-a? o <der-visible-string>))
(define (string->der-visible-string (s string?))
  ;; TODO check string format
  (make <der-visible-string> :value s))
(define (der-visible-string->string (visible-string der-visible-string?))
  (asn1-simple-object-value visible-string))
(define bytevector->der-visible-string
  (make-bytevector->asn1-string string->der-visible-string))

;; General string
(define-class <der-general-string> (<asn1-string>) ())
(define (der-general-string? o) (is-a? o <der-general-string>))
(define (string->der-general-string (s string?))
  ;; TODO check string format
  (make <der-general-string> :value s))
(define (der-general-string->string (general-string der-general-string?))
  (asn1-simple-object-value general-string))
(define bytevector->der-general-string
  (make-bytevector->asn1-string string->der-general-string))

;; Universal string
(define-class <der-universal-string> (<asn1-string>) ())
(define (der-universal-string? o) (is-a? o <der-universal-string>))
(define (string->der-universal-string (s string?))
  ;; TODO check string format
  (make <der-universal-string> :value s))
(define (der-universal-string->string (universal-string der-universal-string?))
  (asn1-simple-object-value universal-string))
(define bytevector->der-universal-string
  (make-bytevector->asn1-string string->der-universal-string))

;; Bmp string
(define-class <der-bmp-string> (<asn1-string>) ())
(define (der-bmp-string? o) (is-a? o <der-bmp-string>))
(define (string->der-bmp-string (s string?))
  ;; TODO check string format
  (make <der-bmp-string> :value s))
(define (der-bmp-string->string (bmp-string der-bmp-string?))
  (asn1-simple-object-value bmp-string))
(define bytevector->der-bmp-string
  (make-bytevector->asn1-string string->der-bmp-string
   (lambda (bv) (utf16->string bv (endianness big)))))

;; Utf8 string
(define-class <der-utf8-string> (<asn1-string>) ())
(define (der-utf8-string? o) (is-a? o <der-utf8-string>))
(define (string->der-utf8-string (s string?))
  ;; TODO check string format
  (make <der-utf8-string> :value s))
(define (der-utf8-string->string (utf8-string der-utf8-string?))
  (asn1-simple-object-value utf8-string))
(define bytevector->der-utf8-string
  (make-bytevector->asn1-string string->der-utf8-string))

;; Application specific
(define-class <der-application-specific> (<asn1-object>)
  ((constructed? :init-keyword :constructed?
		 :reader der-application-specific-constructed?)
   (tag :init-keyword :tag :reader der-application-specific-tag)
   (octets :init-keyword :octets :reader der-application-specific-octets)))
(define (der-application-specific? o) (is-a? o <der-application-specific>))
(define-method object-equal? ((a <der-application-specific>)
			      (b <der-application-specific>))
  (and (eqv? (der-application-specific-constructed? a)
	     (der-application-specific-constructed? b))
       (eqv? (der-application-specific-tag b)
	     (der-application-specific-tag b))
       (equal? (der-application-specific-octets b)
	       (der-application-specific-octets b))))

;; Tagged object
(define-class <der-tagged-object> (<asn1-object>)
  ((tag-no :init-keyword :tag-no :reader der-tagged-object-tag-no)
   (explicit? :init-keyword :explicit? :reader der-tagged-object-explicit?)
   (obj :init-keyword :obj :reader der-tagged-object-obj)))
(define (der-tagged-object? o) (is-a? o <der-tagged-object>))
(define-method object-equal? ((a <der-tagged-object>) (b <der-tagged-object>))
  (and (eqv? (der-tagged-object-tag-no a) (der-tagged-object-tag-no b))
       (eqv? (der-tagged-object-explicit? a) (der-tagged-object-explicit? b))
       (equal? (der-tagged-object-obj a) (der-tagged-object-obj b))))

(define-class <der-unknown-tag> (<asn1-object>)
  ((constructed? :init-keyword :constructed?
		 :reader der-unknown-tag-constructed?)
   (number :init-keyword :number :reader der-unknown-tag-number)
   (data :init-keyword :data :reader der-unknown-tag-data)))
(define (der-unknown-tag? o) (is-a? o <der-unknown-tag>))

;;; BER
;; Constructed octet string
(define *ber-octet-string-max-length* 1000)
(define-class <ber-octet-string> (<der-octet-string>)
  ;; Cache, more or less
  ((octs :init-keyword :octs :reader ber-octet-string-octs)))
(define (ber-octet-string? o)
  (is-a? o <ber-octet-string>))
(define (bytevector->ber-octed-string (bv bytevector?))
  (define (split bv len)
    (let loop ((i 0) (r '()))
      (if (>= i len)
	  (reverse! r)
	  (let* ((len (min *ber-octet-string-max-length* (- len i)))
		 (nstr (make-bytevector len)))
	    (bytevector-copy! bv i nstr 0 len)
	    (loop (+ i *ber-octet-string-max-length*) (cons nstr r))))))
	 
  (define len (bytevector-length bv))
  (let ((bv (bytevector-copy bv)))
    (make <ber-octet-string> :value bv
	  :octs (map bytevector->der-octet-string
		     (if (< len *ber-octet-string-max-length*)
			 (list bv)
			 (split bv len))))))
(define (list->ber-octet-string (list (list-of bytevector?)))
  (make <ber-octet-string> :value (bytevector-concatenate list)
	:octs (map bytevector->der-octet-string list)))

;; Tagged object
(define-class <ber-tagged-object> (<der-tagged-object>) ())
(define (ber-tagged-object? o) (is-a? o <ber-tagged-object>))

;; Sequence
(define-class <ber-sequence> (<der-sequence>) ())
(define (ber-sequence? o) (is-a? o <ber-sequence>))
(define (make-ber-sequence (list list-of-der-encodable?))
  (make <ber-sequence> :elements list))
(define (ber-sequence . e*)  (make-ber-sequence e*))

;; Set
(define-class <ber-set> (<der-set>) ())
(define (ber-set? o) (is-a? o <ber-set>))
(define (make-ber-set =? (list list-of-der-encodable?))
  (make <ber-set> :comparator =? :elements (delete-duplicates list)))
(define (ber-set . e*) (make-ber-set default-set-equal? e*))

)
