;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/amqp/types.scm - AMQP v1.0 type
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

;; reference:
;;   http://docs.oasis-open.org/amqp/core/v1.0/os/amqp-core-types-v1.0-os.html

(library (net mq amqp types)
    (export read-amqp-data
	    write-amqp-data
	    ;; null is the special value for now...
	    +amqp-null+ amqp-null?
	    ->amqp-value
	    define-composite-type
	    define-restricted-type
	    ;; for testing
	    write-primitive-amqp-data
	    scheme-value)
    (import (rnrs) 
	    (sagittarius) 
	    (sagittarius control)
	    (sagittarius object)
	    (clos core)
	    (clos user)
	    (binary data)
	    (srfi :19)
	    (srfi :26)
	    (rfc uuid)
	    (pp))

  (define (read-fixed-width-data n)
    (lambda (in subcategory) (get-bytevector-n in n)))

  (define (read-variable-width-data fix-reader)
    (lambda (in subcategory)
      (let ((len (fix-reader in subcategory)))
	(get-bytevector-n in (bytevector->integer len)))))

  (define (read-compound-data size)
    (let ((fix-reader (read-fixed-width-data size)))
      (lambda (in subcategory)
	(let ((len (fix-reader in subcategory)))
	  (get-bytevector-n in (bytevector->integer len))))))

  (define (read-array-data size)
    (let ((fix-reader (read-fixed-width-data size)))
      (lambda (in subcategory)
	(let ((len (fix-reader in subcategory)))
	  (get-bytevector-n in (bytevector->integer len))))))

  ;; table for data reader
  (define +sub-categories+
    `((#x4 . #f) ;; empty case
      (#x5 . ,(read-fixed-width-data 1))
      (#x6 . ,(read-fixed-width-data 2))
      (#x7 . ,(read-fixed-width-data 4))
      (#x8 . ,(read-fixed-width-data 8))
      (#x9 . ,(read-fixed-width-data 16))
      (#xA . ,(read-variable-width-data (read-fixed-width-data 1)))
      (#xB . ,(read-variable-width-data (read-fixed-width-data 4)))
      ;; compound
      (#xC . ,(read-compound-data 1))
      (#xD . ,(read-compound-data 4))
      ;; array
      (#xE . ,(read-array-data 1))
      (#xF . ,(read-array-data 4))))

  (define (sub-category u8) (bitwise-arithmetic-shift-right u8 4))
  (define (sub-type u8) (bitwise-and #x0F u8))
  (define (read-ext-type in u8) (and (= (sub-type u8) #xF) (get-u8 in)))

  (define *primitive-type-table* (make-eq-hashtable))  ;; Scheme -> binary table
  (define *primitive-code-table* (make-eqv-hashtable)) ;; binary -> Scheme table

  (define *class/type-table* (make-eq-hashtable))
  (define *type/class-table* (make-eq-hashtable))
  (define *code/class-table* (make-eqv-hashtable))

  (define (register! name code reader writer write-pred?)
    (let ((slots (hashtable-ref *primitive-type-table* name '())))
      (hashtable-set! *primitive-type-table* 
		      name (acons code (cons writer write-pred?) slots)))
    (hashtable-set! *primitive-code-table* code reader))

  (define (add-class-entry! class name code)
    (hashtable-set! *class/type-table* class name)
    (hashtable-set! *code/class-table* code class)
    (hashtable-set! *type/class-table* name class))

  (define (read-constructor in)
    (let* ((first (get-u8 in))
	   (descriptor (and (zero? first) (read-amqp-data in))))
      (values first descriptor)))
  (define (read-data code in)
    (let ((sub-cate (sub-category code))
	  (sub-type (sub-type code))
	  (ext-type (read-ext-type in code)))
      (let ((data (and-let* ((slot (assv sub-cate +sub-categories+)))
		    (if (cdr slot)
			((cdr slot) in code)
			code)))
	    (reader (hashtable-ref *primitive-code-table* code)))
	(if (and data reader)
	    (make (hashtable-ref *code/class-table* code)
	      :value (reader data))
	    (error 'read-amqp-data "unknown data" code)))))
  (define (read-amqp-data in)
    (define (construct-composite descriptor compound)
      (define (get-type slot) (slot-definition-option slot :type))
      (define (get-requires slot) (slot-definition-option slot :requires #f))
      (let* ((code (scheme-value descriptor))
	     ;; must be either symbol or ulong
	     (class (hashtable-ref (if (symbol? code)
				       *type/class-table*
				       *code/class-table*)
				   code #f)))
	(unless class (error 'read-amqp-data "not supported" code compound))
	(rlet1 o (make class)
	  (for-each (lambda (slot value)
		      (let ((name (slot-definition-name slot))
			    (type (get-type slot))
			    (class (class-of value)))
			;; mandatory element can be in between
			;; non mandatory elements, so check the type.
			(cond ((eq? (hashtable-ref *type/class-table* type #f) 
				    class)
			       (set! (~ o name) (scheme-value value)))
			      ((and (eq? type :*)
				    (memq (get-requires slot)
					  (~ class 'provides)))
			       (set! (~ o name) value)))))
	   (class-direct-slots class)
	   (scheme-value compound)))))
    (let-values (((first descriptor) (read-constructor in)))
      (if descriptor
	  (construct-composite descriptor (read-data (get-u8 in) in))
	  (read-data first in))))

  (define (write-primitive-amqp-data out type v)
    (define (get-type slot) (slot-definition-option slot :type))
    (define (multiple? slot) (slot-definition-option slot :multiple #f))
    (define (->list-value v slot)
      (let1 name (slot-definition-name slot)
	(if (slot-bound? v name)
	    (let ((vv   (~ v name))
		  (type (get-type slot))
		  (mult? (multiple? slot)))
	      (if mult?
		  (let* ((len (length vv))
			 (vec (make-vector len)))
		    (let loop ((i 0) (vv vv))
		      (if (null? vv)
			  (->amqp-value :array vec)
			  (let ((v (car vv)))
			    (vector-set! vec i (->amqp-value type v))
			    (loop (+ i 1) (cdr vv))))))
		  ;; for now ignore :* ...
		  (if (eq? type :*)
		      ;; this must be already amqp-value
		      (if (is-a? vv <amqp-type>)
			  vv
			  (error 'write-primitive-amqp-data
				 ":* type value must be AMQP type value"))
		      (->amqp-value type vv))))
	    +amqp-null+)))
    
    (cond ((hashtable-ref *primitive-type-table* type #f)
	   => (lambda (slots)
		(let loop ((slots slots))
		  (if (null? slots)
		      (error 'write-primitive-amqp-data
			     "given type is not supported" type v)
		      (let ((code (caar slots))
			    (writer (cadar slots)) 
			    (pred? (cddar slots)))
			(or (and (or (not pred?) (pred? v))
				 (put-u8 out code)
				 (or (writer out v) #t)) ;; in case
			    (loop (cdr slots))))))))
	  ((and-let* (( (is-a? v <amqp-type>) )
		      (class (class-of v))
		      ( (~ class 'descriptor-name) ))
	     class)
	   => (lambda (class)
		;; there is no extended type... is there?
		(let ((slots (class-direct-slots class))
		      ;;(descriptor-name (~ class 'descriptor-name))
		      (descriptor-code (~ class 'descriptor-code)))
		  (put-u8 out #x00) ;; mark for compoisite
		  ;;(write-primitive-amqp-data out :symbol descriptor-name)
		  (write-primitive-amqp-data out :ulong descriptor-code)
		  (write-primitive-amqp-data out :list 
		    (map (cut ->list-value v <>) slots)))))
	  (else
	   (error 'write-primitive-amqp-data 
		  "given type is not supported" type v))))

  (define (write-amqp-data out v)
    (if (amqp-null? v) ;; a bit special case
	(write-primitive-amqp-data out :null v)
	(let ((class (class-of v)))
	  (cond ((hashtable-ref *class/type-table* class #f)
		 => (cut write-primitive-amqp-data out <> (scheme-value v)))
		(else
		 (error 'write-amqp-data "unsupported value" v))))))

  ;; do with inefficient way...
  (define-class <amqp-meta> (<class>)
    ((provides        :init-keyword :provides       :init-value #f)
     (descriptor-name :init-keyword :descriptor-name :init-value #f)
     (descriptor-code :init-keyword :descriptor-code :init-value 0)))
  (define-class <amqp-type> ()
    ((value :init-keyword :value :reader scheme-value))
    :metaclass <amqp-meta>)
  (define-syntax define-amqp-class
    (syntax-rules ()
      ((_ name) (define-class name (<amqp-type>) ()))))

  (define-syntax %define-primitive-type
    (syntax-rules ()
      ((_ name class ((code reader writer)))
       (begin
	 (register! name code reader writer #f)
	 (add-class-entry! class name code)))
      ((_ name class ((code reader writer pred)))
       (begin
	 (register! name code reader writer pred)
	 (add-class-entry! class name code)))
      ((_ name class (pattern pattern* ...))
       (begin
	 (%define-primitive-type name class (pattern))
	 (%define-primitive-type name class (pattern* ...))))))

  (define-generic ->amqp-value)
  
  (define-syntax define-primitive-type
    (lambda (x)
      (define (class-name name)
	(string->symbol (format "<amqp-~a>" (syntax->datum name))))
      (syntax-case x ()
	((k name pattern)
	 (with-syntax ((class (datum->syntax #'k (class-name #'name))))
	   #'(begin
	       (define-amqp-class class)
	       (define-method ->amqp-value ((type (eql name)) o)
		 (make class :value o))
	       (%define-primitive-type name class pattern)))))))

  (define-syntax define-composite-type
    (lambda (x)
      ;; It's painful to separate library so just duplicate...
      (define (class-name name)
	(string->symbol (format "<amqp-~a>" (syntax->datum name))))
      (define (ctr-name name)
	(string->symbol (format "make-amqp-~a" (syntax->datum name))))
      (define (pred-name name)
	(string->symbol (format "amqp-~a?" (syntax->datum name))))
      (define (key&name sym)
	(list (make-keyword sym) sym))
      (define (params slots)
	(let loop ((slots slots) (r '()))
	  (syntax-case slots ()
	    (() (reverse! r))
	    (((name defs ...) . rest)
	     (loop (cdr slots) (cons (key&name (syntax->datum #'name)) r))))))
      (define (make-slots slots)
	(let loop ((slots slots) (r '()))
	  (syntax-case slots ()
	    (() (reverse! r))
	    (((name defs ...) . rest)
	     ;; find default
	     (let ((default (cond ((memq :default #'(defs ...))
				   => (lambda (l)
					(list :init-value (cadr l))))
				  (else '())))
		   (n (syntax->datum #'name)))
	       (loop (cdr slots) 
		     (cons #`(#,n :init-keyword #,(make-keyword n)
			      #,@default defs ...) r)))))))

      (syntax-case x ()
	((k n descriptor-name domain-id descriptor-id (slots ...) . opt)
	 (with-syntax ((name (datum->syntax #'k (class-name #'n)))
		       (ctr  (datum->syntax #'k (ctr-name #'n)))
		       (pred (datum->syntax #'k (pred-name #'n)))
		       (((keys names) ...) (datum->syntax #'k
					     (params #'(slots ...))))
		       ((slot-defs ...) (make-slots #'(slots ...)))
		       ((provides ...) (datum->syntax #'k
					(get-keyword :provides #'opt '()))))
	   #'(begin
	       (define this-code (bitwise-ior
				  (bitwise-arithmetic-shift-left domain-id 32)
				  descriptor-id))
	       (define-class name (<amqp-type>)
		 (slot-defs ...)
		 :provides '(provides ...)
		 :descriptor-name 'descriptor-name
		 :descriptor-code this-code)
	       (define (pred o) (is-a? o name))
	       (define (ctr :key names ...)
		 ;; FIXME ...
		 (rlet1 r (apply make name 
				 (apply append! (list `(keys ,names) ...)))
		   ;; to fake write-amqp-data
		   (set! (~ r 'value) r)))
	       (add-class-entry! name 'descriptor-name this-code)))))))
  ;; for now
  (define-syntax define-restricted-type
    (lambda (x)
      (define (class-name name)
	(string->symbol (format "<amqp-~a>" (syntax->datum name))))
      (syntax-case x ()
	((_ name value . opts)
	 (with-syntax ((class (datum->syntax #'k (class-name #'name)))
		       ((provides ...) (datum->syntax #'k
					(get-keyword :provides #'opts '()))))
	   #'(begin
	       (define-class class (<amqp-type>) ()
		 :provides '(provides ...))
	       (define name value)))))))

  (define (write-nothing out v))
  (define (write/condition pred writer)
    (lambda (out v) (and (pred v) (writer out v))))
  (define (true? o) (and (boolean? o) o))
  (define (false? o) (not o))

  (define (u8? u8) (<= 0 u8 #xFF))
  (define (s8? s8) (<= -128 s8 127))

  (define-class <amqp-null-value> () ())
  (define +amqp-null+ (make <amqp-null-value>))
  (define-method write-object ((o <amqp-null-value>) out)
    (display "#<amqp-null>" out))
  (define (amqp-null? o) (is-a? o <amqp-null-value>))

  (define-primitive-type :null
    ((#x40 (lambda (data) +amqp-null+) write-nothing)))
  (define-primitive-type :boolean
    ((#x56 (lambda (data) 
	     (let ((v (bytevector-u8-ref data 0)))
	       (cond ((zero? v) #f)
		     ((= v 1) #t)
		     (else (error 'boolean "invalid value for boolean" v)))))
	   (lambda (out v) (put-u8 out (if v 1 0))))
     (#x41 (lambda (data) #t)  write-nothing true?)
     (#x42 (lambda (data) #f)  write-nothing false?)))
  ;; unsigned integers
  (define-primitive-type :ubyte
    ((#x50 (lambda (data) (bytevector-u8-ref data 0))
	   (lambda (out u8) (put-u8 out u8)))))
  (define-primitive-type :ushort
    ((#x60 (lambda (data) (bytevector->integer data))
	   (lambda (out u16) (put-u16 out u16 (endianness big))))))
  (define-primitive-type :uint
    ((#x70 (lambda (data) (bytevector->integer data))
	   (lambda (out u32) (put-u32 out u32 (endianness big))))
     (#x52 (lambda (data) (bytevector-u8-ref data 0))
	   (lambda (out u8) (put-u8 out u8))
	   u8?)
     (#x43 (lambda (out) 0)  write-nothing zero?)))
  (define-primitive-type :ulong
    ((#x80 (lambda (data) (bytevector->integer data))
	   (lambda (out u64) (put-u64 out u64 (endianness big))))
     (#x53 (lambda (data) (bytevector-u8-ref data 0))
	   (lambda (out u8) (put-u8 out u8))
	   u8?)
     (#x44 (lambda (out) 0)  write-nothing zero?)))
  ;; signed integers
  (define-primitive-type :byte
    ((#x51 (lambda (data) (bytevector-s8-ref data 0))
	   (lambda (out s8) (put-s8 out s8)))))
  (define-primitive-type :short
    ((#x61 (lambda (data) (bytevector->sinteger data))
	   (lambda (out s16) (put-s16 out s16 (endianness big))))))
  (define-primitive-type :int
    ((#x71 (lambda (data) (bytevector->sinteger data))
	   (lambda (out s32) (put-s32 out s32 (endianness big))))
     (#x54 (lambda (data) (bytevector-s8-ref data 0))
	   (lambda (out s8) (put-s8 out s8))
	   s8?)))
  (define-primitive-type :long
    ((#x81 (lambda (data) (bytevector->sinteger data))
	   (lambda (out s64) (put-s64 out s64 (endianness big))))
     (#x55 (lambda (data) (bytevector-s8-ref data 0))
	   (lambda (out s8) (put-s8 out s8))
	   s8?)))
  ;; ieee 754 floating numbers
  (define-primitive-type :float
    ((#x72 (lambda (data) (bytevector-ieee-single-ref data 0 (endianness big)))
	   (lambda (out f32) (put-f32 out f32 (endianness big))))))
  (define-primitive-type :double
    ((#x82 (lambda (data) (bytevector-ieee-double-ref data 0 (endianness big)))
	   (lambda (out f64) (put-f64 out f64 (endianness big))))))
  ;; we don't support decimals ... for now
  ;; char
  (define-primitive-type :char
    ((#x73 (lambda (data) (integer->char (bytevector->integer data)))
	   (lambda (out c) (put-u32 out (char->integer c) (endianness big))))))
  ;; timestamp (returns date)
  (define-primitive-type :timestamp
    ((#x83 (lambda (data)
	     (let* ((millis (bytevector->integer data))
		    (sec    (div millis 1000))
		    (nano   (- (* millis 1000000) (* sec 1000 1000000))))
	       (time-utc->date (make-time time-utc nano sec) 0)))
	   (lambda (out date) 
	     (let* ((time (date->time-utc date))
		    (sec  (time-second time))
		    (nano (time-nanosecond time)))
	       (put-u64 out (+ (* sec 1000) (div nano 1000000))
			(endianness big)))))))
  ;; uuid
  (define-primitive-type :uuid
    ((#x93 (lambda (data) (bytevector->uuid data))
	   (lambda (out uuid) (put-bytevector out (uuid->bytevector uuid))))))
  ;; variables
  ;; binary
  ;; variables are only 1 or 4
  (define-syntax define-vpred
    (syntax-rules ()
      ((_ name pred sizer)
       (define (name o) (and (pred o) (> (sizer o) #xFF))))))
  (define-vpred vbin32? bytevector? bytevector-length)
  (define-primitive-type :binary
    ((#xA0 (lambda (data) data)
	   (lambda (out bv) 
	     (put-u8 out (bytevector-length bv))
	     (put-bytevector out bv)))
     (#xB0 (lambda (data) data)
	   (lambda (out bv) 
	     (put-u32 out (bytevector-length bv) (endianness big))
	     (put-bytevector out bv))
	   vbin32?)))
  ;; string
  (define (utf8-length str) (bytevector-length (string->utf8 str)))
  (define-vpred str32? string? utf8-length)
  (define-primitive-type :string
    ((#xA1 (lambda (data) (utf8->string data))
	   (lambda (out str) 
	     (let ((bv (string->utf8 str)))
	       (put-u8 out (bytevector-length bv))
	       (put-bytevector out bv))))
     (#xB1 (lambda (data) (utf8->string data))
	   (lambda (out str) 
	     (let ((bv (string->utf8 str)))
	       (put-u32 out (bytevector-length bv) (endianness big))
	       (put-bytevector out bv)))
	   str32?)))

  (define (symbol-length sym) (utf8-length (symbol->string sym)))
  (define-vpred sym32? symbol? symbol-length)
  (define-primitive-type :symbol
    ((#xA3 (lambda (data) (string->symbol (utf8->string data)))
	   (lambda (out str) 
	     (let ((bv (string->utf8 (symbol->string str))))
	       (put-u8 out (bytevector-length bv))
	       (put-bytevector out bv))))
     (#xB3 (lambda (data) (string->symbol (utf8->string data)))
	   (lambda (out str) 
	     (let ((bv (string->utf8 (symbol->string str))))
	       (put-u32 out (bytevector-length bv) (endianness big))
	       (put-bytevector out bv)))
	   sym32?)))

  ;; TODO following composite primitive types are implemented
  ;; a bit awkward way. so need refactoring
  ;; list
  (define (read-amqp-list size)
    (lambda (data)
      (let* ((in (open-bytevector-input-port data))
	     (count (bytevector->integer (get-bytevector-n in size))))
	(let loop ((r '()) (c 0))
	  (if (or (eof-object? (lookahead-u8 in)) (> c count))
	      (reverse! r)
	      (loop (cons (read-amqp-data in) r) (+ c 1)))))))
  (define (write-amqp-list out lst)
    (define (write-list lst)
      (call-with-bytevector-output-port
       (lambda (out) 
	 (for-each (cut write-amqp-data out <>) lst))))
    (let ((bv (write-list lst))
	  (len (length lst)))
      (if (> (count-aprox lst) 255)
	  (begin 
	    (put-u32 out (+ (bytevector-length bv) 4) (endianness big))
	    (put-u32 out len (endianness big)))
	  (begin
	    (put-u8 out (+ (bytevector-length bv) 1))
	    (put-u8 out len)))
      (put-bytevector out bv)))
  (define (count-aprox lst)
    (define (aprox e)
      (cond ((undefined? e) 1)
	    ((and (integer? e) (zero? e)) 1)
	    ((and (integer? e) (< e 255)) 2)
	    ((integer? e) (+ 5 (div (bitwise-lengt e) 8)))
	    ((string? e) (+ (string-length e) 5))
	    ((symbol? e) (+ (string-length (symbol->string e)) 5))
	    ((bytevector? e) (+ (bytevector-length e) 5))
	    ((flonum? e) 8)
	    ((pair? e) (count-aprox e))
	    ((vector? e) (count-aprox (vector->list e)))
	    ((hashtable? e)
	     (let-values (((keys values) (hashtable-entries e)))
	       (+ (count-aprox (vector->list keys))
		  (count-aprox (vector->list values)))))
	    ;; should we raise an error?
	    (else 0)))
    (let loop ((count 0) (lst lst))
      (if (null? lst)
	  count
	  (loop (+ count (aprox (car lst))) (cdr lst)))))
  (define-primitive-type :list
    ((#x45 (lambda (data) '()) write-nothing)
     (#xC0 (read-amqp-list 1) write-amqp-list) 
     (#xD0 (read-amqp-list 4) write-amqp-list
	   (lambda (o) (> (count-aprox o) 255)))))

  ;; TODO map

  (define (read-amqp-array size)
    (lambda (data)
      (let* ((in (open-bytevector-input-port data))
	     (count (bytevector->integer (get-bytevector-n in size)))
	     (r (make-vector count)))
	(let-values (((first descriptor) (read-constructor in)))
	  (if descriptor
	      (let1 code (get-u8 in) ;; well must be #xC0 or #xD0 though
		(dotimes (i count r)
		  (vector-set! r i (construct-composite descriptor 
							(read-data code in)))))
	      (dotimes (i count r)
		(vector-set! r i (read-data first in))))))))

  (define (write-amqp-array out array)
    (define (write-array array)
      ;; array is homogeneous so only need to get first one
      (let* ((v (vector-ref array 0))
	     (class (class-of v))
	     (type (hashtable-ref *class/type-table* class #f))
	     (slots (hashtable-ref *primitive-type-table* type #f)))
	(unless slots (error 'write-amqp-array "unsupported type" v))
	(let loop ((slots slots))
	  (if (null? slots)
	      (error 'write-amqp-array "unsupported type" v)
	      (let ((pred? (cddar slots)))
		;; again must be homogeneous so no different code
		(if (or (not pred?)
			;; well this is sort of awkward ...
			(and (not (boolean? (scheme-value v)))
			     (pred? (scheme-value v))))
		    (let ((writer (cadar slots))
			  (code (caar slots)))
		      (values code 
			      (call-with-bytevector-output-port
			       (lambda (out)
				 (if (keyword? type)
				     ;; ok primitive just code
				     (put-u8 out code)
				     ;; now constructor
				     (begin
				       (put-u8 out 0)
				       (write-primitive-amqp-data
					out :symbol type)))))
			      (call-with-bytevector-output-port
			       (lambda (out) 
				 (vector-for-each 
				  (lambda (v) (writer out (scheme-value v)))
				  array)))))
		    (loop (cdr slots))))))))
    (let-values (((code type bv) (write-array array)))
      (let1 len (vector-length array)
	(if (> (count-aprox (vector->list array)) 255)
	    (begin 
	      (put-u32 out (+ (bytevector-length bv)
			      (bytevector-length type) 
			      4)
		       (endianness big))
	      (put-u32 out len (endianness big)))
	    (begin
	      (put-u8 out (+ (bytevector-length bv) (bytevector-length type) 1))
	      (put-u8 out len)))
	(put-bytevector out type)
	(put-bytevector out bv))))

  (define-primitive-type :array
    ((#xE0 (read-amqp-array 1) write-amqp-array) 
     (#xF0 (read-amqp-array 4) write-amqp-array
	   (lambda (o) (> (count-aprox (vector->list o)) 255)))))
  )