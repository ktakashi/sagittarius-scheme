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
	    ;; for testing
	    write-primitive-amqp-data
	    scheme-value)
    (import (rnrs) 
	    (sagittarius) 
	    (clos core)
	    (clos user)
	    (binary data)
	    (srfi :19)
	    (srfi :26)
	    (rfc uuid))

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
	  ;; TODO is this actually correct?
	  (get-bytevector-n in (- (bytevector->integer len) size))))))

  (define (read-array-data size)
    (let ((fix-reader (read-fixed-width-data size)))
      (lambda (in subcategory)
	(let ((len (fix-reader in subcategory)))
	  ;; TODO is this actually correct?
	  (get-bytevector-n in (- (bytevector->integer len) size))))))

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
  (define *code/class-table* (make-eq-hashtable))

  (define (register! name code reader writer write-pred?)
    (let ((slots (hashtable-ref *primitive-type-table* name '())))
      (hashtable-set! *primitive-type-table* 
		      name (acons code (cons writer write-pred?) slots)))
    (hashtable-set! *primitive-code-table* code reader))

  (define (add-class-entry! class name code)
    (hashtable-set! *class/type-table* class name)
    (hashtable-set! *code/class-table* code class))

  (define (read-amqp-data in)
    (define (rec code)
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

    (let* ((first (get-u8 in))
	   (descriptor (and (zero? first) (read-amqp-data in))))
      (if descriptor
	  (construct-composite descriptor (rec (get-u8)))
	  (rec first))))

  (define (write-primitive-amqp-data out type v)
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
	  (else
	   (error 'write-primitive-amqp-data 
		  "given type is not supported" type v))))

  (define (write-amqp-data out v)
    (let ((class (class-of v)))
      (cond ((hashtable-ref *class/type-table* class #f)
	     => (cut write-primitive-amqp-data out <> (scheme-value v)))
	    (else
	     (error 'write-amqp-data "unsupported value" v)))))

  ;; do with inefficient way...
  (define-class <amqp-type> ()
    ((value :init-keyword :value :reader scheme-value)))
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

  (define-syntax define-primitive-type
    (lambda (x)
      (define (class-name name)
	(string->symbol (format "<amqp-~a>" (syntax->datum name))))
      (syntax-case x ()
	((k name pattern)
	 (with-syntax ((class (datum->syntax #'k (class-name #'name))))
	   #'(begin
	       (define-amqp-class class)
	       (export class)
	       (%define-primitive-type name class pattern)))))))

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
  (define (amqp-null? o) (is-a? o <amqp-null>))

  (define-primitive-type :null
    ((#x40 (lambda (data) +amqp-null+) write-nothing)))
  (define-primitive-type :boolean
    ((#x56 (lambda (data) 
	     (let ((v (bytevector-u8-ref data 0)))
	       (cond ((zero? v) #f)
		     ((= v 1) #t)
		     (else (error 'boolean "invalid value for boolean" v)))))
	   (lambda (out v) (put-u8 (if v 1 0))))
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
      ;; TODO are these length encoding correct?
      (if (> (count-aprox lst) 255)
	  (begin 
	    (put-u32 out (+ (bytevector-length bv) 8) (endianness big))
	    (put-u32 out len (endianness big)))
	  (begin
	    (put-u8 out (+ (bytevector-length bv) 2))
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
  )