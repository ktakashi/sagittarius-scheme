;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a136/extensible-record-types.scm - Extensible record types
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :136 extensible-record-types)
  (export record?
	  record-type-descriptor?
	  record-type-descriptor
	  (rename (record-predicate record-type-predicate))
	  record-type-name
	  record-type-parent
	  record-type-fields
	  make-record-type-descriptor
	  make-record

	  define-record-type)
  (import (except (rnrs)
		  define-record-type
		  ;; it's a macro in R6RS so can't satisfy the SRFI
		  record-type-descriptor
		  make-record-type-descriptor)
	  (only (core record procedural) <record-type-descriptor>)
	  (clos user)
	  (clos core)
	  (sagittarius)
	  ;; for <record-type-meta>
	  (sagittarius clos)
	  (sagittarius control))

;; extends default record-type-descriptor to hold field-specs...
(define-class <srfi:record-type-descriptor> (<record-type-descriptor>)
  ((field-specs :init-keyword :field-specs :reader record-type-fields)))

(define (make-record-type-descriptor name fields :optional (parent #f))
  (define (process-fields fields)
    ;; -> slot form
    (map (lambda (field)
	   (if (pair? field)
	       (let ((len (length field))
		     (name (car field)))
		 (unless (<= 2 len 3)
		   (assertion-violation 'make-record-type-descriptor
					"invalid field spec" field))
		 (list name :mutable (= (length len) 3)
		       :init-keyword (make-keyword name)))
	       (assertion-violation 'make-record-type-descriptor
				    "invalid field" field))) fields))
  (define type (make <record-type-meta>
		 :definition-name name
		 :direct-supers (or (and parent
					 (list (slot-ref parent 'class)))
				    '())
		 :direct-slots (process-fields fields)
		 :define-library (current-library)))
  (define (->field-vector fields)
    (list->vector (map (lambda (field)
			 (if (null? (cddr field))
			     `(immutable ,(car field))
			     `(mutable ,(car field)))) fields)))
  (let ((rtd (make <srfi:record-type-descriptor>
	       :name name :parent parent :uid #f :sealed? #f :opaque? #f
	       :fields (->field-vector fields)
	       :class type)))
    (slot-set! type 'rtd rtd)
    rtd))

(define (make-record rtd field-vector)
  (define type (slot-ref rtd 'class))
  (let ((r (make type)))
    (for-each (lambda (acc obj) (slot-set-using-accessor! r acc obj))
	      (slot-ref type 'getters-n-setters) (vector->list field-vector))
    r))


(define-syntax define-record-type
  (lambda (x)
    (define (parse-type type-spec)
      (syntax-case type-spec ()
	((type-name parent)
	 (list #'type-name #'parent #'(parent)))
	(type-name
	 (identifier? #'type-name)
	 (list #'type-name #f #f))))
    (define (make-ctr type spec fields)
      (syntax-case spec ()
	((ctr constructor-tag ...)
	 (for-all (lambda (ct) 
		    (memp (lambda (ft) (bound-identifier=? ct ft))
			  fields))
		  #'(constructor-tag ...))
	 (with-syntax ((rtd type)
		       ((args ...) fields))
	   #'((define (ctr constructor-tag ...)
		(make-record rtd (vector args ...))))))
	(ctr '())))
    (define (make-pred type pred-spec)
      (if (identifier? pred-spec)
	  (with-syntax ((pred pred-spec)
			(rtd type))
	    #'((define pred (record-predicate rtd))))
	  '()))
    (define (make-accessor type field-spec)
      (define (->reader i name)
	(with-syntax ((acc name)
		      (rtd type)
		      (k i))
	  #'(define acc (record-accessor rtd k))))
      (define (->mutator i name)
	(with-syntax ((mut name)
		      (rtd type)
		      (k i))
	  #'(define mut (record-mutator rtd k))))
      (let loop ((i 0) (field-spec field-spec) (r '()) (m '()))
	(syntax-case field-spec ()
	  (() (list r m))
	  (((f acc mu) rest ...)
	   (loop (+ i 1)
		 #'(rest ...)
		 (cons (->reader i #'acc) r)
		 (cons (->mutator i #'mu) m)))
	  (((f acc) rest ...)
	   (loop (+ i 1)
		 #'(rest ...)
		 (cons (->reader i #'acc) r) m)))))
    (syntax-case x ()
      ((_ type-spec ctr-spec pred-spec (field acc ...) ...)
       (with-syntax* (((rtd) (generate-temporaries '(#t)))
		      ((type parent prtd) (parse-type #'type-spec))
		      ((ctr ...) (make-ctr #'rtd #'ctr-spec #'(field ...)))
		      ((pred ...) (make-pred #'rtd #'pred-spec))
		      (((reader ...) (mutator ...))
		       (make-accessor #'rtd #'((field acc ...) ...))))
	 #'(begin
	     (define rtd (make-record-type-descriptor
			  'type '((field acc ...) ...)
			  prtd))
	     (define-syntax type
	       (syntax-rules ()
		 ((_) rtd)
		 ((_ (keyword datum (... ...)))
		  (keyword datum (... ...) parent (field acc ...) ...))))
	     ctr ...
	     pred ...
	     reader ...
	     mutator ...))))))
	     
  )
	  
