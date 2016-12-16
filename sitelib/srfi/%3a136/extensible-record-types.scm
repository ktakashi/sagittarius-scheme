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

	  (rename (srfi:define-record-type define-record-type)))
  (import (rename (except (rnrs) make-record-type-descriptor)
		  (record-type-descriptor r6:record-type-descriptor)
		  (record-constructor-descriptor r6:record-constructor-descriptor))
		  
	  (only (core record procedural) <record-type-descriptor>)
	  (srfi :1)
	  (clos user)
	  (clos core)
	  (sagittarius)
	  ;; for <record-type-meta>
	  (sagittarius clos)
	  (sagittarius control))

(define (make-record-type-descriptor name fields :optional (parent #f))
  (define (process-fields fields)
    ;; -> slot form
    (map (lambda (field)
	   (if (pair? field)
	       (let ((len (length field))
		     (name (cadr field)))
		 (unless (= len 2)
		   (assertion-violation 'make-record-type-descriptor
					"invalid field spec" field))
		 (list name :mutable (eq? (car field) 'mutable)
		       :init-keyword (make-keyword name)))
	       (list name :mutable #t :init-keyword field))) fields))
  (define type (make <record-type-meta>
		 :definition-name name
		 :direct-supers (or (and parent
					 (list (slot-ref parent 'class)))
				    '())
		 :direct-slots (process-fields fields)
		 :define-library (current-library)))
  (define (->field-vector fields)
    (list->vector (map (lambda (field)
			 (if (symbol? field)
			     `(mutable ,field)
			     field)) fields)))
  (let ((rtd (make <record-type-descriptor>
	       :name name :parent parent :uid #f :sealed? #f :opaque? #f
	       :fields (->field-vector fields)
	       :class type)))
    (slot-set! type 'rtd rtd)
    rtd))

(define (record-type-fields rtd)
  (define (make-field rtd k field)
    (list field (record-accessor rtd k)
	  (if (record-field-mutable? rtd k)
	      (record-mutator rtd k)
	      #f)))
  (let ((fields (record-type-field-names rtd)))
    (do ((len (vector-length fields))
	 (i 0 (+ i 1))
	 (r '() (cons (make-field rtd i (vector-ref fields i)) r)))
	((= i len) (reverse! r)))))

(define (make-record rtd field-vector)
  (define type (slot-ref rtd 'class))
  (let ((r (make type)))
    (for-each (lambda (acc obj) (slot-set-using-accessor! r acc obj))
	      (slot-ref type 'getters-n-setters) (vector->list field-vector))
    r))

(define (record-type-descriptor record)
  (let ((class (class-of record)))
    (slot-ref class 'rtd)))

(define-syntax %rcd (syntax-rules ()))
(define-syntax srfi:define-record-type
  (lambda (x)
    (define (parse-type-spec spec)
      (syntax-case spec ()
	((type parent)
	 (identifier? #'parent)
	 (list #'type #'parent #'((parent-rtd (parent) (parent %rcd)))))
	((type parent)
	 (eq? #f #'parent)
	 (list #'type #f '()))
	(type
	 (identifier? #'type)
	 (list #'type #f '()))))
    (define (make-ctr spec)
      (syntax-case spec ()
	((name args ...) #'name)
	(name (identifier? #'name) #'name)
	(name (eq? #f #'name) #'dummy)))
    (define (make-protocol parent? spec fields)
      (define (fill args fields)
	(define undef #'(undefined))
	(let loop ((fields fields) (r '()))
	  (syntax-case fields ()
	    (() (reverse! r))
	    (((f acc m ...) rest ...)
	     (loop #'(rest ...)
		   (cond ((memp (lambda (a) (bound-identifier=? a #'f)) args)
			  (cons #'f r))
			 ((memp (lambda (a) (bound-identifier=? a #'acc))
				args)
			  (cons #'acc r))
			 (else (cons undef r))))))))
      (define (split-args args fields)
	(let loop ((args args) (p '()))
	  (syntax-case args ()
	    (() (list (reverse! p) '()))
	    ((field rest ...)
	     (if (memp (lambda (f) (bound-identifier=? (car f) #'field)) fields)
		 (list (reverse! p) #'(field rest ...))
		 (loop #'(rest ...) (cons #'field p)))))))
      (if parent?
	  (syntax-case spec ()
	    ((name args ...)
	     (with-syntax* ((((pargs ...) (targs ...))
			     (split-args #'(args ...) fields))
			    ((vars ...) (fill #'(targs ...) fields)))
	       #'(lambda (n)
		   (lambda (pargs ... targs ...)
		     ((n pargs ...) vars ...)))))
	    ;; this isn't efficient at all ...
	    (_
	     (with-syntax ((ln (length fields)))
	       #'(lambda (n)
		   (lambda args
		     (let-values (((p t) (split-at args (- (length args) ln))))
		       (apply (apply n p) t)))))))
	  (syntax-case spec ()
	    ((name args ...)
	     (with-syntax (((vars ...) (fill #'(args ...) fields)))
	       #'(lambda (p)
		   (lambda (args ...)
		     (p vars ...)))))
	    (_
	     (with-syntax (((args ...) fields))
	       #'(lambda (p)
		   (lambda (args ...)
		     (p args ...))))))))
    (define (convert-fields fields)
      (let loop ((fields fields) (r '()))
	(syntax-case fields ()
	  (() (reverse! r))
	  (((name a) rest ...)
	   (with-syntax (((n) (if (identifier? #'name)
				  #'(name)
				  (generate-temporaries '(#t)))))
	     (loop #'(rest ...) (cons #'(immutable n a) r))))
	  (((name a m) rest ...)
	   (with-syntax (((n) (if (identifier? #'name)
				  #'(name)
				  (generate-temporaries '(#t)))))
	     (loop #'(rest ...) (cons #'(mutable n a m) r)))))))
    
    (syntax-case x ()
      ((_ type-spec ctr-spec pred-spec field-spec ...)
       (with-syntax* (((real dummy) (generate-temporaries '(#f #f)))
		      ((type parent (parent-rtd ...))
		       (parse-type-spec #'type-spec))
		      (ctr (make-ctr #'ctr-spec))
		      (proto (make-protocol #'parent #'ctr-spec
					    #'(field-spec ...)))
		      (predicate (if (identifier? #'pred-spec)
				     #'pred-spec
				     (car (generate-temporaries '(#f)))))
		      ((field ...) (convert-fields #'(field-spec ...))))
	 #'(begin
	     (define-syntax type
	       (syntax-rules (%rcd)
		 ((_) (r6:record-type-descriptor real))
		 ((_ %rcd) (r6:record-constructor-descriptor real))
		 ((_ (keyword datum (... ...)))
		  (keyword datum (... ...) parent field-spec ...))))
	     (define-record-type (real ctr predicate)
	       parent-rtd ... ;; parent-rtd clause or '()
	       (protocol proto)
	       (fields field ...))
	     ;; given type must be a macro so overwrite the
	     ;; name here.
	     (define dummy
	       (let ((rtd (r6:record-type-descriptor real)))
		 (slot-set! rtd 'name 'type)))))))))

  )
	  
