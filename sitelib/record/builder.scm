;;; record/builder.scm -*- mode:scheme;coding:utf-8 -*-
;;;
;;; record/builder.scm - Record builder library
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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
(library (record builder)
    (export make-record-builder from)
    (import (rnrs)
	    (srfi :1 lists) ;; for lset-difference
	    )

(define-syntax from (syntax-rules ()))
(define-syntax make-record-builder
  (lambda (xx)
    (define (->name rt)
      (string->symbol
       (string-append
	(symbol->string (syntax->datum rt)) "-builder")))
    (define (collect-defaults k fields)
      (let loop ((acc '()) (fields fields))
	(syntax-case fields ()
	  (((field default-value) rest ...)
	   (loop (cons (list #'field #'default-value #f) acc) #'(rest ...)))
	  (((field default-value conv) rest ...)
	   (loop (cons (list #'field #'default-value #'conv) acc) #'(rest ...)))
	  (() acc))))
    (syntax-case xx ()
      ((k ?record-type)
       #'(k ?record-type ()))
      ((kk ?record-type ((?fields ...) ...))
       (with-syntax ((?name (datum->syntax #'kk (->name #'?record-type)))
		     (((?field ?default-value ?converter) ...)
		      (collect-defaults #'kk #'((?fields ...) ...)))
		     ((rtd) (generate-temporaries '("rtd"))))
	 #'(lambda (x)
	     (syntax-case x (from)
	       ((k (from record) (name value) (... ...))
		#'(apply (record-constructor
			  (record-constructor-descriptor ?record-type))
			 (sort-values (record-type-descriptor ?record-type)
			  (list (cons* '?field ?default-value ?converter) ...)
			  (merge-values
			   (record-type-descriptor ?record-type)
			   record
			   (list (cons 'name value) (... ...))))))
	       ((k (name value) (... ...))
		#'(apply (record-constructor
			  (record-constructor-descriptor ?record-type))
			 (sort-values
			  (record-type-descriptor ?record-type)
			  (list (cons* '?field ?default-value ?converter) ...)
			  (list (cons 'name value) (... ...))))))))))))

(define (merge-values rtd record provided-values)
  (define (child-of? rtd record)
    (let loop ((child (record-rtd record)))
      (cond ((not child) #f)
	    ((eq? child rtd))
	    (else (loop (record-type-parent child))))))
  (define (collect-fields&value rtd record)
    (let loop ((rtd rtd) (r '()))
      (if rtd
	  (do ((i 0 (+ i 1))
	       (fields (record-type-field-names rtd))
	       (r r (cons (cons (vector-ref fields i)
				((record-accessor rtd i) record))
			  r)))
	      ((eq? i (vector-length fields))
	       (loop (record-type-parent rtd) r)))
	  r)))
  ;; is this actually valid in R6RS?
  (unless (child-of? rtd record)
    (assertion-violation 'record-builder "Wrong record type" record))
  (let ((record-values (collect-fields&value rtd record)))
    (lset-union (lambda (a b) (eq? (car a) (car b)))
		provided-values record-values)))

(define (sort-values rtd default-values provided-values)
  (define fields (collect-fields rtd))
  (define (emit fields values)
    (define (find-value field values)
      (cond ((assq field values) =>
	     (lambda (slot)
	       (let ((v (cdr slot)))
		 (cond ((assq field default-values) =>
			(lambda (d)
			  (let ((conv (cddr d)))
			    (if conv (conv v) v))))
		       (else v)))))
	    ((assq field default-values) =>
	     (lambda (fvd)
	       (let ((v (cadr fvd))
		     (conv (cddr fvd)))
		 (if conv (conv v) v))))
	    (else #f)))
    (do ((fields fields (cdr fields))
	 (acc '() (cons (find-value (car fields) values) acc)))
	((null? fields) (reverse acc))))
  (let ((non-exists (lset-difference eq? (map car provided-values) fields)))
    (unless (null? non-exists)
      (assertion-violation 'record-builder
			   "Unknown fields" non-exists)))
  (emit fields provided-values))

(define (collect-fields rtd)
  (let loop ((fields-list '()) (rtd rtd))
    (if rtd
	(loop (cons (vector->list (record-type-field-names rtd)) fields-list)
	      (record-type-parent rtd))
	(apply append fields-list))))

)
