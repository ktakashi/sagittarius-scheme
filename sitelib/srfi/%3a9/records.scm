;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; records.scm - SRFI-9 record
;;;  
;;;   Copyright (c) 2000-2012  Takashi Kato  <ktakashi@ymail.com>
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

#!compatible
(library (srfi :9 records)
  (export (rename (srfi:define-record-type define-record-type)))
  (import (rnrs)
	  (sagittarius)
	  (match))

  (define-syntax srfi:define-record-type
    (er-macro-transformer
     (lambda (form rename compare)
       (define (check-syntax type ctr ctr-tag pred fields) 
	 (and (variable? type) (variable? ctr) (variable? pred)
	      (for-all variable? ctr-tag)
	      (map (match-lambda
		       ((tag accessor . rest)
			(and (variable? tag) (variable? accessor)
			     (or (null? rest)
				 (variable? (car rest)))))
		     (_ #f)) fields)
	      (for-all (lambda (ct)
			 (memp (lambda (ft) (compare ct ft))
			       (map car fields)))
		       ctr-tag)))
       (define (unspec-tag field-tag ctr-tag)
	 (filter values
	  (map (lambda (tag)
		 (and (not (exists (lambda (ct) (compare tag ct)) ctr-tag))
		      `(,(rename 'define) ,tag ,(undefined))))
		 field-tag)))
       (define (field-clause fields)
	 (map (lambda (field)
		(if (= (length field) 2)
		    `(immutable . ,field)
		    `(mutable . ,field))) fields))
       (match form
	 ((_ type (ctr ctr-tag ...) pred fields ...)
	  (unless (check-syntax type ctr ctr-tag pred fields)
	    (syntax-violation 'define-record-type
			      "malformed define-record-type"
			      form))
	  (let ((field-tag (map car fields))
		(_lambda (rename 'lambda)))
	    `(,(rename 'define-record-type) (,type ,ctr ,pred)
	      (sealed #t)
	      (protocol (,_lambda (ctor)
			  (,_lambda (,@ctr-tag)
			    ,@(unspec-tag field-tag ctr-tag)
			    (ctor ,@field-tag))))
	      (fields ,@(field-clause fields)))))))))
)
