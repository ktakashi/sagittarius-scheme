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
    (export make-record-builder)
    (import (rnrs)
	    (rnrs r5rs))

(define-syntax make-record-builder
  (lambda (xx)
    (define (->name rt)
      (string->symbol
       (string-append
	(symbol->string (syntax->datum rt)) "-builder")))
    (syntax-case xx ()
      ((k ?record-type)
       #'(k ?record-type ()))
      ((kk ?record-type ((?field ?default-value) ...))
       (with-syntax ((?name (datum->syntax #'kk (->name #'?record-type)))
		     ((rtd) (generate-temporaries '("rtd"))))
	 #'(lambda (x)
	     (syntax-case x ()
	       ((k (name value) (... ...))
		#'(let ()
		    (define rtd (record-type-descriptor ?record-type))
		    (define rcd (record-constructor-descriptor ?record-type))
		    (define ctr (record-constructor rcd))
		    (apply ctr
			   (sort-values rtd
			    (list (cons '?field ?default-value) ...)
			    (list (cons 'name value) (... ...)))))))))))))

(define (sort-values rtd default-values provided-values)
  (define fields (collect-fields rtd))
  (define (emit fields values)
    (define (find-value field values)
      (cond ((assq field values) => cdr)
	    ((assq field default-values) => cdr)
	    (else #f)))
    (do ((fields fields (cdr fields))
	 (acc '() (cons (find-value (car fields) values) acc)))
	((null? fields) (reverse acc))))
  (emit fields provided-values))

(define (collect-fields rtd)
  (let loop ((fields-list '()) (rtd rtd))
    (if rtd
	(loop (cons (vector->list (record-type-field-names rtd)) fields-list)
	      (record-type-parent rtd))
	(apply append fields-list))))

)
