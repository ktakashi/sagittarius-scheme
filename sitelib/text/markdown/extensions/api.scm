;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/extensions/api.scm - Extension API
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
(library (text markdown extensions api)
    (export markdown-extension-builder
	    markdown-extension?
	    markdown-extension-custom-block-factories
	    markdown-extension-custom-inline-content-factories
	    markdown-extension-custom-delimiter-processors
	    markdown-extension-custom-reference-processors
	    
	    combine-markdown-extensions

	    define-markdown-converter
	    markdown-converter:merge
	    )
    (import (rnrs)
	    (record accessor)
	    (record builder)
	    (srfi :1 lists)
	    (text markdown converter api))

(define-record-type markdown-extension
  (fields custom-block-factories
	  custom-inline-content-factories
	  custom-delimiter-processors
	  custom-reference-processors))
(define (make-check-list who)
  (lambda (v)
    (unless (or (null? v) (pair? v))
      (assertion-violation who "Must be a list" v))
    v))
(define-syntax markdown-extension-builder
  (make-record-builder markdown-extension
   ((custom-block-factories '() (make-check-list 'custom-block-factories))
    (custom-inline-content-factories '()
     (make-check-list 'custom-inline-content-factories))
    (custom-delimiter-processors '() 
     (make-check-list 'custom-delimiter-processors))
    (custom-reference-processors '()
     (make-check-list 'custom-reference-processors)))))

(define empty-extension (markdown-extension-builder))

(define *markdown-extension-accessors*
  (record-type-all-field-accessors (record-type-descriptor markdown-extension)))
(define (combine-markdown-extensions . extension*)
  (define (combine-fields e*)
    (map (lambda (accessor) (append-map accessor e*))
	 *markdown-extension-accessors*))
  (if (null? extension*)
      empty-extension
      (apply make-markdown-extension (combine-fields extension*))))

)
    
