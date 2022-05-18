;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/tools.scm - Document tools
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
(library (sagittarius document tools)
    (export document:content
	    document:change-content
	    document:info

	    document:adjust-sections)
    (import (rnrs)
	    (match)
	    (srfi :1 lists)
	    (text sxml tools))

(define (document:content document)
  (match document
    (('document ('info info ...) ('content elm ...))
     (cons 'content elm))
    (('document ('@ attr) ('info info ...) ('content elm ...))
     (cons 'content elm))
    (else #f)))

(define (document:change-content document content)
  (match document
    (('document ('info info ...) ('content elm ...))
     `(document (info ,@info) ,content))
    (('document ('@ attr) ('info info ...) ('content elm ...))
     `(document (@ ,attr)(info ,@info) ,content))
    (else #f)))

(define (document:info document)
  (match document
    (('document ('info info ...) ('content elm ...)) (cons 'info info))
    (('document ('@ attr) ('info info ...) ('content elm ...))
     (cons 'info info))
    (else #f)))

(define (document:adjust-sections document)
  (define (adjust-sections content)
    (let loop ((elements (list content))
	       (contents (sxml:content content))
	       (r '()))
      (if (null? contents)
	  (values (sxml:change-content (car elements) (reverse! r))
		  (cdr elements)
		  contents)
	  (let ((e (car contents)))
	    (if (pair? e) ;; an element
		(case (sxml:name e)
		  ((included) ;; put the content before the current content
		   (loop elements (append (sxml:content e) (cdr contents)) r))
		  ((section)
		   (let ((l (string->number (sxml:attr e 'level)))
			 (cur (- (length elements) 1))
			 (c* (sxml:content e)))
		     (cond ((> l cur)
			    (let-values (((ne elements next)
					  (loop (cons e elements) c* '())))
			      (loop elements (append next (cdr contents))
				    (cons ne r))))
			   ((= l cur)
			    (values (sxml:change-content (car elements)
							 (reverse! r))
				    (cdr elements)
				    contents))
			   (else
			    (let ((ne (sxml:change-content (car elements)
							   (reverse! r))))
			      (values ne (cdr elements) contents))))))
		  (else
		   ;; there should not be any section in other tags (I hope...)
		   (loop elements (cdr contents) (cons e r))))
		(loop elements (cdr contents) (cons e r)))))))
  (let ((content (document:content document)))
    (let-values (((new-content ignore1 ignroe2) (adjust-sections content)))
      (document:change-content document new-content))))

)
