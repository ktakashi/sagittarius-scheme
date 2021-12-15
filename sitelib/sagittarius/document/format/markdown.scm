;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/markdown.scm - Markdown format
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
(library (sagittarius document format markdown)
    (export document->markdown)
    (import (rnrs)
	    (match)
	    (sagittarius document output))

(define (document->markdown doc options out)
  (match doc
    (('document ('info info) ('content elm ...))
     (for-each (lambda (e) (write-markdown e options out)) elm))
    (('document ('@ attr) ('info info) ('content elm ...))
     (for-each (lambda (e) (write-markdown e options out)) elm))
    (else
     (assertion-violation 'document->markdown "Unknown document" doc))))

(define (write-markdown e options out)
  (cond ((string? e) (put-string out e))
	((pair? e)
	 (case (car e)
	   ((section) (write-section e options out))
	   ;; (else (assertion-violation 'write-markdown "Unknown element" e))
	   ))
	(else (put-datum out e))))

(define (write-section e options out)
  (match e
    (('section ('@ attr ...) ('header header ...) content ...)
     (write-header (cons 'header header) options out)
     (for-each (lambda (e) (write-markdown e options out)) content))
    (('section ('header header ...) content ...)
     (write-header (cons 'header header) options out)
     (for-each (lambda (e) (write-markdown e options out)) content))
    (else (assertion-violation 'write-section "Unknown element" e))))

(define (write-header e options out)
  (define (write-it level content)
    (define l (string->number level))
    (cond ((memv l '(1 2))
	   (for-each (lambda (e) (write-markdown e options out)) content)
	   (put-char out #\newline)
	   (if (eqv? l 1)
	       (put-string out "======")
	       (put-string out "------"))
	   (put-char out #\newline))
	  (else
	   (do ((i 0 (+ i 1)))
	       ((= i l))
	     (put-char out #\#))
	   (put-char out #\space)
	   (for-each (lambda (e) (write-markdown e options out)) content)
	   (put-char out #\newline))))
  (match e
    (('header ('@ ('level level)) content ...)
     (write-it level content))
    (('header content ...)
     (write-it "1" content))))

)
