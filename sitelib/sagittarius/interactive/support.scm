;;; -*- Scheme -*-
;;;
;;; support.scm - REPL support library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

#< (sagittarius regex) >
(library (sagittarius interactive support)
    (export document)
    (import (rnrs)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (sagittarius)
	    (sagittarius io)
	    (sagittarius object)
	    (sagittarius process)
	    (text html-lite) ;; for html-escape-string
	    (text sxml htmlprag)
	    (text sxml sxpath)
	    (text sxml tools))

  (define *parsed-document* (make-parameter #f))

  (cond-expand
   (windows
    (define document-path (string-append (sagittarius-installed-directory)
					 "doc/sagittarius-ref.html")))
   (else
    ;; we need to get it from sagittarius-config
    (define document-path
      (string-append
       (or (guard (e (else #f))
	     (and-let* ((v (string-trim-right
			    (with-output-to-string
			      (cut run "sagittarius-config" "--sharedir"))))
			( (not (string-null? v))))
	       v))
	   ;; default /usr/local/share/sagittarius/{version}
	   (format "/usr/local/share/sagittarius/~a" (sagittarius-version)))
       "/doc/sagittarius-ref.html"))
    ))

  ;; lookup given name document
  ;; it's better to define this macro so that we can write it like
  ;; (document define).
  (define-syntax document
    (syntax-rules ()
      ((_ name)
       (%document (->string 'name)))))

  ;; parse document file to sxml and look it up
  (define (%document oname :optional (port (current-output-port)))
    (unless (*parsed-document*)
      (*parsed-document* (call-with-input-file document-path 
			 (cut html->sxml <>))))
    (let ((sxml (*parsed-document*))
	  (name (html-escape-string oname)))
      (or (and-let* ((base-path (format "//div[@class = 'define']/a/span[@name = '~a']/parent::node()/parent::node()" name))
		     (def-path (if-sxpath base-path))
		     (defines (def-path sxml))
		     (desc-path
		      ;; it can get multiple description depending on how many
		      ;; 'define' are there. and of course each of them are
		      ;; the same so we can simply get the first element.
		      (if-car-sxpath (string-append base-path "/following-sibling::node()[name() = 'div' and @class = 'desc'][1]")))
		     (desc (desc-path sxml)))
	    (display "Definition(s):" port) (newline port)
	    (for-each (cut print-definition <> port) defines)
	    (newline port)
	    (display "Description:" port) (newline port)
	    (print-document desc port))
	  (and (print-document `("document for " ,oname " not found\n") port)
	       #f))))

  (define (print-definition shtml out)
    (and-let* ((name ((if-car-sxpath "//span[@class = 'name']") shtml))
	       (args ((if-car-sxpath "//span[@class = 'args']") shtml)))
      (display `(,@(sxml:content name) ,@(sxml:content args)) out)
      (newline out)))

  (define (print-document shtml out)
    (let loop ((docs shtml))
      (cond ((null? docs))
	    ;; ignore html attribute
	    ((and (pair? docs) 
		  (pair? (car docs))
		  (eq? (caar docs) '@))
	     (loop (cdr docs)))
	    ((pair? docs)
	     (loop (car docs)) (loop (cdr docs)))
	    ((string? docs) (display docs))
	    ((eq? docs 'p) (newline out))
	    ;; ignore the rest
	    (else))))
)