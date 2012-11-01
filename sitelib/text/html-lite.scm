;;; -*- Scheme -*-
;;;
;;; html-lite.scm - Simple HTML document builder
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

;; API names are taken from Gauche
(library (text html-lite)
    (export html-escape
	    html-escape-string
	    html-doctype
	    ;; here I actually need to list up the exported procedures,
	    ;; but sorry I'm lazy so I used (export ...) syntax...
	    )
    (import (rnrs) (sagittarius)
	    (util port))

  (define (html-escape :optional (in (current-input-port)))
    (call-with-string-output-port
     (lambda (out)
       (let loop ((c (get-char in)))
	 (unless(eof-object? c)
	   (case c
	     ((#\<) (put-string out "&lt;"))
	     ((#\>) (put-string out "&gt;"))
	     ((#\&) (put-string out "&amp;"))
	     ((#\") (put-string out "&quot;"))
	     (else (put-char out c)))
	   (loop (get-char in)))))))

  (define (html-escape-string s)
    (html-escape (open-string-input-port s)))

  ;; Doctype
  (define-constant *doctype-alist*
    '(((:html-5)
       #f
       "<!DOCTYPE html>")
      ((:strict :html :html-strict :html-4.01 :html-4.01-strict)
       #f
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
       \"http://www.w3.org/TR/html4/strict.dtd\">\n")
      ((:transitional :html-transitional :html-4.01-transitional)
       #f
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
       \"http://www.w3.org/TR/html4/loose.dtd\">\n")
      ((:frameset :html-frameset :html-4.01-frameset)
       #f
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
       \"http://www.w3.org/TR/html4/frameset.dtd\">\n")
      ((:xhtml-1.0-strict :xhtml-1.0)
       #t
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
      ((:xhtml-1.0-transitional)
       #t
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
      ((:xhtml-1.0-frameset)
       #t
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n")
      ((:xhtml-1.1)
       #t
       "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
       \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n")
      ))

  (define (html-doctype :key (type :html-4.01-strict))
    (cond ((find (lambda (e) (memq type (car e))) *doctype-alist*)
	   => caddr)
	  (else (assertion-violation 'html-doctype
				     "Unknown doctype type spec" type))))

  (define (make-html-element name empty?)
    (define (get-attr args attrs)
      (cond ((null? args) (values (reverse! attrs) args))
	    ((keyword? (car args))
	     (cond ((null? (cdr args))
		    (values (reverse! (cons* (car args) " " attrs)) args))
		   ((eq? (cadr args) #f)
		    (get-attr (cddr args) attrs))
		   ((eq? (cadr args) #t)
		    (get-attr (cddr args) (cons* (car args) " " attrs)))
		   (else
		    (get-attr (cddr args)
			      (cons* (format "=\"~a\""
					     (html-escape-string 
					      (format "~a" (cadr args))))
				     (car args)
				     " "
				     attrs)))))
	    (else (values (reverse! attrs) args))))
    (if empty?
	(lambda args
	  (receive (attr args) (get-attr args '())
	    (unless (null? args)
	      (assertion-violation 'make-html-element
				   (format "element ~s can't have content" name)
				   args))
	    (list "<" name attr " />")))
	(lambda args
	  (receive (attr args) (get-attr args '())
	    (list "<" name attr ">" args "</" name ">")))))

  (define-syntax define-html-elements
    (lambda (x)
      (define (make-name n)
	(string->symbol (format "html:~a" (identifier->symbol n))))
      (define (build specs)
	(let loop ((specs specs) (r '()))
	  (cond ((null? specs) (reverse! r))
		((and (pair? (car specs)) (eqv? :empty (caar specs)))
		 (let* ((spec (cadar specs))
			(name (make-name spec)))
		   (loop (cdr specs)
			 (cons* `(define ,name
				   (make-html-element 
				    ',(identifier->symbol spec) #t))
				`(export ,name)
				r))))
		(else
		 (let* ((spec (car specs))
			(name (make-name spec)))
		   (loop (cdr specs)
			 (cons* `(define ,name
				   (make-html-element
				    ',(identifier->symbol spec) #f))
				`(export ,name)
				r)))))))
      (syntax-case x ()
	((_ specs ...)
	 #`(begin #,@(build #'(specs ...)))))))

  ;; http://www.w3.org/TR/html-markup/elements-by-function.html

  ;; The root element
  (define-html-elements html)

  ;; Document metadata
  (define-html-elements head title (:empty base) (:empty link)
    (:empty meta) style)

  ;; Scripting
  (define-html-elements script noscript)

  ;; Sections
  (define-html-elements body section nav article aside h1 h2 h3 h4 h5 h6
    hgroup header footer address)

  ;; Grouping content
  (define-html-elements p (:empty hr) (:empty br) pre blockquote 
    ol ul li dl dt dd figure figcaption div)

  ;; Text-level semantics
  (define-html-elements a em strong small s cite q dfn abbr time code
    var samp kbd sub sup i b u mark ruby rt rp bdi bdo span (:empty wbr))

  ;; Edits
  (define-html-elements ins del)

  ;; Embedded content
  (define-html-elements (:empty img) iframe (:empty embed) object
    (:empty param) video audio (:empty source) (:empty track) canvas
    map (:empty area))

  ;; Tables
  (define-html-elements table caption colgroup (:empty col) 
    tbody thead tfoot tr td th)

  ;; Forms
  (define-html-elements form fieldset legend label (:empty input) button
    select datalist optgroup option textarea keygen output progress meter)

  ;; Interactive elements
  (define-html-elements details summary (:empty command) menu)
)