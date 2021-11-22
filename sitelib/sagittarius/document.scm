;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document.scm - Generic document format
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

;; Sagittarius has self documentation ecosystem. Here we specify
;; what's the structure of the document.
;; Basically a document is a sxml with schema.
;; - `document`: the root element
;;   - `table-of-contents`: table of contents, must appear max once
;;                          right after `document`
;;   - `secton`*          : a container, may appear multiple times
;;   - `appendix`         : appendix
;; 
;; The above tags can contain inline tags
;; - header (@ (level "1"))         : H1, level can be up to 6 (h6)
;; - codeblock (@ (style "scheme")) : <pre><code>...</code></pre> equivalent
;; - list (@ (style "number"))      : <ul> or <ol>
;;   - item                         : <li>
;; - dlist                          : <dl>
;;   - term                         : <dt>
;;   - desc                         : <dd>
;; ... TODO
;;
;; All tags can contain attributes. The following attributes are
;; meta info, these are most likely injected by the parser
;; - location: Source file location
;; - file    : Source file name

#!nounbound
(library (sagittarius document)
    (export file->document
	    port->document
	    write-document)
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius))

(define (file->document type file)
  (call-with-input-file file (lambda (p) (port->document type p))))

(define (port->document type port)
  (define name (string->symbol
		(string-append (symbol->string type) "->document")))
  (let ((proc (load-procedure type name)))
    (proc port)))

(define write-document
  (case-lambda
   ((type document) (write-document type document (current-output-port)))
   ((type document out)
    (define name (string->symbol
		  (string-append "document->" (symbol->string type))))
    (let ((proc (load-procedure type name)))
      (proc document out)))))

(define (load-procedure type name)
  (define fmt
    "(sagittarius document format ~a) doesn't exist or ~a isn't defined")
  (guard (e (else (error type (format fmt type name))))
    (eval name (environment `(sagittarius document format ,type)))))

)
