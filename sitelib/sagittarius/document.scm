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
;;   - `info`   : document info, e.g. copy right
;;   - `content`: document itself
;;
;; - `content`
;;   - `section`* : document section
;;   - `appendix`*: appendix
;;   - any*       : anything
;; 
;; The above tags can contain inline tags
;; - header (@ (level "1"))         : H1, level can be up to 6 (h6)
;; - codeblock (@ (style "scheme")) : <pre><code>...</code></pre> equivalent
;; - list (@ (style "number"))      : <ul> or <ol>
;;   - item                         : <li>
;; - dlist                          : <dl>
;;   - term                         : <dt>
;;   - desc                         : <dd>
;; - define                         : <dfn>
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
	    parse-document
	    write-document


	    file->document-input
	    port->document-input

	    <document-output-options>
	    document-output-options?
	    document-output-options-builder
	    document-output:make-file-link-callback

	    <document-input-options>
	    document-input-options?
	    document-input-options-builder
	    
	    &document document-error?
	    &document-input document-input-error?
	    )
    (import (rnrs)
	    (sagittarius document conditions)
	    (sagittarius document loader)
	    (sagittarius document input)
	    (sagittarius document output))

(define (file->document type file . rest)
  (apply parse-document type (file->document-input file) rest))

(define (port->document type port . rest)
  (apply parse-document type (port->document-input port) rest))

(define (parse-document type input . opt)
  (let ((proc (load-reader-procedure type)))
    (apply parse-document-input proc input opt)))

(define write-document
  (case-lambda
   ((type document)
    (write-document type document (document-output-options-builder)))
   ((type document options)
    (write-document type document options (current-output-port)))
   ((type document options out)
    (let ((proc (load-writer-procedure type)))
      (proc document options out)))))
)
