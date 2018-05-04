;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/writer.scm - DOM tree writer
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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
(library (text xml dom writer)
    (export make-dom-writer)
    (import (rnrs)
	    (text xml dom nodes)
	    (srfi :14 char-sets)
	    (srfi :117 list-queues))

(define-record-type xml-write-options
  (fields emit-internal-dtd?))

(define *default-options* (make-xml-write-options #f))

(define make-dom-writer
  (case-lambda
   (() (make-dom-writer *default-options*))
   ((options)
    (case-lambda
     ((tree) (write-dom tree options (current-output-port)))
     ((tree out) (write-dom tree options out))))))
   
(define (write-dom tree options out)
  (unless (document? tree)
    (assertion-violation 'write-dom "DOM document is required" tree))
  (write-xml-decl tree options out)
  (for-each (lambda (child) (write-node child options out))
	    (list-queue-list (node-children tree))))

(define (write-xml-decl tree options out)
  (put-string out "<?xml")
  (when (document-xml-version tree)
    (put-string out " version=\"")
    (put-string out (document-xml-version tree))
    (put-string out "\""))
  (put-string out " encoding=\"")
  (put-string out (document-character-set tree))
  (put-string out "\"")

  (put-string out " standalone=\"")
  (put-string out (if (document-xml-standalone? tree) "yes" "no"))
  (put-string out "\"")
  
  (put-string out "?>"))

(define (write-node tree options out)
  (cond ((hashtable-ref *writer-table* (node-node-type tree) #f) =>
	 (lambda (writer) (writer tree options out)))
	(else (assertion-violation 'write-node "Unknown DOM node" tree))))

(define *writer-table* (make-eqv-hashtable))
(define-syntax define-node-writer
  (syntax-rules ()
    ((_ type (name tree options out) body ...)
     (define name
       (let ((name (lambda (tree options out) body ...)))
	 (hashtable-set! *writer-table* type name)
	 name)))))
(define-node-writer +element-node+ (element-writer e options out)
  (let ((name (node-node-name e)))
    (put-char out #\<)
    (put-string out name)
    (when (element:has-attributes? e)
      (let ((attrs (element-attributes e)))
	(do ((len (named-node-map-length attrs))
	     (i 0 (+ i 1)))
	    ((= i len))
	  (put-char out #\space)
	  (write-node (named-node-map:item attrs i) options out))))
    (let ((content (list-queue-list (node-children e))))
      (cond ((null? content) (put-string out "/>"))
	    (else
	     (put-char out #\>)
	     (for-each (lambda (child) (write-node child options out)) content)
	     (put-string out "</")
	     (put-string out name)
	     (put-char out #\>))))))

(define (make-write/escape attr?)
  (define (write/attr out ch alt)
    (if attr?
	(put-string out alt)
	(put-char out ch)))
  (lambda (out ch)
    (case ch
      ((#\<) (put-string out "&lt;"))
      ((#\>) (put-string out "&gt;"))
      ((#\&) (put-string out "&amp;"))
      ((#\") (write/attr out ch "&quot;"))
      ((#\') (write/attr out ch "&apos;"))
      (else (put-char out ch)))))
(define write/attr-escape (make-write/escape #t))

(define-node-writer +attribute-node+ (attribute-write a options out)
  (define (write-it ch) (write/attr-escape out ch))
  (put-string out (attr-name a))
  (put-string out "=\"")
  (string-for-each write-it (attr-value a))
  (put-string out "\""))

(define write/escape (make-write/escape #f))
(define-node-writer +text-node+ (text-write t options out)
  (define (write-it ch) (write/escape out ch))
  (if (char-ref-text? t)
      (let* ((source (node-source t))
	     (radix (cadr source))
	     (value (caddr source)))
	(case radix
	  ((10) (put-string out "&#"))
	  ((16) (put-string out "&#x"))
	  (else (assertion-violation 'character-ref "invalid radix")))
	(put-string out (number->string value radix))
	(put-char out #\;))
      (string-for-each write-it (character-data-data t))))

(define-node-writer +cdata-section-node+ (cdata-writer t options out)
  (put-string out "<![CDATA[")
  (put-string out (character-data-data t))
  (put-string out "]]>"))

(define-node-writer +entity-reference-node+ (entity-reference er options out)
  (put-char out #\&)
  (put-string out (node-node-name er))
  (put-char out #\;))

(define-node-writer +comment-node+ (comment-writer tree options out)
  (put-string out "<!--")
  (put-string out (character-data-data tree))
  (put-string out "-->"))
(define-node-writer +processing-instruction-node+ (pi-writer pi options out)
  (put-string out "<?")
  (put-string out (processing-instruction-target pi))
  (put-string out " ")
  (put-string out (character-data-data pi))
  (put-string out "?>"))
(define-node-writer +document-type-node+ (doctype-writer doctype options out)
  (cond ((and (document-type-public-id doctype)
	      (document-type-system-id doctype))
	 (put-string out "<!DOCTYPE ")
	 (put-string out (document-type-name doctype))
	 (put-string out " PUBLIC ")
	 (put-string out (document-type-public-id doctype))
	 (put-string out " ")
	 (put-string out (document-type-system-id doctype))
	 (put-string out ">"))
	((document-type-system-id doctype)
	 (put-string out "<!DOCTYPE ")
	 (put-string out (document-type-name doctype))
	 (put-string out " SYSTEM ")
	 (put-string out (document-type-system-id doctype))
	 ;; TODO intSubst?
	 (put-string out ">"))
	((xml-write-options-emit-internal-dtd? options)
	 (assertion-violation 'doctype-writer "not supported yet"))))

)
