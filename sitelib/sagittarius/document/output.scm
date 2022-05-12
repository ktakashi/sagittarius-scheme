;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/output.scm - Document output
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
(library (sagittarius document output)
    (export (rename (document-output-options <document-output-options>))
	    document-output-options?
	    document-output-options-default-codeblock-language
	    document-output-options-link-source-callback
	    document-output-options-unknown-node-handler
	    document-output-options-builder

	    write-document

	    document-output:make-file-link-callback
	    document-decompose
	    document-element-of?

	    document-output-error document-output-error? &document-output

	    ;; For writer implementation
	    (rename (document-writer <document-writer>))
	    document-writer?
	    document-writer-output-port
	    document-writer-options
	    document-writer-node-handlers
	    document-writer-string-handler
	    document-writer-datum-handler
	    default-unknown-node-handler
	    traverse-document
	    )
    (import (rnrs)
	    (rnrs eval)
	    (record builder)
	    (sagittarius document conditions)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (text sxml tools)
	    (text sxml sxpath)
	    (util file))

(define-condition-type &document-output &document
  make-document-output-error document-output-error?)
(define (document-output-error who message . irr)
  (raise (condition (make-document-output-error)
		    (make-who-condition who)
		    (make-message-condition message)
		    (make-irritants-condition irr))))

(define-record-type document-output-options
  (fields default-codeblock-language
	  link-source-callback
	  unknown-node-handler
	  resolve-table-of-contents
	  resolve-index-table?
	  author-resolver
	  execute-eval-on))

(define (write-document writer document options out)
  (define resolve-toc
    (and options (document-output-options-resolve-table-of-contents options)))
  (define author-resolver
    (and options (document-output-options-author-resolver options)))
  (define execute-eval-on
    (and options (document-output-options-execute-eval-on options)))
  (define resolve-index-table?
    (and options (document-output-options-resolve-index-table? options)))
  (if (or resolve-toc author-resolver execute-eval-on)
      (let ((resolved (resolve-marker document resolve-toc
				      resolve-index-table?
				      author-resolver execute-eval-on)))
	(writer resolved options out))
      (writer document options out)))

(define (resolve-marker document resolve-toc index-table?
			author-resolver eval-env)
  (define (collect-toc e depth)
    (define (do-collect)
      (define (id/tag header)
	(string-append "#" (cond ((sxml:attr header 'tag))
				 ((sxml:attr header 'id))
				 (else "this-should-not-happen"))))
      (let loop ((current 1)
		 (items (list-queue))
		 (headers ((sxpath '(// section header)) `(*TOP* ,document))))
	(if (null? headers)
	    (values (list-queue-list items) headers)
	    (let* ((header (car headers))
		   (level (string->number (sxml:attr header 'level))))
	      (cond ((> level depth) (loop current items (cdr headers)))
		    ((> level current)
		     ;; dive in
		     (let-values (((children next)
				   (loop level (list-queue) (cdr headers))))
		       (let ((last (list-queue-remove-back! items)))
			 (list-queue-add-back! items
			  (append last 
				  `((list (@ (style "number")) ,@children))))
			 (loop current items next))))
		    ((= level current)
		     (list-queue-add-back! items
		      `(item (@)
			     (link (@ (href ,(id/tag header)))
				   ,@(sxml:content header))))
		     (loop current items (cdr headers)))
		    ((< level current)
		     (values (list-queue-list items) headers)))))))
    (let-values (((r ignore) (do-collect)))
      `(list (@ (id "table-of-contents")
		(style "number"))
	     ,@r)))
  (define (do-eval e env)
    (let ((content (sxml:content e)))
      (eval (get-datum (open-string-input-port (car content)))
	    env)))
  (define (collect-indice e)
    e)
    ;; TODO 
  ;; We do stupidly here...
  (let loop ((document document))
    (cond ((null? document) '())
	  ((pair? document)
	   (case (car document)
	     ((table-of-contents)
	      (if resolve-toc (collect-toc document resolve-toc) document))
	     ((eval)
	      (if eval-env (do-eval document eval-env) document))
	     ((index-table)
	      (if index-table? (collect-indice document) document))
	     ((author)
	      (if author-resolver (author-resolver document) document))
	     (else (let ((content (sxml:content document)))
		     (sxml:change-content document (map loop content))))))
	  (else document))))

(define (default-unknown-node-handler writer name attr content travarse)
  (document-output-error 'write-markdown "Unknown element"
			 (cons* name attr content)))

(define-syntax document-output-options-builder
  (make-record-builder document-output-options
    ((unknown-node-handler default-unknown-node-handler))))

(define (document-output:make-file-link-callback ext)
  (lambda (source format writer)
    (let ((file (string-append (path-sans-extension source) ext)))
      (when writer (writer (list file) file))
      file)))

(define (document-decompose doc)
  (let ((name (car doc))
	(attr (sxml:attr-list doc))
	(content (sxml:content doc)))
    (values name attr content)))

(define (document-element-of? e name)
  (and (pair? e) (eq? (car e) name)))

(define-record-type document-writer
  (fields output-port
	  options
	  node-handlers
	  string-handler
	  datum-handler))

(define (traverse-document writer e)
  (define (noop e))
  (define (call-node-handler writer name attr content)
    (define node-handlers (document-writer-node-handlers writer))
    (let ((handler (cond ((assq name node-handlers) => cadr)
			 (else (document-output-options-unknown-node-handler
				(document-writer-options writer))))))
      (handler writer name attr content traverse)))
  (define (traverse e)
    (cond ((string? e)
	   ((document-writer-string-handler writer) writer e noop))
	  ((pair? e)
	   (let-values (((name attr content) (document-decompose e)))
	     (call-node-handler writer name attr content)))
	  (else
	   ((document-writer-datum-handler writer) writer e noop))))
  (traverse e))


)
