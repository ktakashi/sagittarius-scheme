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
	    
	    (rename (make-table-of-contents-resolver
		     make-default-table-of-contents-resolver)
		    (make-eval-resolver make-default-eval-resolver)
		    (make-index-table-resolver
		     make-default-index-table-resolver))
	    
	    write-document

	    document-output:make-file-link-callback
	    ;; document-output:resolve-marker
	    ;; document-output:eval
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
	    (sagittarius document tools)
	    (srfi :1 lists)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (text sxml tools)
	    (text sxml sxpath)
	    (util file)
	    (pp))

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
	  table-of-contents-resolver
	  index-table-resolver
	  author-resolver
	  eval-executor
	  section-splitter))

(define-syntax document-output-options-builder
  (make-record-builder document-output-options
    ((unknown-node-handler default-unknown-node-handler))))

(define *document-output-resolver* (make-parameter #f))
(define-record-type document-marker-resolver
  (fields document
	  resolvers)
  (protocol (lambda (p)
	      (lambda (doc)
		(p doc (make-eq-hashtable))))))

(define (write-document writer document options out)
  (define resolver (make-document-marker-resolver document))
  (define ht (document-marker-resolver-resolvers resolver))
  (when options
    (cond ((document-output-options-table-of-contents-resolver options) =>
	   (lambda (proc) (hashtable-set! ht 'table-of-contents proc))))
    (cond ((document-output-options-author-resolver options) =>
	   (lambda (proc) (hashtable-set! ht 'author proc))))
    (cond ((document-output-options-eval-executor options) =>
	   (lambda (proc) (hashtable-set! ht 'eval proc))))
    (cond ((document-output-options-index-table-resolver options) =>
	   (lambda (proc) (hashtable-set! ht 'index-table proc)))))
  (parameterize ((*document-output-resolver* resolver))
    (cond ((document-output-options-section-splitter options) =>
	   (lambda (splitter)
	     ;; we can't generate ToC, so ignore it
	     (hashtable-set! ht 'table-of-contents (lambda (e) ""))
	     (let ((q (list-queue)))
	       (write-splitted-sections! q document
					 writer splitter options out)
	       (for-each (lambda (proc) (proc writer)) (list-queue-list q)))))
	  (else (writer document options out)))))

(define (write-splitted-sections! queue document writer splitter options out)
  (define toc-resolver
    ;; assume the config is right...
    (or (document-output-options-table-of-contents-resolver options)
	(make-table-of-contents-resolver 1)))
  (define section-path (sxpath '(* section)))
  (define (collect&replace-sections level document)
    (define (strip-section sections)
      (filter-map (lambda (section)
		    (let ((l (sxml:attr section 'level)))
		      (and (eqv? (string->number l) (+ (length level) 1))
			   section)))
		  sections))
    (define (replace-section document toc)
      (let loop ((c (sxml:content document)) (replaced? #f) (r '()))
	(cond ((null? c) (sxml:change-content document (reverse! r)))
	      ((pair? (car c))
	       (if (eq? 'section (sxml:name (car c)))
		   (if replaced?
		       (loop (cdr c) replaced? r)
		       (loop (cdr c) #t (cons toc r)))
		   (loop (cdr c) replaced? (cons (car c) r))))
	      (else (loop (cdr c) replaced? (cons c r))))))
    ;; Do a bit of trick to decive the resolver
    (let ((toc (toc-resolver `(dummy . ,(cdr document)))))
      (values (replace-section document toc)
	      (section-path `(*TOP* ,document)))))
    
  (define (continue level section)
    (splitter level (make-accept level section) (make-stop section)))
  (define (do-accept level document)
    (let-values (((new-doc sections) (collect&replace-sections level document)))
      (do ((i 1 (+ i 1)) (sections sections (cdr sections)))
	  ((null? sections) new-doc)
	(continue (cons i level) (car sections)))))

  (define (make-executor sec file-name pre post)
    (define document `(document (info) (content ,sec)))
    (lambda (writer)
      (call-with-output-file file-name
	(lambda (out)
	  (pre out)
	  (writer document options out)
	  (post out)))))
  (define (make-root-executor document pre post)
    (lambda (writer)
      (pre out)
      (writer document options out)
      (post out)))
  (define (make-accept level document)
    (lambda (filename pre post)
      (let ((doc (do-accept level document)))
	(list-queue-add-back! queue (make-executor doc filename pre post)))))
  (define (make-stop document)
    (lambda (filename pre post)
      (list-queue-add-back! queue (make-executor document filename pre post))))

  (define (root-accept filename pre post)
    (let* ((content (document:content document))
	   (new (do-accept '() content)))
      ;; we ignore the filename as it's already provided
      (list-queue-add-back! queue (make-root-executor
				   (document:change-content document new)
				   pre post))))
  (define (root-stop filename pre post)
    (list-queue-add-back! queue (make-root-executor document pre post)))
  (splitter '() root-accept root-stop))

(define (document-output:resolve-marker marker)
  (define resolver (*document-output-resolver*))
  (define ht (document-marker-resolver-resolvers resolver))
  (cond ((hashtable-ref ht marker #f) =>
	 (lambda (proc) (proc (document-marker-resolver-document resolver))))
	(else #f)))
(define (document-output:eval e . maybe-fallback)
  (define resolver (*document-output-resolver*))
  (define fallback (if (null? maybe-fallback) #f (car maybe-fallback)))
  (define ht (document-marker-resolver-resolvers resolver))
  (cond ((hashtable-ref ht 'eval #f) => proc)
	(else fallback)))

(define (id/tag header)
  (string-append "#" (cond ((sxml:attr header 'tag))
			   ((sxml:attr header 'id))
			   (else "this-should-not-happen"))))
(define (default-attribute-retriever for) '())

(define (make-table-of-contents-resolver depth . maybe-attribute-retriever)
  (define attribute-retriever (if (null? maybe-attribute-retriever)
				  default-attribute-retriever
				  (car maybe-attribute-retriever)))
  (define section-path (sxpath '(// section)))
  (define (check-level sections)
    (if (null? sections)
	(values sections 1)
	(let ((level (string->number (sxml:attr (car sections) 'level))))
	  (values sections level))))

  (lambda (document)
    (define (do-collect)
      (let-values (((sections highest-level)
		    (check-level (section-path `(*TOP* ,document)))))
	(define correct-depth (+ (- highest-level depth) 1))
	(define (check-depth level) (> level correct-depth))
	(let loop ((current highest-level)
		   (items (list-queue))
		   (sections sections))
	  (if (null? sections)
	      (values (list-queue-list items) sections)
	      (let* ((section (car sections))
		     (level (string->number (sxml:attr section 'level))))
		(cond ((check-depth level) (loop current items (cdr sections)))
		      ((> level current)
		       ;; dive in
		       (let-values (((children next)
				     (loop level (list-queue) sections)))
			 (let ((item `(list (@ (style "number")
					       ,@(attribute-retriever
						  'nested-toc))
					    ,@children))
			       (last (list-queue-remove-back! items)))
			   (list-queue-add-back! items (append last `(,item)))
			   (loop current items next))))
		      ((= level current)
		       ;; (section (@ ...) (header ...) elem ...)
		       ;; it's expensive to do (if-car-sxpath '(section header))
		       (let ((header (car (sxml:content section))))
			 (list-queue-add-back! items
			  `(item (@ ,@(attribute-retriever 'toc-item))
				 (link (@ (href ,(id/tag header))
					  ,@(attribute-retriever 'toc-link))
				       ,@(sxml:content header)))))
		       (loop current items (cdr sections)))
		      ((< level current)
		       (values (list-queue-list items) sections))))))))
    (let-values (((r ignore) (do-collect)))
      `(list (@ (style "number")
		,@(attribute-retriever 'toc-root))
	     ,@r))))

(define (make-eval-resolver env)
  (lambda (e)
    (let ((content (sxml:content e)))
      (eval (get-datum (open-string-input-port (car content))) env))))

(define (make-index-table-resolver . maybe-attribute-retriever)
  (define attribute-retriever (if (null? maybe-attribute-retriever)
				  default-attribute-retriever
				  (car maybe-attribute-retriever)))
  (lambda (document)
    (define (collect-definitions)
      (let loop ((defs '())
		 (current-section #f)
		 (content (document:content document)))
	(cond ((null? content) defs)
	      ((pair? content)
	       (case (sxml:name content)
		 ((define) (cons (cons content current-section) defs))
		 ((section)
		  (fold-left (lambda (defs c) (loop defs content c))
			     defs (sxml:content content)))
		 (else
		  (fold-left (lambda (defs c) (loop defs current-section c))
			     defs (sxml:content content)))))
	      (else defs))))
    (define (def-name e) (car (sxml:content e)))
    (define (name< a b) (string-ci<? (def-name (car a)) (def-name (car b))))
    (define (->index-tables defs)
      (define (->index-table defs)
	(define (->row n def)
	  (define (name->href n name)
	    (define suffix (number->string n))
	    (string-append "#" name "_" suffix))
	  (define (section->href section) (id/tag (caddr section)))
	  (define (section-name section)
	    (car (sxml:content (caddr section))))
	  (let* ((name (def-name (car def)))
		 (section (cdr def))
		 (def-href (name->href n name)))
	    ;; update define element
	    (sxml:set-attr! (car def) (list 'name def-href))
	    `(row (@)
		  (cell (@ ,@(attribute-retriever 'index-name))
			(link (@ (href ,def-href)
				 ,@(attribute-retriever 'index-name-link))
			      ,name))
		  (cell (@ ,@(attribute-retriever 'index-section))
			(link (@ (href ,(section->href section))
				 ,@(attribute-retriever 'index-section-link))
			      ,(section-name section))))))
	(define letter
	  (string (char-upcase (string-ref (def-name (caar defs)) 0))))
	(let loop ((n 0) (defs defs) (r '()))
	  (if (null? defs)
	      `(paragraph (@ ,@(attribute-retriever 'index-table-fragment))
			  (header (@ (level "5")
				     (name ,(string-append "index-" letter))
				     ,@(attribute-retriever 'index-letter))
				  ,letter)
			  (table (@ ,@(attribute-retriever 'index-table))
				 ,@(reverse! r)))
	      (let* ((def (car defs))
		     (d (car def)))
		(loop (+ n 1) (cdr defs) (cons (->row n def) r))))))
      (define (first-char=? e)
	(define c (char-upcase (string-ref (def-name (car e)) 0)))
	(lambda (d)
	  (eqv? c (char-upcase (string-ref (def-name (car d)) 0)))))
      (let loop ((defs defs) (r '()))
	(if (null? defs)
	    (reverse! r)
	    (let ((e (car defs)))
	      (let-values (((this rest) (span (first-char=? e) defs)))
		(loop rest (cons (->index-table this) r)))))))
    (let ((defs (list-sort name< (collect-definitions))))
      `(paragraph (@ ,@(attribute-retriever 'index)) ,@(->index-tables defs)))))

(define (default-unknown-node-handler writer name attr content travarse)
  (document-output-error 'write-markdown "Unknown element"
			 (cons* name attr content)))

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
	     (case name
	       ((table-of-contents index-table author)
		(cond ((document-output:resolve-marker name) => traverse)
		      (else (call-node-handler writer name attr content))))
	       ((eval)
		(let ((r (document-output:eval e e)))
		  (if (eq? r e)
		      (call-node-handler writer name attr content)
		      (traverse r))))
	       (else 
		(call-node-handler writer name attr content)))))
	  (else
	   ((document-writer-datum-handler writer) writer e noop))))
  (traverse e))


)
