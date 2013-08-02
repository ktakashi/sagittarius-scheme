;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/html - scribble sxml to html
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


;; FIXME getting uglier refactor it!
(library (sagittarius document html)
    (export scribble-sxml->html
	    scribble-sxml->shtml
	    scribble-file->html
	    scribble-file->shtml)
    (import (rnrs)
	    (pp)
	    (sagittarius)
	    (sagittarius control)
	    (util list)
	    (util file)
	    (srfi :1 lists)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (text sxml sxpath)
	    (text sxml serializer)
	    (text sxml tools)
	    (scribble convert)
	    (scribble parser)
	    (scribble plugin))

  ;; parameters
  (define names-for-index 	 (make-parameter '()))
  (define current-section 	 (make-parameter #f))
  (define current-section-number (make-parameter '(0)))
  (define table-of-contents 	 (make-parameter #f))
  (define author            	 (make-parameter #f))

  ;; define scribble plugin here to reduce the rest of handling process
  (define-scribble-plugin (dl-list . args)
    (let ((dt&dls (filter values 
			  (map (lambda (sexp)
				 (if (pair? sexp)
				     (scribble->sxml-inner sexp)
				     #f)) args))))
      `(div (@ (class "dl-list-wrapper"))
	    (dl (@ (class "dl-list"))
		,@(apply append dt&dls)))))

  (define-scribble-plugin (dl-item title . body)
    `((dt (@ (class "dl-item-title"))
	  ,(scribble->sxml-inner title))
      (dd (@ (class "dl-item-desc"))
	  ,@(map scribble->sxml-inner body))))

  (define-scribble-plugin (dl-itemx num . args)
    (let-values (((dds body)
		  (do ((i 0 (+ i 1))
		       (src args (cdr src))
		       (r '() (cons `(dt (@ (class "dl-item-title"))
					 ,(scribble->sxml-inner (car src))) r)))
		      ((= i num) (values (reverse! r) src)))))
      `(,@dds
	(dd (@ (class "dl-item-desc"))
	    ,@(map scribble->sxml-inner body)))))

  (define-scribble-plugin (itemlist . items)
    (let ((lis (map scribble->sxml-inner items)))
      `(ul (@ (class "itemlist"))
	   ,@lis)))
  (define-scribble-plugin (item . body)
    `(li (@ (class "itemlist-item"))
	 ,@(map scribble->sxml-inner body)))

  (define-scribble-plugin (define cate . body)
    `(div (@ (class "define"))
	  (span (@ (class "define-category")) ,(format "~a" cate))
	  ,@(map scribble->sxml-inner body)))

  (define-scribble-plugin (args . body)
    `(span (@ (class "args"))
	   ,@(map scribble->sxml-inner body)))

  (define-scribble-plugin (name . body)
    ;; assume, or else VM raise error with ambiguous message ;)
    (define (body->name body)
      (string-concatenate 
       (map (^i (cond ((symbol? i) (symbol->string i))
		      ((string? i) i)
		      ((pair? i)   (car (sxml:content i)))
		      (else (assertion-violation 'name
						 "bogus data"
						 i))))
	    body)))
    (let* ((name (body->name body))
	   (tag (symbol->string (gensym name))))
      (names-for-index (acons name (cons tag (current-section))
			      (names-for-index)))
      `(a (@ (name ,tag))
	  (span (@ (class "name") (name ,name))
		,@(map scribble->sxml-inner body)))))
  
  ;; over write
  (define-scribble-plugin (secref t . args)
    `(a (@ (href ,(string-append "#" t))) ,@(map scribble->sxml-inner args)))

  (define-scribble-plugin (codeblock . args)
    (let* ((e (if (eq? (car args) '=>) (format "~a" (cadr args)) #f))
	   (code `(pre (@ (class "codeblock"))
		       ,@(map scribble->sxml-inner (if e (cddr args) args)))))
      (if e
	  (append! code `((span (@ (class "codeblock-arrow")) "=>")
			  (span (@ (class "codeblock-result"))
				,(scribble->sxml-inner e))))
	  code)))

  (define-scribble-plugin (snipet . args)
    (let* ((e (if (eq? (car args) '=>) (format "~a" (cadr args)) #f))
	   (code `(pre (@ (class "snipet"))
		       (span (@ (class "snipet-code"))
			     ,@(map scribble->sxml-inner
				    (if e (cddr args) args))))))
      (if e
	  (append! code `((span (@ (class "snipet-arrow")) "=>")
			  (span (@ (class "snipet-result"))
				,(scribble->sxml-inner e))))
	  code)))

  (define-scribble-plugin (atmark) "@")
  (define-scribble-plugin (dots) "...")

  (define (list->section-number section)
    (call-with-string-output-port
     (lambda (p)
       (for-each-with-index
	(lambda (index number)
	  (if (zero? index)
	      (format p "~a" number)
	      (format p ".~a" number))) section))))

  (define (section-handler tag style body)    
    (define (numbering attr)
      (let-syntax ((set-section!
		    (syntax-rules ()
		      ((_ count)
		       (begin
			 (unless (= (length (current-section-number)) count)
			   (current-section-number 
			    (append (current-section-number) '(0))))
			 (current-section-number (take (current-section-number)
						       count))
			 (set-car! (list-tail (current-section-number)
					      (- count 1))
				   (+ (list-ref (current-section-number)
						(- count 1)) 1)))))))
	(cond ((and attr (assq 'appendix attr)) => cadr)
	      (else
	       (case tag
		 ((h2) (set-section! 1))
		 ((h3) (set-section! 2))
		 ((h4) (set-section! 3))
		 ((h5) (set-section! 4)))
	       (current-section-number)))))

    (let-values (((attr contents) (scribble-parse-attribute body)))
      (let* ((name (cond ((assq 'tag attr) => cadr)
			 (else (symbol->string (gensym)))))
	     (num  (numbering attr))
	     (formatted (if (pair? num)
			    (list->section-number num)
			    num))
	     (content (map scribble->sxml-inner contents)))
	(current-section (cons `(,formatted " " ,@content)
			       name))
	(table-of-contents (cons (cons* (if (pair? num) num (list num))
					content name) (table-of-contents)))
	`(,tag (@ (class ,style))
	       (a (@ (class "section.anchor")
		     (name ,name))
		  (span (@ (class "section-number"))
			,(format "~a" formatted))
		  ,@content)))))

  (define-scribble-plugin (section . body)
    (section-handler 'h2 "section" body))
  (define-scribble-plugin (subsection . body)
    (section-handler 'h3 "subsection" body))
  (define-scribble-plugin (subsubsection . body)
    (section-handler 'h4 "subsubsection" body))
  (define-scribble-plugin (sub*section . body)
    (section-handler 'h5 "sub-section" body))

  (define-scribble-plugin (desc . body)
    `(div (@ (class "desc"))
	  ,@(map scribble->sxml-inner body)))

  (define-scribble-plugin (hyperlink . elements)
    (let-values (((attr contents) (scribble-parse-attribute elements)))
      `(a (@ ,@attr)
	  ,@(map scribble->sxml-inner contents))))

  (define-scribble-plugin (author . names)
    (author names)
    "")

  (define (scribble-file->shtml file . opt)
    (parameterize ((current-section #f)
		   (names-for-index '())
		   (current-section-number '(0))
		   (table-of-contents '())
		   (author (list "anonymous")))
      (call-with-input-file file
	(lambda (p)
	  (apply scribble-sxml->shtml (scribble->sxml p) opt)))))

  (define (scribble-file->html file . opt)
    (parameterize ((current-section #f)
		   (names-for-index '())
		   (current-section-number '(0))
		   (table-of-contents '())
		   (author (list "anonymous")))
      (call-with-input-file file
	(lambda (p)
	  (apply scribble-sxml->html (scribble->sxml p) opt)))))
  
  (define (content-list-handler element)
    (define (process contents)
      (define (li-gen content class)
	(let* ((tag     (cddr content))
	       (section (list->section-number (car content))))
	  `((li (@ (class ,class))
		(a (@ (href ,(format "#~a" tag)))
		   (span (@ (class "section-number")) ,section)
		   ,@(cadr content))))))
      (define (ul-gen class)
	`(ul (@ (class ,class))))
      (define (rec contents generator top)
	(let loop ((contents contents)
		   (r top))
	  (if (null? contents)
	      r
	      (let ((content (car contents)))
		(let-values (((class depth) (generator content)))
		  (let loop ((i 0) (r r))
		    (if (= i depth)
			(cond 
			 ((and (zero? i) ;; top
			       (eq? (car r) 'ul))
			  (append! r (li-gen content class)))
			 (else
			  (let ((tail (car (list-tail r (- (length r) 1)))))
			    (cond ((eq? (car tail) 'ul)
				   (append! tail (li-gen content class))
				   r)
				  (else
				   (let ((ul (ul-gen class)))
				     (append! ul (li-gen content class))
				     (if (eq? (car tail) 'li)
					 (append! tail (list ul))
					 (append! r (list ul)))
				     r))))))
			(loop (+ i 1)
			      (let ((rr (car (list-tail r (- (length r) 1)))))
				(if (eq? (car rr) 'li)
				    rr
				    (car (list-tail rr (- (length rr) 1))))))))
		  (loop (cdr contents) r))))))

      (define (generate-classname element)
	(if (null? (cdar element))
	    "top-content"
	    "child-content"))

      (define (section-generator element)
	(values (generate-classname element)
		(- (length (car element)) 1)))
      ;; assume first one is section
      (rec contents section-generator (ul-gen "section")))

    (let* ((contents (reverse (table-of-contents)))
	   (attr (sxml:attr-list-node element))
	   (id   (cond ((and attr (assq 'id attr)) => cadr)
		       (else (symbol->string (gensym))))))
      `(div (@ (id ,id)
	       (class "table-of-contents"))
	    ,(process contents))
      )
    )

  (define (include-handler element)
    ;; TODO create option
    `(div (@ (class "included"))
	  ,@(cdr element)))

  ;; image
  ;;(div (@ (id "id"))
  ;;     (div (@ (class "index-letters"))
  ;;	    (a (@ (class "index-letter")) "A") 
  ;;	    ;; continue
  ;;	    )
  ;;     (div (@ (class "index-table-fragment"))
  ;;	    (span (@ (class "index-title-letter")) "A")
  ;;	    (table (@ (class "index-table"))
  ;;		   (tr (td (@ (class "index-name")) "acons")
  ;;		       (td (@ (class "index-section")) "(sagittarius)"))
  ;;		   ;; continue
  ;;		   ))
  ;;     ;; continue
  ;;     )
  (define (index-table-handler element)
    ;; generates index table
    (define (generate-index-letters lst)
      (let loop ((lst lst)
		 (current-letter #f)
		 (r '()))
	(cond ((null? lst) 
	       `(div (@ (class "index-letters"))
		     ,@(reverse! r)))
	      ((and current-letter
		    (char-ci=? current-letter (string-ref (car lst) 0)))
	       (loop (cdr lst) current-letter r))
	      (else
	       (let ((letter (substring (car lst) 0 1)))
		 (loop (cdr lst) (string-ref (car lst) 0)
		       (cons `(a (@ (class "index-letter")
				    (href ,(string-append "#index-" letter)))
				 ,(string-upcase letter))
			   r)))))))
    (define (generate-index-table lst)
      (define (gen-table lst)
	`((tr (td (@ (class "index-name"))
		  (a (@ (href ,(string-append "#" (cadr lst))))
		     ,(car lst)))
	      (td (@ (class "index-section"))
		  (a (@ (href ,(format "#~a" (cdddr lst))))
		     ,@(caddr lst))))))
      (define (gen-div letter table)
	`(div (@ (class "index-table-fragment"))
	      (span (@ (class "index-title-letter"))
		    (a (@ (name ,(string-append "index-" letter)))
		       ,(string-upcase letter)))
	      ,table))
      (let loop ((lst lst)
		 (current-letter #f)
		 (r '())
		 (table '()))
	(cond ((null? lst)
	       (reverse! (cons (gen-div (string current-letter) table) r)))
	      ((not current-letter)
	       (loop (cdr lst) (string-ref (caar lst) 0)
		     r `(table (@ (class "index-table"))
			       ,@(gen-table (car lst)))))
	      ((char-ci=? current-letter (string-ref (caar lst) 0))
	       (append! table (gen-table (car lst)))
	       (loop (cdr lst) current-letter r table))
	      (else
	       (let ((letter (string current-letter)))
		 (loop lst #f
		       (cons (gen-div letter table) r)
		     '()))))))
	       
    (let* ((lst (list-sort (lambda (pair1 pair2)
			     (let ((name1 (car pair1))
				   (name2 (car pair2)))
			       (string-ci<=? name1 name2)))
			   (names-for-index)))
	   (attr (sxml:attr-list-node element))
	   (id   (cond ((and attr
			     (assq 'id attr)) => cadr)
		       ;; default
		       (else "index-table"))))
      `(div (@ (id ,id))
	    ,(generate-index-letters (map car lst))
	    ,@(generate-index-table lst))))

  ;; alist of tag handler need-reverse
  ;; TODO add handler things
  (define *handlers*
    `((table-of-contents  ,content-list-handler . #f)
      (included           ,cdr                  . #t)
      (index-table        ,index-table-handler  . #f)
      (title              h1                    . #f)))
  ;; 
  ;; from here we just need to handle two elements,
  ;; table-of-contents and include. include might be used for creating separated
  ;; document
  (define (dispatch elements :optional (had-linefeed? #f))
    (let loop ((elements elements)
	       (r '())
	       (had-linefeed? had-linefeed?))
      ;;(unless (null? elements) (pp (car elements)))
      (cond ((null? elements) (reverse! r))
	    ((and (pair? (car elements))
		  (assq (caar elements) *handlers*))
	     => (lambda (slot)
		  (if (procedure? (cadr slot))
		      (let ((dr (dispatch ((cadr slot) (car elements)))))
			(if (pair? (car dr))
			    (if (cddr slot)
				(loop (cdr elements) `(,@(reverse dr) . ,r) #f)
				(loop (cdr elements) `(,@dr . ,r) #f))
			    (loop (cdr elements) (cons dr r) #f)))
		      (loop (cdr elements)
			    (cons `(,(cadr slot) ,@(cdar elements)) r)
			    #f))))
	    ((pair? (car elements))
	     (loop (cdr elements)
		   (cons (dispatch (car elements) had-linefeed?) r)
		   #f))
	    ((and (equal? "\n" (car elements)) had-linefeed?)
	     (loop (cdr elements) (cons '(p) r) #f))
	    ((equal? (car elements) "\n")
	     (loop (cdr elements) (cons "\n" r) #t))
	    (else
	     (loop (cdr elements) (cons (car elements) r) #f)))))

  ;; let's do it only the section tag
  (define (fixup doc)
    (define (fixup-persecion doc section parents)
      (define (sibling? e) 
	(and (pair? e) (eq? (sxml:element-name e) section)))
      (define (parent? e)
	(and (pair? e) (memq (sxml:element-name e) parents)))

      (define (lookup-next-section doc)
	(let loop ((doc doc) (r '()))
	  (cond ((and (pair? doc)
		      (or (sibling? (car doc)) (parent? (car doc))))
		 (values doc (reverse! r)))
		((pair? doc)
		 (loop (cdr doc) (cons (car doc) r)))
		((null? doc) (values '() (reverse! r)))
		(else doc)))) ;; should this happen?
      (let loop ((doc doc))
	(cond ((and (pair? doc) (sibling? (car doc)))
	       (receive (next section) (lookup-next-section (cdr doc))
		 (let ((class (sxml:attr-u (car doc) 'class)))
		   (cons `(section (@ (class ,class))
				   ,@(cons (car doc) section))
			 (loop next)))))
	      ((pair? doc) (cons (car doc) (loop (cdr doc))))
	      (else doc))))
    (fold-left (lambda (doc sec parent) (fixup-persecion doc sec parent))
	       doc '(h5 h4 h3 h2) '((h4 h3 h2) (h3 h2) (h2) ())))

  (define (generate-html-metas title style javascript)
    `(html
      (head (meta (@ (http-eqiv "Content-Type")
		     (content   "text/html; charset=utf-8")))
	    ,@(filter values
		      (list (and style
				 `(link (@ (rel "stylesheet")
					   (type "text/css")
					   (href ,style))))
			    (and javascript
				 `(script (@ (type "text/javascript")
					     (src ,javascript)) " "))))
	    (title ,(or title
			"Auto generated by html-generator")))))

  (define (scribble-sxml->shtml sexp :key (style #f)
					  (javascript #f))
    (let* ((doc  (cdar ((sxpath '(scribble)) sexp)))
	   (title ((sxpath '(scribble title)) sexp))
	   (html-metas (generate-html-metas (and title (cadar title))
					    style javascript)))
      (append html-metas
	      `((body 
		 (div (@ (id "sagittarius-doc-wrapper"))
		      ,@(fixup (dispatch doc))
		      (hr)
		      (div (@ (id "document-footer"))
			   (div (@ (id "footer-message"))
				"This document was generated by"
				(i ,@(author))
				" with Sagittarius gendoc. ")
			   (div (@ (id "footer-date"))
				"Generated date: " 
				(i ,(date->string
				     (current-date) "~4"))))))))))

  (define (scribble-sxml->html sexp :key (output (current-output-port))
			                 (style #f)
					 (javascript #f)
					 (separate-file #f))
    (when (and separate-file (not (string? output)))
      (assertion-violation 'scribble-file->html
			   ":separate-file is set but output is not a path"))
    (let ((document (scribble-sxml->shtml sexp :style style
					  :javascript javascript)))
      ;; for some reason, SXML does not accept doctype so write it
      ;; manually
      (define (write-doctype output)
	(display "<!DOCTYPE html>" output)
	(newline output)
	(srl:sxml->html document output))

      (cond ((port? output) (write-doctype output))
	    (else
	     (when (file-exists? output) (delete-file output))
	     (call-with-output-file output write-doctype)))))

)
