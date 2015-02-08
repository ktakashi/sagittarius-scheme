;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sxml/html-parser.scm - HTML parser
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

;; HTMLPrag compatible library.
;; htmlprag is written by Neil W. Van Dyke but completely rewritten
;; to avoid LGPL
;; I'm not even sure how LGPL will be applied on Scheme script
;; however it's better not to use original code if we want to
;; keep our license BSD/MIT compatible.

;; base on html-parse.scm
;; html-parse.scm -- SSAX-like tree-folding html parser
;; Copyright (c) 2003-2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(library (text sxml html-parser)
    (export shtml-comment-symbol
	    shtml-decl-symbol
	    shtml-empty-symbol
	    shtml-end-symbol
	    shtml-entity-symbol
	    shtml-pi-symbol
	    shtml-start-symbol
	    shtml-text-symbol
	    shtml-top-symbol
	    shtml-named-char-id
	    shtml-numeric-char-id
	    make-shtml-entity
	    shtml-entity-value
	    make-html-tokenizer
	    tokenize-html
	    shtml-token-kind
	    parse-html/tokenizer
	    html->sxml-0nf
	    html->sxml-1nf
	    html->sxml-2nf
	    html->sxml
	    html->shtml
	    write-shtml-as-html
	    shtml->html

	    ;; extra parameters
	    *shtml-entity-converter*
	    )
    (import (rnrs)
	    (sagittarius)
	    (match)
	    (text parse)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))

  ;; These are SXML specification
  (define shtml-comment-symbol '*COMMENT*)
  (define shtml-decl-symbol    '*DECL*)
  (define shtml-empty-symbol   '*EMPTY*)
  (define shtml-end-symbol     '*END*)
  (define shtml-entity-symbol  '*ENTITY*)
  (define shtml-pi-symbol      '*PI*)
  (define shtml-start-symbol   '*START*)
  (define shtml-text-symbol    '*TEXT*)
  (define shtml-top-symbol     '*TOP*)

  ;; keep compatibility as original
  ;; I don't think the values need to be the same but in case
  (define shtml-named-char-id   "shtml-named-char")
  (define shtml-numeric-char-id "shtml-numeric-char")
  
  ;; make shtml character entiry
  (define (make-shtml-entity val)
    `(& ,(cond ((or (symbol? val) (integer? val)) val)
	       ((string? val) (string->symbol val))
	       (else (error 'make-shtml-entity
			    "symbol, integer or string is required"
			    val)))))
  ;; retrieve shtml entity value
  ;; example:
  ;; (define (f s) (shtml-entity-value (cadr (html->shtml s))))
  ;; (f "&nbsp;")  @result{} nbsp
  ;; (f "&#2000;") @result{} 2000
  ;; entity must be (& value) or (*ENTITY* value)
  (define (shtml-entity-value entity)
    (define (entity? o) (eq? o shtml-entity-symbol))
    (match entity
      (('& val)
       (cond ((or (symbol? val) (integer? val)) val)
	     ((string? val) (string->symbol val))
	     (else #f)))
      (((? entity?) public-id val)
       (cond ((equal? public-id shtml-named-char-id)
	      (string->symbol val))
	     ((equal? public-id shtml-numeric-char-id)
	      (string->number val))
	     (else #f)))
      (_ #f)))

  
  (define-constant +default-entities+
    '(("amp" . "&") ("quot" . "\"") ("lt" . "<")
      ("gt" . ">")  ("apos" . "'")  ("nbsp" . " ")))
  

  ;; make an html tokenizer
  ;; example:
  ;; (define input (open-input-string "<a href=\"foo\">bar</a>"))
  ;; (define next  (make-html-tokenizer input #f))
  ;; (next) @result{} (a (@@ (href "foo")))
  ;; (next) @result{} "bar"
  ;; (next) @result{} (*END* a)
  ;; (next) @result{} ()
  ;; (next) @result{} ()
  ;; to handle horrible HTML structure, we need to manage state
  ;; e.g) <ul>
  ;;       <li>aaa
  ;;       <li>bbb
  ;;      </ul>
  ;;  is a valid html...
  ;; hmmmm is unicode space whitespece on HTML?
  (define *white* (char-set-intersection char-set:ascii char-set:whitespace))
  (define (make-string-reader/ci str)
    (let* ((len (string-length str))
	   (vec (make-vector len 0)))
      (when (> len 0)
	(vector-set! vec 0 -1)
	(cond ((> len 1) (vector-set! vec 1 0))))
      (let lp ((i 2) (j 0))
	(when (< i len)
	  (let ((c (string-ref str i)))
	    (cond ((char-ci=? (string-ref str (- i 1)) (string-ref str j))
		   (vector-set! vec i (+ j 1))
		   (lp (+ i 1) (+ j 1)))
		  ((> j 0)
		   (lp i (vector-ref vec j)))
		  (else
		   (vector-set! vec i 0)
		   (lp (+ i 1) j))))))
      (lambda o
	(let ((in (if (pair? o) (car o) (current-input-port))))
	  (call-with-string-output-port
	   (lambda (out)
	     (let lp ((i 0))
	       (when (< i len)
		 (let ((c (lookahead-char in)))
		   (cond
		    ((eof-object? c)
		     (display (substring str 0 i) out))
		    ((char-ci=? c (string-ref str i))
		     (get-char in)
		     (lp (+ i 1)))
		    (else
		     (let* ((i2 (vector-ref vec i))
			    (i3 (if (= -1 i2) 0 i2)))
		       (if (> i i3) (display (substring str 0 (- i i3)) out) #f)
		       (if (= -1 i2) (put-char out (get-char in)) #f)
		       (lp i3)))))))))))))
  
  ;; should we add pre here?
  (define-constant +default-varbatim-pair+ '(script server style xmp pre))
  (define-constant +default-varbatim-eof+ '(plaintext))
  (define-record-type (<parser-context> make-context context?)
    ;; type must #f, pair or eof
    (fields (mutable verbatim-element verbatim-element set-verbatim-element!)
	    (mutable token saved-token save-token!))
    (protocol (lambda (p)
		(lambda ()
		  (p #f #f)))))

  (define (ascii->converter i) (and (<= 32 i 126) (integer->char i)))
  ;; by default we keep original behaviour
  (define *shtml-entity-converter* (make-parameter ascii->converter))

  (define (make-html-tokenizer in normalized?
			       ;; resolving TODO from original htmlprag
			       :optional (entities +default-entities+)
					 (verbatim-pair +default-varbatim-pair+)
					 (verbatim-eof +default-varbatim-eof+)
					 )
    ;; need this to track if the context is verbatim
    (define context (make-context))
    (define (char-hex-numeric? c)  (char-set-contains? char-set:hex-digit c))
    (define (char-alphanumeric? c) (or (char-alphabetic? c) (char-numeric? c)))
    (define (skip-whitespace in)
      (skip-while (lambda (c) 
		    (and (char? c) (char-set-contains? *white* c))) in))
    ;; read
    (define (read-identifier in)  (next-token-of char-alphanumeric? in))
    (define (read-integer in)     (next-token-of char-numeric? in))
    (define (read-hex-integer in) (next-token-of char-hex-numeric? in))
    (define (read-until pred o)   (next-token-of (lambda (c) (not (pred c))) o))

    (define (read-pi in)
      (let ((tag (read-identifier in)))
	(skip-whitespace in)
	(list
	 (if (equal? tag "") #f (string->symbol (string-downcase tag)))
	 (list->string
	  (reverse!
	   (let loop ((res '()))
	     (let ((c (lookahead-char in)))
	       (cond ((eof-object? c) (get-char in) res)
		     ((eqv? c #\?) (get-char in)
		      (let loop2 ((res res))
			(cond ((eof-object? (lookahead-char in)) 
			       (cons #\? res))
			      ((eqv? #\> (lookahead-char in))
			       (get-char in)
			       res)
			      ((eqv? #\? (lookahead-char in))
			       (get-char in)
			       (loop2 (cons c res)))
			      (else
			       (loop (cons c res))))))
		     (else (get-char in) (loop (cons c res)))))))))))

    (define read-cdata (make-string-reader/ci "]]>"))
    (define read-comment (make-string-reader/ci "-->"))

    (define (read-entity in entities)
      (define (conv/make-entity num)
	(let ((c ((*shtml-entity-converter*) num)))
	  (if (and c (char-set-contains? char-set:printing c))
	      (string c)
	      (make-shtml-entity num))))
      (get-char in)
      (cond ((eqv? (lookahead-char in) #\#)
	     (get-char in)
	     (cond ((char-numeric? (lookahead-char in))
		    (let* ((str (read-integer in))
			   (num (string->number str)))
		      (when (eqv? (lookahead-char in) #\;)
			(get-char in))
		      (conv/make-entity num)))
		   ((memv (lookahead-char in) '(#\x #\X))
		    (get-char in)
		    (let* ((str (read-hex-integer in))
			   (num (string->number str 16)))
		      (when (eqv? (lookahead-char in) #\;)
			(get-char in))
		      (conv/make-entity num)))
		   ;; should we check ; to return "&#;"?
		   (else "&#")))
	    ((char-alphabetic? (lookahead-char in))
	     (let ((name (read-identifier in)))
	       (when (eqv? (lookahead-char in) #\;)
		 (get-char in))
	       (cond ((assoc name entities) => cdr)
		     (else (make-shtml-entity name)))))
	    (else "&")))

    (define (tag-char? c)
      (and (char? c)
	   (or (char-alphanumeric? c)
	       (memv c '(#\- #\+ #\* #\_ #\:)))))

    (define (read-quoted in entities)
      (let ((terminator (get-char in)))
	(let lp ((res '()))
	  (let ((c (lookahead-char in)))
	    (cond ((or (eof-object? c) (eqv? terminator c))
		   (get-char in)  ; discard terminator
		   (reverse! res))
		  ((eqv? #\& c)
		   (let ((x (read-entity in entities)))
		     (lp (cons x res))))
		  (else
		   (lp (cons (read-until 
			      (lambda (c) 
				(or (eqv? #\& c) (eqv? terminator c))) 
			      in)
			     res))))))))

    ;; returns ns and local name
    (define (read-qname in)
      (let ((tag (next-token-of tag-char? in)))
	(cond ((string-contains tag ":") =>
	       (lambda (pos)
		 (values (string-copy tag 0 pos)
			 (string-copy tag (+ pos 1)))))
	      (else (values #f tag)))))

    (define (read-attrs in entities)
      (let loop ((attrs '()))
	(skip-whitespace in)
	(let ((c (lookahead-char in)))
	  (cond ((or (eof-object? c) (eqv? c #\>))
		 (get-char in) (cons #f (reverse! attrs)))
		((eqv? c #\/)
		 (get-char in)
		 (skip-whitespace in)
		 (cond ((eqv? #\> (lookahead-char in))
			(get-char in)
			(cons #t (reverse! attrs)))
		       (else
			(loop attrs))))
		((eqv? c #\")
		 (get-char in)
		 (loop attrs))
		((not (tag-char? c)) (cons #f (reverse! attrs)))
		(else
		 (let-values (((ns name) (read-qname in)))
		   (define (attr-name ns name)
		     ;; FIXME we should support proper namespace!
		     (if (and ns (or (string=? ns "xmlns")
				     (string=? ns "xml")))
			 (string->symbol (string-append ns ":" name))
			 (string->symbol (string-downcase name))))
		   (if (string=? name "")
		       (loop attrs)
		       (let ((name (attr-name ns name)))
			 (cond ((eqv? (lookahead-char in) #\=)
				(get-char in)
				(let ((value 
				       (if (memv (lookahead-char in) '(#\" #\'))
					   ;; don't convert attr's entity
					   ;; kinda nonsense
					   (let* ((term (get-char in))
						  (r (read-until
						      (lambda (c) (eqv? term c))
						      in)))
					     (get-char in)
					     r)
					   #;
					   (apply string-append
						  (read-quoted in entities))
					   (read-until
					    (lambda (c)
					      (or (char-whitespace? c)
						  (memv c '(#\' #\" #\< #\>))))
					    in))))
				  (if (string=? value "")
				      (if normalized?
					  (loop (cons (list name name) attrs))
					  (loop (cons (list name) attrs)))
				      (loop (cons (list name value) attrs)))))
			  (else
			   (loop (cons (list name) attrs))))))))))))

    (define (read-start in entities)
      ;; original htmlprag allows to have whitespace in between < and
      ;; element name
      (skip-whitespace in)
      (let-values (((ns tag) (read-qname in)))
	;; TODO handling namespace
	(if (equal? tag "")
	    (values #f #f #f)
	    (let ((v (string->symbol (string-downcase tag))))
	      (cond ((memq v verbatim-pair)
		     (set-verbatim-element! context v))
		    ((memq v verbatim-eof)
		     (set-verbatim-element! context #t)))
	      (values v ns (read-attrs in entities))))))

    (define (read-end in)
      ;; original htmlprag allows to have whitespace in between </ and
      ;; element name
      (skip-whitespace in)
      (let-values (((ns tag) (read-qname in)))
	;; discard closing attrs
	(let ((attrs (read-attrs in '())))
	  (values (if (equal? tag "")
		      #f
		      (string->symbol (string-downcase tag)))
		  #f
		  attrs))))

    (define (read-decl in entities)
      (let loop ((res '()))
	(skip-whitespace in)
	(let ((c (lookahead-char in)))
	  (cond ((eof-object? c)
		 (reverse! res))
		((eqv? c #\")
		 (loop (append! (read-quoted in entities) res)))
		((eqv? c #\>)
		 (get-char in)
		 (reverse! res))
		((eqv? c #\<)
		 (get-char in)
		 (if (eqv? (lookahead-char in) #\!) (get-char in) #f)
		 (loop (cons (read-decl in entities) res)))
		((tag-char? c)
		 (loop (cons (string->symbol (next-token-of tag-char? in))
			     res)))
		(else
		 (get-char in)
		 (loop res))))))

    (define (read-token in entities)
      (let ((c (lookahead-char in)))
	(if (eof-object? c)
	    '()
	    (case c
	      ((#\<)
	       (get-char in)
	       ;; keep compatibility of original htmlprag...
	       (case (lookahead-char in)
		 ((#\!)
		  ;; discard first one
		  (let ((nc (peek-next-char in)))
		    (cond ((eqv? #\[ nc)
			   (get-char in)
			   (let lp ((check '(#\C #\D #\A #\T #\A #\[))
				    (acc (list #\[ #\! #\<)))
			     (cond ((null? check)
				    (read-cdata in))
				   ((and-let* ((c (lookahead-char in))
					       ( (not (eof-object? c)) )
					       ( (char-ci=? c (car check)) )))
				    (lp (cdr check) (cons (get-char in) acc)))
				   (else
				    (list->string (reverse! acc))))))
			  (else
			   (skip-whitespace in)
			   (let ((nc (lookahead-char in)))
			     (if (and (eqv? #\- nc) 
				      (eqv? #\- (peek-next-char in)))
				 (begin
				   (get-char in)
				   (list shtml-comment-symbol 
					 (read-comment in)))
				 (cons shtml-decl-symbol 
				       (read-decl in entities))))))))
		 ((#\?)
		  (get-char in)
		  (cons shtml-pi-symbol (read-pi in)))
		 ((#\/)
		  (get-char in)
		  ;; keep compatibility of original htmlprag
		  (let-values (((name ns attrs) (read-end in)))
		    (let ((attrs (cdr attrs)))
		      (cond (normalized? 
			     (list shtml-end-symbol name (cons '@ attrs)))
			    ((null? attrs) (list shtml-end-symbol name))
			    (else  (list shtml-end-symbol name 
					 (cons '@ attrs)))))))
		 (else =>
		   (lambda (c)
		     ;; original htmlprag allows < ul> tag...
		     ;; original htmlprag tokenize <> as "<" and ">"
		     (if (and (char? c) (not (char=? c #\>)))
			 (let-values (((name ns attrs) 
				       (read-start in entities)))
			   (define (make-start-token name ns attrs)
			     (cond (normalized? (list name (cons '@ attrs)))
				   ((null? attrs) (list name))
				   (else (list name (cons '@ attrs)))))
			   
			   (if name
			       (if (car attrs)
				   (cons shtml-empty-symbol
					 (make-start-token name ns (cdr attrs)))
				   (make-start-token name ns (cdr attrs)))
			       "<"))
			 "<")))))
	      ;;((#\&) (read-entity in entities))
	      ;; keep the compatibility of original htmlprag...
	      ((#\>) (get-char in) ">")
	      (else 
	       ;; keep the compatibility of original htmlprag...
	       (let-values (((out extract) (open-string-output-port)))
		 (let loop ()
		   (let ((s (read-until (lambda (c) 
					  (or (eof-object? c)
					      (eqv? c #\<) 
					      (eqv? c #\>)
					      (eqv? c #\&)))
					in)))
		     (unless (string=? "" s)
		       (put-string out s)))
		   (let ((nc (lookahead-char in)))
		     (cond ((eof-object? nc) (extract))
			   ((char=? nc #\&)
			    (let ((entity (read-entity in entities)))
			      (if (string? entity)
				  (begin
				    (put-string out entity)
				    (loop))
				  (let ((r (extract)))
				    (if (string=? r "")
					entity
					(begin
					  (save-token! context entity)
					  r))))))
			   ((memv nc '(#\< #\>)) (extract))
			   (else (loop)))))))))))
    
    (define (read-verbatim-pair-token in entities)
      (let-values (((out extract) (open-string-output-port)))
	(define (finish)
	  (let ((r (extract)))
	    (if (string=? r "")
		(next-token)
		r)))
	(let loop ((c (lookahead-char in)))
	  (cond ((eof-object? c)
		 (set-verbatim-element! context #f)
		 (finish))
		((char=? c #\<)
		 (get-char in)
		 (case (lookahead-char in)
		   ((#\/)
		    (let ((nc (peek-next-char in)))
		      (cond ((eof-object? nc) (put-string out "</") (finish))
			    ((char-alphabetic? nc)
			     (let-values (((ns tag) (read-qname in)))
			       (if (and (eq? (string->symbol 
					      (string-downcase tag))
					     (verbatim-element context))
					(and-let* ((nc (lookahead-char in)))
					  (or (eof-object? nc)
					      (char=? nc #\>)
					      (char-set-contains? *white* nc))))
				   ;; ok end it
				   (begin
				     (read-until (lambda (c)
						   (or (eof-object? c)
						       (char=? c #\<)
						       (char=? c #\>)))
						 in)
				     (when (eqv? (lookahead-char in) #\>)
				       (get-char in))
				     ;; will create an end tag
				     (set-verbatim-element! context tag)
				     (finish))
				   ;; not the one
				   (begin
				     (put-string out "</")
				     (when ns
				       (put-string out ns)
				       (put-char out #\:))
				     (put-string out tag)
				     (loop (lookahead-char in))))))
			    (else (put-string out "</")
				  (loop (lookahead-char in))))))
		   (else => (lambda (c)
			      (put-char out #\<)
			      (loop c)))))
		(else 
		 (put-char out (get-char in))
		 (if (char=? c #\newline)
		     (finish)
		     (loop (lookahead-char in))))))))
    (define (read-to-eof in)
      (if (eof-object? (lookahead-char in))
	  '()
	  (let-values (((out extract) (open-string-output-port)))
	    (let loop ((c (get-char in)))
	      (cond ((eof-object? c) (extract))
		    ((char=? c #\newline) (put-char out #\newline) (extract))
		    (else (put-char out c) (loop (get-char in))))))))
    (define (next-token)
      (cond ((saved-token context) => (lambda (r) (save-token! context #f) r))
	    ((verbatim-element context)
	     => (lambda (e)
		  (cond ((symbol? e)
			 (read-verbatim-pair-token in entities))
			((string? e)
			 (set-verbatim-element! context #f)
			 ;; we don't read attr for xmp or so
			 (list shtml-end-symbol 
			       (string->symbol (string-downcase e))))
			;; plaintext tag
			(else (read-to-eof in)))))
	    (else (read-token in entities))))
    next-token)

  (define (tokenize-html in normalized? :optional (entities +default-entities+))
    (let ((next-token (make-html-tokenizer in normalized? entities)))
      (let loop ((token (next-token)) (r '()))
	(if (null? token)
	    (reverse! r)
	    (loop (next-token) (cons token r))))))

  (define (shtml-token-kind token)
    (cond ((string? token) shtml-text-symbol)
	  ((list? token)
	   (let ((s (car token)))
	     (if (memq s `(,shtml-comment-symbol
			   ,shtml-decl-symbol
			   ,shtml-empty-symbol
			   ,shtml-end-symbol
			   ,shtml-entity-symbol
			   ,shtml-pi-symbol))
		 s
		 shtml-start-symbol)))
	  (else
	   (error 'shtml-token-kind "unrecognized token kind" token))))

  (define-constant +default-empty-elements+
    '(& area base br frame hr img input isindex keygen link meta object param
      spacer wbr))
  (define-constant +default-constraints+
    '((area     . (div map))
      (body     . (div html))
      (caption  . (div table))
      (colgroup . (div table))
      (dd       . (div dl))
      (dt       . (div dl))
      (frame    . (div frameset))
      (head     . (div html))
      (isindex  . (div head))
      (li       . (div dir menu ol ul))
      (meta     . (div head))
      (noframes . (div frameset))
      (option   . (div select))
      (p        . (div body td th))
      (param    . (div applet))
      (tbody    . (div table))
      (td       . (div tr))
      (th       . (div tr))
      (thead    . (div table))
      (title    . (div head))
      (tr       . (div table tbody thead))))

  ;; i have no idea why normalized? argument is there (it's not used)
  ;; but we can't change API now...
  (define (parse-html/tokenizer tokenizer normalized? 
				:key (empty-elements +default-empty-elements+)
				     (constraints +default-constraints+))
    (define add-kinds `(,shtml-comment-symbol
			,shtml-decl-symbol
			,shtml-entity-symbol
			,shtml-pi-symbol
			,shtml-text-symbol))
    ;; tokenizer returns SAX like element unit
    ;; so we need to resolve a tag properly
    ;; e.g) given input
    ;;   (html)
    ;;   (body)
    ;;   "body"
    ;;   (*END* body)
    ;;   (*END* html)
    ;; output must be
    ;; (html (body "body"))
    ;; easiest way would be storing all tags one by one and
    ;; resolve it one by one
    ;; e.g.) store model
    ;;  ((body ...)
    ;;   (html (body ...))
    ;;   (#f   (html (body ...))))
    ;; the first element is the current element
    ;; so when "body" is there, we simply need to add to current element.
    ;; the returning value is the last element
    ;;
    ;; To handle broken html, we need to keep all elements
    ;; e.g.) <b><i>hoo</b></i> <!-- very common error -->
    (define (find-probable-currnet all-store store name)
      (cond ((assq name all-store))
	    ;; hmmmm
	    (else (car store))))
    (define (add-current all-store store token)
      (let ((store (if (pair? token)
		       (find-probable-currnet all-store store (car token))
		       (car store))))
	(set-cdr! store (append! (cdr store) (list token)))))
    (define (merge-upto parents store)
      (let* ((top (car store)) ;; (p) as above example
	     (name (car top)))
	;; we need to keep the second last one in case there is
	;; something outside of <body> (broken HTML)
	(cond ((and (not (null? (cdr store))) (null? (cddr store))) store)
	      ((not name) store) ;; should not reach this
	      ((memq name parents) store)
	      (else (merge-upto parents (cdr store))))))
    (define (finish-to name otokens)
      (let loop ((tokens otokens))
	(let* ((token (car tokens))
	       (token-name (car token)))
	  (cond ((not token-name) otokens) ;; well not found
		((eq? name token-name) (cdr tokens))
		(else (loop (cdr tokens)))))))
    (define tokens (list (list #f)))
    (let loop ((token (tokenizer)) (tokens tokens) (all tokens))
      (if (null? token)
	  (cdar (last-pair tokens))
	  (let ((kind (shtml-token-kind token)))
	    (cond ((memq kind add-kinds) 
		   (add-current all tokens token)
		   (loop (tokenizer) tokens all))
		  ((eq? kind shtml-start-symbol)
		   ;; because of stupid HTML rule we need to merge
		   ;; some of elements. 
		   ;; e.g) 
		   ;;  <body><p><p><div></body> needs to be
		   ;;  (body (p) (p div)) instead of (body (p (p (div))))
		   (let* ((name (car token))
			  (slot (assq name constraints))
			  (tokens (if slot
				      (merge-upto (cdr slot) tokens)
				      tokens)))
		     ;; we pass '() for all so that
		     ;; the token is added to the current element.
		     ;; parant constraints resolves the
		     ;; <body><p><p><div></body> thing.
		     (add-current '() tokens token)
		     (if (memq name empty-elements)
			 (loop (tokenizer) tokens tokens)
			 (let ((new-tokens (cons token tokens)))
			   (loop (tokenizer) new-tokens new-tokens)))))
		  ((eq? kind shtml-empty-symbol)
		   (add-current all tokens (cdr token))
		   (loop (tokenizer) tokens all))
		  ((eq? kind shtml-end-symbol)
		   (let ((name (cadr token)))
		     (if name
			 (loop (tokenizer) (finish-to name tokens) all)
			 ;; well drop the most recent one
			 (loop (tokenizer) 
			       (if (caar tokens)
				   (cdr tokens)
				   tokens)
			       all))))
		  (else
		   (error 'parse-html/tokenizer
			  "unknown token kind" kind token)))))))


  (define-syntax make-html->sxml
    (syntax-rules ()
      ((_ normalized?)
       (lambda (input)
	 (define (parse)
	   (parse-html/tokenizer
	    (make-html-tokenizer
	     (cond ((input-port? input) input)
		   ((string? input) (open-string-input-port input))
		   (else (error 'make-html->sxml
				"input port or string required" input)))
	     normalized?)
	    normalized?))
	 (cons shtml-top-symbol (parse))))))

  (define html->sxml-0nf (make-html->sxml #f))
  (define html->sxml-1nf html->sxml-0nf)
  (define html->sxml-2nf (make-html->sxml #t))

  (define html->sxml html->sxml-0nf)
  (define html->shtml html->sxml-0nf)


  ;; write
  (define (default-filter o attr?)
    (error 'write-shtml-as-html
	   (if attr?
	       "unhandled foreign object in shtml attribute value"
	       "unhandled foreign object in shtml")
	   o))
  ;; adding optional argument is a bit too inconvenient so it's a paramter
  (define *writer-empty-elements* (make-parameter +default-empty-elements+))
  (define (write-shtml-as-html shtml
			       :optional (out (current-output-port))
					 (error-filer default-filter))
    (define (write-text text out)
      (string-for-each (lambda (c)
			 (case c
			   ((#\&) (display "&amp;" out))
			   ((#\<) (display "&lt;" out))
			   ((#\>) (display "&gt;" out))
			   (else  (display c out))))
		       text))
    (define (write-dqoute-amplified text out)
      (string-for-each (lambda (c)
			 (case c
			   ((#\") (display "&quot;" out))
			   (else  (display c out))))
		       text))
    (define (write-attribute-value v out)
      ;; keep compatibility ... SUCKS!!!
      (cond ((null? v))
	    ((eqv? v #t))
	    ((string? v)
	     (put-char out #\=)
	     ;; check if the value contains #\" or not
	     (let ((pos (string-contains v "'")))
	       (cond (pos
		      ;; ok we use double quote quote
		      (put-char out #\")
		      (write-dqoute-amplified v out)
		      (put-char out #\"))
		     ((string-contains v "\"") 
		      (put-char out #\')
		      (put-string out v)
		      (put-char out #\'))
		     (else
		      ;; default
		      (put-char out #\")
		      (put-string out v)
		      (put-char out #\")))))
	    (else (error 'write-shtml-as-html
			 "invalid shtml attribute value" v))))
    (define (write-attribute attr out)
      (unless (list? attr)
	(error 'write-shtml-as-html "invalid shtml attribute" attr))
      (unless (null? attr)
	(let ((name (car attr)))
	  (unless (symbol? name)
	    (error 'write-shtml-as-html "invalid name in shtml attribute"
		   name attr))
	  (format out " ~a" name)
	  (unless (null? (cdr attr))
	    (write-attribute-value (cadr attr) out)))))

    (define (write-comment comment out)
      (unless (null? (cddr comment))
	(error 'write-shtml-as-html "invalid shtml comment" comment))
      (put-string out "<!-- ")
      (let ((text (cadr comment)))
	(if (string? text)
	    (put-string out text)
	    (error 'write-shtml-as-html "invalid shtml comment text" text)))
      (put-string out " -->"))

    (define (write-pi pi out)
      (put-string out "<?")
      (put-string out (symbol->string (cadr pi)))
      (put-char out #\space)
      (put-string out (caddr pi))
      (put-string out "?>"))

    (define (write-decl decl out)
      (put-string out "<!")
      (put-string out (symbol->string (cadr decl)))
      (for-each (lambda (thing)
		  (cond ((symbol? thing)
			 (put-char out #\space)
			 (put-string out (symbol->string thing)))
			((string? thing)
			 (put-string out " \"")
			 (write-dqoute-amplified thing out)
			 (put-char out #\"))
			(else (error 'write-shtml-as-html
				     "invalid shtml decl" thing decl))))
		(cddr decl))
      (put-char out #\>))

    (define (write-top top out) 
      (for-each (lambda (thing) (write-thing thing out)) top))
    (define (write-empty empty out))
    (define (write-entity entity out)
      (or (and-let* ((v (shtml-entity-value entity)))
	    (put-char out #\&)
	    (when (integer? v) (put-char out #\#))
	    (display v out)
	    (put-char out #\;))
	  (error 'write-shtml-as-html "invalid shtml entity reference"
		 entity)))
    (define (write-start start out)
      (define (write-content content)
	(if (memq (car start) (*writer-empty-elements*))
	    (put-string out " />")
	    (begin
	      (put-char out #\>)
	      (for-each (lambda (thing) (write-thing thing out)) content)
	      (put-string out "</")
	      (put-string out (symbol->string (car start)))
	      (put-char out #\>))))
      (put-char out #\<)
      (put-string out (symbol->string (car start)))
      (match (cdr start)
	((('@ attrs ...) rest ...)
	 (for-each (lambda (attr) (write-attribute attr out)) attrs)
	 (write-content rest))
	((rest ...)
	 (write-content rest))))
    (define (invalid thing out)
      (error 'write-shtml-as-html "invalid shtml symbol" (car thing) thing))
    (define *handler*
      `((,shtml-comment-symbol . ,write-comment)
	(,shtml-pi-symbol      . ,write-pi)
	(,shtml-decl-symbol    . ,write-decl)
	(,shtml-top-symbol     . ,write-top)
	(,shtml-empty-symbol   . ,write-empty)
	(@                     . ,invalid)
	(&                     . ,write-entity)
	(,shtml-entity-symbol  . ,write-entity)
	(,shtml-end-symbol     . ,invalid)
	(,shtml-start-symbol   . ,invalid)
	(,shtml-text-symbol    . ,invalid)))

    (define (write-list-thing lst out)
      (if (symbol? (car lst))
	  (cond ((and  (assq (car lst) *handler*))
		 => (lambda (handler) ((cdr handler) lst out)))
		(else (write-start lst out)))
	  (for-each (lambda (thing) (write-thing thing out)) lst)))
    (define (write-thing thing out)
      (cond ((string? thing) (write-text thing out))
	    ((list? thing) (unless (null? thing) (write-list-thing thing out)))
	    (else (write-thing (error-filer thing #f) out))))
    (write-thing shtml out))
  (define (shtml->html shtml)
    (call-with-string-output-port
     (lambda (out) (write-shtml-as-html shtml out))))
)
