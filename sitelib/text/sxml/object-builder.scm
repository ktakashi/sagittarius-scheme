;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sxml/object-builder.scm - SXML to Scheme object builder
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (text sxml object-builder)
    (export sxml->object ? * + / ?? <!> *namespace*
	    ;; object->sxml

	    ;; XML object
	    xml-object xml-object? make-xml-object
	    xml-object-name xml-object-attributes xml-object-contents
	    sxml->xml-object

	    sxml-object-builder

	    object-builder object-builder?
	    make-simple-object-builder simple-object-builder?
	    make-set-object-builder set-object-builder?
	    make-choice-object-builder choice-object-builder?
	    make-recursive-object-builder recursive-object-builder?
	    )
    (import (rnrs)
	    (text sxml tools)
	    (srfi :139))

  ;; TODO maybe we can make this more generic?
  (define-record-type object-builder
    (fields build-object))
  
  (define-record-type simple-object-builder
    (fields tag?
	    >object
	    (mutable next-builder simple-object-builder-next-builder
		     %simple-object-builder-next-builder-set!))
    (parent object-builder)
    (protocol (lambda (n)
		(lambda (tag? ->object next-builder)
		  ((n build-object) tag? ->object next-builder)))))
  (define-record-type set-object-builder
    (fields object-builders)
    (parent object-builder)
    (protocol (lambda (n)
		;; ((builder pred min max) ...)
		(lambda (object-builders)
		  ((n build-set-object) object-builders)))))

    (define-record-type choice-object-builder
    (fields object-builders)
    (parent object-builder)
    (protocol (lambda (n)
		;; ((builder pred) ...)
		(lambda (object-builders)
		  ((n build-choice-object) object-builders)))))
  
  (define-record-type recursive-object-builder
    (parent simple-object-builder)
    (protocol (lambda (n)
		(lambda (sob)
		  (define tag? (simple-object-builder-tag? sob))
		  (define ->object (simple-object-builder->object sob))
		  (let ((r ((n tag? ->object #f))))
		    (%simple-object-builder-next-builder-set! r r)
		    r)))))

  (define (default-unknown-tag-handler builder sxml)
    (assertion-violation 'sxml->object "unknown tag" sxml builder))

  ;; Build simple object
  (define (build-object sxml builder handler)
    (define check-tag? (simple-object-builder-tag? builder))
    (define ->object (simple-object-builder->object builder))
    (define next-builder (simple-object-builder-next-builder builder))
    (cond ((sxml:element? sxml)
	   (let ((content (sxml:content sxml))
		 (attrs   (sxml:attr-list sxml))
		 (name    (sxml:name sxml)))
	     (if (check-tag? name)
		 (->object name attrs 
			   (sxml->object content next-builder handler))
		 (handler builder sxml))))
	  ((string? sxml) sxml)
	  (else (map (lambda (c) (sxml->object c builder handler))
		     (sxml:content sxml)))))

  ;; Build set object
  (define (build-set-object sxml builder handler)
    (define builders (set-object-builder-object-builders builder))
    (define len (length builders))
    (define objects (make-vector len #f))
    (define (find-builder builders tag)
      (let loop ((builders builders) (index 0))
	(if (null? builders)
	    (values #f #f)
	    (let ((pred (cadar builders)))
	      (if (pred tag)
		  (values (car builders) index)
		  (loop (cdr builders) (+ index 1)))))))

    (define (set-in-order who o index min&max)
      (define min (car min&max))
      (define max (cadr min&max))
      (cond ((vector-ref objects index) =>
	     (lambda (box)
	       (let ((v* (vector-ref box 0)))
		 (if (or (not max) (<= (length v*) max))
		     (vector-set! box 0 (cons o v*))
		     (assertion-violation who "Too many elements" o sxml)))))
	    (else
	     (vector-set! objects index `#(,(if (eqv? max 1) o (list o)))))))

    (define (retrieve-objects)
      (define (check-required conf)
	(unless (zero? (caddr conf))
	  (assertion-violation 'sxml->object "Required element is missing"
			       (cadr conf)))
	#f)
      (define (check-reverse v conf)
	(if (eqv? (cadddr conf) 1)
	    v
	    (reverse v)))
      (map (lambda (v b)
	     (if v
		 (check-reverse (vector-ref v 0) b)
		 (check-required b)))
	   (vector->list objects) builders))
    (let loop ((contents (sxml:content sxml)))
      (if (null? contents)
	  (retrieve-objects)
	  (let ((tag (if (sxml:element? (car contents))
			 (sxml:name (car contents))
			 (car contents))))
	    (let-values (((conf index) (find-builder builders tag)))
	      (if conf
		  (let ((object (sxml->object (car contents) 
					      (car conf) handler)))
		    (set-in-order tag object index (cddr conf))
		    (loop (cdr contents)))
		  (begin
		    ;; discards the result if returned
		    (handler builder (car contents))
		    (loop (cdr contents)))))))))

  ;; Build choice object
  (define (build-choice-object sxml builder handler)
    ;; the SXML must only have one content
    (unless (sxml:element? sxml)
      (assertion-violation 'sxml->object
			   "SXML element required for choice" sxml))
    ;; ((builder pred) ...)
    (let loop ((builders (choice-object-builder-object-builders builder)))
      (if (null? builder)
	  (handler builder sxml)
	  (let ((builder (car builders))
		(tag (sxml:name sxml)))
	    (if ((cadr builder) tag)
		(sxml->object sxml (car builder) handler)
		(loop (cdr builders)))))))
  
  ;; API
  (define (sxml->object sxml builder . opt)
    (define handler (if (null? opt) default-unknown-tag-handler (car opt)))
    (define (rec sxml builder)
      ((object-builder-build-object builder) sxml builder handler))
    (cond ((not builder) sxml)
	  ((and (pair? sxml) (eq? (car sxml) '*TOP*))
	   (rec (car (sxml:content sxml)) builder))
	  (else (rec sxml builder))))
  
  (define-syntax ? (syntax-rules ()))
  (define-syntax ?? (syntax-rules ()))
  (define-syntax <!> (syntax-rules ()))
  (define-syntax-parameter *namespace* (syntax-rules () ((_) '())))
  
  (define-syntax sxml-object-builder-predicate
    (syntax-rules (??)
      ((_ (?? pred)) pred)
      ((_ tag)
       (let ((ns (*namespace*)))
	 (if (or (null? ns) (not (sxml:name->ns-id 'tag)))
	     (let ((tag (lambda (t) (eq? t 'tag)))) tag)
	     (let ((ns-id (cond ((assq (string->symbol (sxml:name->ns-id 'tag))
				       ns) => cadr)
				;; tag has namespace but not defined
				(else #f)))
		   (lcname (sxml:ncname '(tag))))
	       (let ((tag (lambda (t)
			    (or (and ns-id
				     (equal? (sxml:name->ns-id t) ns-id)
				     (string=? lcname (sxml:ncname (list t))))
				(eq? t 'tag)))))
		 tag)))))))
  (define-syntax sxml-object-builder-helper
    (syntax-rules (??)
      ((_ tag ctr next)
       (make-simple-object-builder 
	(sxml-object-builder-predicate tag) ctr next))))

  (define-syntax sxml-set-object-builder
    (syntax-rules (? * + /)
      ((_ "parse" (((count tag) builder) ...) ())
       (make-set-object-builder 
	(list (cons* builder (sxml-object-builder-predicate tag) 'count) ...)))
      ;; ?
      ((_ "parse" (tcn ...) ((? tag ctr nb ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((0 1) tag)
		  (sxml-object-builder-helper
		   tag ctr (sxml-object-builder nb ...))))
	(next ...)))
      ;; *
      ((_ "parse" (tcn ...) ((* tag ctr nb ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((0 #f) tag)
		  (sxml-object-builder-helper
		   tag ctr (sxml-object-builder nb ...))))
	(next ...)))
      ;; +
      ((_ "parse" (tcn ...) ((+ tag ctr nb ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((1 #f) tag)
		  (sxml-object-builder-helper
		   tag ctr (sxml-object-builder nb ...))))
	(next ...)))
      ;; /
      ((_ "parse" (tcn ...) ((/ (tag ctr nb ...) ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((1 1) (lambda (t)
			   (or ((sxml-object-builder-predicate tag) t)...)))
		  (sxml-object-builder (/ (tag ctr nb ...) ...))))
	(next ...)))
      
      ((_ "parse" (tcn ...) ((tag ctr nb ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((1 1) tag) 
		  (sxml-object-builder-helper
		   tag ctr (sxml-object-builder nb ...))))
	(next ...)))
      ((_ specs ...)
       (sxml-set-object-builder "parse" () (specs ...)))))

  (define-syntax sxml-choice-object-builder
    (syntax-rules ()
      ((_ "parse" ((b p) ...) ())
       (make-choice-object-builder (list (list b p) ...)))
      ((_ "parse" (bp ...) ((tag ctr nb ...) next ...))
       (sxml-choice-object-builder "parse"
	(bp ... ((sxml-object-builder (tag ctr nb ...))
		 (sxml-object-builder-predicate tag)))
	(next ...)))
      ((_ spec specs ...)
       (sxml-choice-object-builder "parse" () (spec specs ...)))))
  
  (define-syntax sxml-object-builder
    (syntax-rules (* ? + <!> / *namespace*)
      ((_) #f)
      ((_ (*namespace* ((ns uri) ...)) specs ...)
       (syntax-parameterize ((*namespace* (syntax-rules ()
					    ((_) '((ns uri) ...)))))
	 (sxml-object-builder specs ...)))
      ((_ (* spec ...)) (sxml-set-object-builder (* spec ...)))
      ((_ (? spec ...)) (sxml-object-builder (spec ...)))
      ((_ (+ spec ...)) (sxml-set-object-builder (+ spec ...)))
      ((_ (/ spec specs ...)) (sxml-choice-object-builder spec specs ...))
      ((_ (<!> tag builder))
       (make-recursive-object-builder
	(sxml-object-builder-helper tag builder #f)))
      ((_ (tag ctr . next))
       (sxml-object-builder-helper tag ctr (sxml-object-builder . next)))
      ((_ builder) builder)
      ((_ spec specs ...) (sxml-set-object-builder spec specs ...))))

  ;; XML object
  (define-record-type xml-object
    (fields name attributes contents))
  (define xml-object-builder
    (sxml-object-builder
     (<!> (?? values) make-xml-object)))

  (define (sxml->xml-object sxml . maybe-handler)
    (apply sxml->object sxml xml-object-builder maybe-handler))
  
  )
