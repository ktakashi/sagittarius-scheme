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
    (export sxml->object ? * + ?? <!>
	    ;; object->sxml

	    ;; XML object
	    xml-object xml-object? make-xml-object
	    xml-object-name xml-object-attributes xml-object-contents
	    sxml->xml-object

	    sxml-object-builder

	    object-builder object-builder?
	    make-simple-object-builder simple-object-builder?
	    make-set-object-builder set-object-builder?
	    make-recursive-object-builder recursive-object-builder?
	    )
    (import (rnrs)
	    (text sxml tools))

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
      (map (lambda (v b) (if v (vector-ref v 0) (check-required b)))
	   (vector->list objects) builders))
    (let loop ((contents (sxml:content sxml)))
      (if (null? contents)
	  (retrieve-objects)
	  (let ((tag (sxml:name (car contents))))
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
  
  (define-syntax sxml-object-builder-predicate
    (syntax-rules (??)
      ((_ (?? pred)) pred)
      ((_ tag) (let ((tag (lambda (t) (eq? t 'tag)))) tag))))
  (define-syntax sxml-object-builder-helper
    (syntax-rules (??)
      ((_ tag ctr next)
       (make-simple-object-builder 
	(sxml-object-builder-predicate tag) ctr next))))

  (define-syntax sxml-set-object-builder
    (syntax-rules (? * +)
      ((_ "parse" (((count tag) ctr next) ...) ())
       (make-set-object-builder 
	(list (cons* (sxml-object-builder-helper tag ctr next)
		     (sxml-object-builder-predicate tag)
		     'count) ...)))
      ;; ?
      ((_ "parse" (tcn ...) ((? tag ctr nb ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((0 1) tag) ctr (sxml-object-builder nb ...)))
	(next ...)))
      ;; *
      ((_ "parse" (tcn ...) ((* tag ctr nb ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((0 #f) tag) ctr (sxml-object-builder nb ...)))
	(next ...)))
      ;; +
      ((_ "parse" (tcn ...) ((+ tag ctr nb ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((1 #f) tag) ctr (sxml-object-builder nb ...)))
	(next ...)))
      
      ((_ "parse" (tcn ...) ((tag ctr nb ...) next ...))
       (sxml-set-object-builder "parse"
	(tcn ... (((1 1) tag) ctr (sxml-object-builder nb ...)))
	(next ...)))
      ((_ specs ...)
       (sxml-set-object-builder "parse" () (specs ...)))))
  
  (define-syntax sxml-object-builder
    (syntax-rules (* ? + <!>)
      ((_) #f)
      ((_ (* spec ...)) (sxml-set-object-builder (* spec ...)))
      ((_ (? spec ...)) (sxml-object-builder (spec ...)))
      ((_ (+ spec ...)) (sxml-set-object-builder (+ spec ...)))
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
