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
    (export sxml->object ? * +
	    ;; object->sxml

	    sxml-object-builder

	    object-builder object-builder?
	    make-simple-object-builder simple-object-builder?
	    make-set-object-builder set-object-builder? 
	    )
    (import (rnrs)
	    (text sxml tools))

  ;; TODO maybe we can make this more generic?
  (define-record-type object-builder
    (fields build-object))
  
  (define-record-type simple-object-builder
    (fields tag?
	    >object
	    next-builder)
    (parent object-builder)
    (protocol (lambda (n)
		(lambda (tag? ->object next-builder)
		  ((n build-object) tag? ->object next-builder)))))
  (define-record-type set-object-builder
    (fields object-builders)
    (parent object-builder)
    (protocol (lambda (n)
		(lambda (accept-tags . object-builders)
		  ((n build-set-object)
		   (map cons accept-tags object-builders))))))
  
  (define (default-unknown-tag-handler builder sxml)
    (assertion-violation 'sxml->object "unknown tag" sxml builder))

  ;; Build simple object
  (define (build-object sxml builder handler)
    (define check-tag? (simple-object-builder-tag? builder))
    (define ->object (simple-object-builder->object builder))
    (define next-builder (simple-object-builder-next-builder builder))
    (cond ((sxml:element? sxml)
	   (let ((content (sxml:content sxml))
		 (attrs   (sxml:attr-list sxml)))
	     (if (check-tag? (sxml:name sxml))
		 (->object attrs (sxml->object content next-builder handler))
		 (handler builder sxml))))
	  (else (map (lambda (c) (sxml->object c builder))
		     (sxml:content sxml)))))

  (define (build-set-object sxml builder handler)
    (define builders (set-object-builder-object-builders builder))
    (define (find-builder builders tag)
      (define (check-tag s) (eq? (cadr s) tag))
      (cond ((assp check-tag builders) => cdr)
	    (else #f)))
    ;; FIXME There must be a better way to do it
    (define (order-objects objects)
      (define builder-vec (list->vector builders))
      (define len (vector-length builder-vec))
      (define vec (make-vector len #f))
      (define index-table (make-eqv-hashtable))
      (define order-table (make-eq-hashtable))
      (define (try-set! vec where object)
	(if (caddr where)
	    (let ((index (car where)))
	      (when (vector-ref vec index)
		(assertion-violation 'sxml->object "Too many objects" object))
	      (vector-set! vec index (vector object)))
	    (if (vector-ref vec (car where))
		(let ((box (vector-ref vec (car where))))
		  (vector-set! box 0 (cons object (vector-ref box 0))))
		(vector-set! vec (car where) (vector (list object))))))
      ;; check if the box can be #f
      (define (box-ref box index)
	(let ((tag (hashtable-ref index-table index #f)))
	  (if box
	      (let ((v (vector-ref box 0))
		    (required (cadr (car tag))))
		(unless (or (not required) (<= (length v) required))
		  (assertion-violation 'sxml->object "Too many objects" tag v))
		v)
	      (if (zero? (caar tag))
		  #f
		  (assertion-violation 'sxml->object
				       "Required element is missing" tag)))))
      ;; init tables
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(let ((tag (car (vector-ref builder-vec i))))
	  (hashtable-set! order-table (cadr tag) (cons i (car tag)))
	  (hashtable-set! index-table i tag)))
      (for-each (lambda (object)
		  (let ((tag (car object))
			(object (cdr object)))
		    (cond ((hashtable-ref order-table tag #f) =>
			   (lambda (where) (try-set! vec where object)))
			  (else (handler builder sxml)))))
		objects)
      (do ((i 0 (+ i 1)) (r '() (cons (box-ref (vector-ref vec i) i) r)))
	  ((= i len) (reverse r))))

    (let loop ((contents (sxml:content sxml)) (objects '()))
      (if (null? contents)
	  (order-objects objects)
	  (let* ((tag (sxml:name (car contents)))
		 (object (sxml->object (car contents)
				       (find-builder builders tag) handler)))
	    (loop (cdr contents) (cons (cons tag object) objects))))))

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
  
  (define-syntax sxml-object-builder-helper
    (syntax-rules ()
      ((_ (ignore tag) ctr next)
       (make-simple-object-builder (lambda (t) (eq? t 'tag)) ctr next))
      ((_ tag ctr next)
       (make-simple-object-builder (lambda (t) (eq? t 'tag)) ctr next))))

  (define-syntax sxml-set-object-builder
    (syntax-rules (? * +)
      ((_ "parse" ((tag ctr next) ...) ())
       (make-set-object-builder '(tag ...)
	 (sxml-object-builder-helper tag ctr next)
	 ...))
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
    (syntax-rules (* ? +)
      ((_) #f)
      ((_ (* spec ...)) (sxml-set-object-builder (* spec ...)))
      ((_ (? spec ...)) (sxml-object-builder (spec ...)))
      ((_ (+ spec ...)) (sxml-set-object-builder (+ spec ...)))
      ((_ (tag ctr . next))
       (sxml-object-builder-helper tag ctr (sxml-object-builder . next)))
      ((_ builder) builder)
      ((_ spec specs ...) (sxml-set-object-builder spec specs ...))))
  
  )
