;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; naming/context.scm - Naming interface
;;;
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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
(library (naming context)
    (export naming-error? &naming-error naming-error-name
	    name-not-found? &name-not-found

	    (rename (naming-context <naming-context>)) 
	    naming-context? make-naming-context
	    naming-context-lookup naming-context-bind!
	    naming-context-set-attribute! naming-context-get-attribute

	    (rename (naming-category <naming-category>)) naming-category?
	    naming-category-context
	    (rename (scheme-category <scheme-category>)) scheme-category?

	    naming-category-specification? make-naming-category-specification
	    *default-naming-category-specification*
	    )
    (import (rnrs)
	    (sagittarius)
	    (rfc uri)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))

  (define-condition-type &naming-error &error
    make-naming-error naming-error?
    (name naming-error-name))
  (define-condition-type &name-not-found &naming-error
    make-name-not-found name-not-found?)

  (define-record-type naming-context
    (fields categories attributes parent)
    (protocol (lambda (p)
		(lambda (:optional (parent #f))
		  ;; TODO should we use treemap?
		  (p (make-hashtable string-ci-hash string-ci=?)
		     (make-equal-hashtable)
		     parent)))))

  (define (naming-context-set-attribute! context attr value)
    (hashtable-set! (naming-context-attributes context) attr value))
  (define (naming-context-get-attribute context attr . opt)
    (apply hashtable-ref (naming-context-attributes context) attr opt))
  
  (define-record-type naming-category-specification
    (fields make-category
	    name-parser
	    name-lookup
	    bind-value!))

  (define-record-type naming-category
    (fields context type category-specification)
    (protocol (lambda (p)
		(lambda (context type specification)
		  (p context type specification)))))

  ;; scheme:
  ;; for now it's not really efficient...
  ;; not sure what was the initial intension to make this
  ;; sort of directory structure emulator. feels like we
  ;; can make is simple hashtable.
  (define-record-type scheme-directory
    (fields name (mutable value) children)
    (protocol (lambda (p)
		(lambda (name value)
		  (p name value (make-string-hashtable))))))

  (define (scheme-directory-add-child! directory name)
    (let ((child (make-scheme-directory name #f)))
      (hashtable-set! (scheme-directory-children directory) name child)
      child))

  (define-record-type scheme-category
    (parent naming-category)
    (fields directory)
    (protocol (lambda (n)
		(lambda (context specification)
		  ((n context "scheme" specification)
		   (make-scheme-directory #f #f))))))

  (define scheme-name-token
    ;; a-zA-Z0-9_
    (char-set-intersection
     (char-set-union char-set:letter char-set:digit (->char-set #\_))
     char-set:ascii))
  
  (define (scheme-name-parser specific)
    (let-values (((auth path query frag) (uri-decompose-hierarchical specific)))
      (when (or auth query frag)
	(assertion-violation 'scheme-name-parser
			     "name path contains auth, query or fragmentation"
			     specific))
      (unless (string-every (lambda (c)
			      (or (char=? c #\/)
				  (char-set-contains? scheme-name-token c)))
			    path)
	(assertion-violation 'scheme-name-parser
			     "name contains other than [a-zA-Z0-9_/]" path))
      (string-tokenize path scheme-name-token)))

  (define (scheme-name-lookup category names)
    (let loop ((names names) (directory (scheme-category-directory category)))
      (cond ((null? names) #f)
	    ((hashtable-ref (scheme-directory-children directory)
			    (car names) #f)
	     => (lambda (directory)
		  (if (null? (cdr names))
		      (scheme-directory-value directory)
		      (loop (cdr names) directory))))
	    (else #f))))

  (define (scheme-bind-value! category names value)
    (when (null? names)
      (assertion-violation 'scheme-bind-value! "empty name" names))
    (let loop ((names names) (directory (scheme-category-directory category)))
      (cond ((and (null? (cdr names))
		  (equal? (scheme-directory-name directory) (car names)))
	     (scheme-directory-value-set! directory value))
	    ((hashtable-ref (scheme-directory-children directory)
			    (car names) #f)
	     => (lambda (directory)
		  (if (null? (cdr names))
		      (scheme-directory-value-set! directory value)
		      (loop (cdr names) directory))))
	    (else
	     (let ((child (scheme-directory-add-child! directory (car names))))
	       (if (null? (cdr names))
		   (scheme-directory-value-set! child value)
		   (loop (cdr names) child)))))))

  (define scheme-category-specification
    (make-naming-category-specification make-scheme-category
					scheme-name-parser
					scheme-name-lookup
					scheme-bind-value!))

  (define *default-naming-category-specification*
    (make-parameter scheme-category-specification
      (lambda (v)
	(unless (naming-category-specification? v)
	  (assertion-violation '*default-naming-category-specification*
			       "must be naming-category-specification" v))
	v)))
  
  (define (naming-context-lookup context name)
    (define (not-found)
      (raise (condition
	      (make-name-not-found name)
	      (make-who-condition 'naming-context-lookup)
	      (make-message-condition "name is not bound")
	      (make-irritants-condition (list context name)))))
    (let-values (((scheme specific) (uri-scheme&specific name)))
      (if (not scheme)
	  (raise (condition
		  (make-naming-error name)
		  (make-who-condition 'naming-context-lookup)
		  (make-message-condition "name must be a URI with scheme")
		  (make-irritants-condition (list context name))))
	  (let loop ((context context))
	    (cond ((hashtable-ref (naming-context-categories context) scheme) =>
		   (lambda (category)
		     (or (naming-category-lookup category specific)
			 (cond ((naming-context-parent context) =>
				(lambda (parent) (loop parent)))
			       (else (not-found))))))
		  ((naming-context-parent context) =>
		   (lambda (parent) (loop parent)))
		  (else (not-found)))))))

  (define (naming-context-bind! context name value
				:key (specification #f))
    (let-values (((scheme specific) (uri-scheme&specific name)))
      (if scheme
	  (let ((categories (naming-context-categories context)))
	    (cond ((hashtable-ref categories scheme #f) =>
		   (lambda (category)
		     (naming-category-bind! category specific value)))
		  (else
		   (let* ((spec (or specification
				    (*default-naming-category-specification*)))
			  (cate (make-category-of context spec)))
		     (unless (string-ci=? (naming-category-type cate) scheme)
		       (raise (condition
			       (make-naming-error name)
			       (make-who-condition 'naming-context-bind!)
			       (make-message-condition
				"name and specification doesn't match")
			       (make-irritants-condition (list name spec)))))
		     (hashtable-set! categories scheme cate)
		     (naming-category-bind! cate specific value)))))
	  (raise (condition
		  (make-naming-error name)
		  (make-who-condition 'naming-context-bind!)
		  (make-message-condition "name doesn't contain scheme")
		  (make-irritants-condition (list context name value)))))))

  (define (make-category-of context specification)
    ((naming-category-specification-make-category specification)
     context specification))
  
  (define (naming-category-lookup category name)
    (define specification (naming-category-category-specification category))
    (define parser (naming-category-specification-name-parser specification))
    ((naming-category-specification-name-lookup specification) category
     (parser name)))

  (define (naming-category-bind! category name value)
    (define specification (naming-category-category-specification category))
    (define parser (naming-category-specification-name-parser specification))
    ((naming-category-specification-bind-value! specification) category
     (parser name) value))
)
