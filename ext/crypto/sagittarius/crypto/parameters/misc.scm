;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/parameters/misc.scm - Parameters misc APIs
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius crypto parameters misc)
    (export define-compositable-record-type)
    (import (rnrs))
;; Do we want to put this somewhere else?
(define-syntax define-compositable-record-type
  (lambda (x)
    (define (make-record-ctr&pred k name)
      (define n (symbol->string (syntax->datum name)))
      (datum->syntax k
       (list (string->symbol (string-append "make-" n))
	     (string->symbol (string-append n "?")))))
    (define (make-composite-name&definer k name)
      (define n (symbol->string (syntax->datum name)))
      (datum->syntax k
       (list (string->symbol (string-append "composite-" n))
	     (string->symbol (string-append "make-define-" n )))))
    (syntax-case x ()
      ((k name clause ...)
       (identifier? #'name)
       (with-syntax (((ctr pred) (make-record-ctr&pred #'k #'name)))
	 #'(k (name ctr pred) clause ...)))
      ((k (name ctr pred) clause ...)
       (with-syntax (((find-parameter cpred)
		      (generate-temporaries '(find-parameter pred)))
		     ((composite-name definer-name)
		      (make-composite-name&definer #'k #'name)))
       #'(begin
	   (define-record-type (name dummy0 pred) clause ...)
	   (define-record-type (composite-name dummy1 cpred)
	     (parent name)
	     (fields (immutable elements get-elements))
	     (protocol
	      (lambda (p)
		(lambda params
		  (unless (for-all pred params)
		    (assertion-violation 'composite-name
		     (string-append (symbol->string 'name) " is required")
		     params))
		  ((p) (apply append (map (lambda (p)
					    (if (cpred p)
						(get-elements p)
						(list p))) params)))))))
	   (define ctr dummy1)
	   (define (find-parameter pred composite)
	     (cond ((cpred composite)
		    (let loop ((param (get-elements composite)))
		      (cond ((null? param) #f)
			    ((pred (car param)) (car param))
			    (else (loop (cdr param))))))
		   ((pred composite) composite)
		   (else #f)))

	   (define-syntax definer-name
	     (lambda (xx)
	       (syntax-case xx ()
		 ((k) #'(k name))
		 ((_ <parent>)
		  #'(lambda (xxx)
		      (syntax-case xxx ()
			;; field
			((k "field" name ctr pred %ctr %pred
			    (field ((... ...) (... ...)))
			    ((fname acc) rest ((... ...) (... ...))))
			 #'(k "field" name ctr pred %ctr %pred
			      (field ((... ...) (... ...))
				     (fname real-accessor acc))
			      (rest ((... ...) (... ...)))))
			((k "field" name ctr pred %ctr %pred
			    (field ((... ...) (... ...))) ())
			 #'(k "ctr" name ctr pred %ctr %pred
			      (field ((... ...) (... ...)))))
			;; ctr
			((k "ctr" name (ctr proc) pred %ctr %pred
			    (field ((... ...) (... ...))))
			 #'(k "parent" name ctr pred %ctr %pred (protocol proc)
			      (field ((... ...) (... ...)))))
			((k "ctr" name ctr pred %ctr %pred
			    (field ((... ...) (... ...))))
			 #'(k "parent" name ctr pred %ctr %pred  (protocol #f)
			      (field ((... ...) (... ...)))))
			;; parent
			((k "parent" (name p) ctr pred %ctr %pred
			    protocol fields)
			 #'(k "make" name ctr pred %ctr %pred
			      (parent p) protocol fields))
			((k "parent" name ctr pred %ctr %pred protocol fields)
			 #'(k "make" name ctr pred %ctr %pred
			      (parent <parent>) protocol fields))
			((k "make" name ctr pred %ctr %pred parent protocol
			    ((field real acc) ((... ...) (... ...))))
			 #'(begin
			     (define-record-type (name ctr %pred)
			       parent
			       protocol
			       (fields (immutable field real)
				       ((... ...) (... ...))))
			     (define (pred o)
			       (or (%pred o)
				   (and (cpred o)
					(find-parameter %pred o))))
			     (define (acc o . optional)
			       (let ((p (find-parameter %pred o)))
				 (cond (p (real p))
				       ((not (null? optional)) (car optional))
				       (else 
					(error 'acc
					       "doesn't have the field")))))
			     ((... ...) (... ...))))
			;; entry point
			((k name ctr pred fields ((... ...) (... ...)))
			 #'(k "field" name ctr pred %ctr %pred ()
			      (fields ((... ...) (... ...)))))))))))))))))
)
