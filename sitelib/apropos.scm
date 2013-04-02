;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; apropos.scm: REPL support
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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
#!compatible
(library (apropos)
    (export apropos)
    (import (rnrs)
	    (core base) ;; for hashtable->alist
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius vm)
	    (pp))
  
  #|
  library parents structure got changed. this is the memo.
  paretns ::= ((#<self library> #<parent> . spec) ...)
  spec is the import spec and might be null.
  |#
  (define (apropos arg :key (port (current-output-port)))
    (define (->regex arg)
      (cond ((symbol? arg) (regex (symbol->string arg) LITERAL))
	    ((string? arg) (regex arg))
	    ((regex-pattern? arg) arg)
	    (else 
	     (assertion-violation 
	      'apropos 
	      (format "symbol, string or regex-pattern required, but got ~a"
		      arg)))))
    (let1 pattern (->regex arg)
      (define (match? e) (and (pattern (symbol->string e)) e))
      (define core (format "~a" '(core)))
      (define (library->string lib)
	(let1 name (library-name lib)
	  (if (eq? name 'null) core (symbol->string name))))
      ;; search parents
      ;; we don't consider the renaming so only export spec is important.
      (define (store-variable real renamed library store)
	(and-let* (( (symbol? real) )
		   ( (match? real) )
		   (gloc (find-binding library real #f))
		   (val  (gloc-ref gloc))
		   (key  (cons (or renamed real) val))
		   (name (library->string library)))
	  (cond ((~ store key)
		 => (^n (set! (~ store key) (lset-union eq? n (list name)))))
		(else (set! (~ store key) (list name))))))
      (define (search-library library using-name store seen)
	(unless (~ seen library)
	  ;; check export spec
	  (let1 exported (library-exported library)
	    (cond ((not exported) ;; c library
		   ;; we need to check all defined variables
		   (let ((keys (hashtable-keys-list (library-table library)))
			 (name (library-name library)))
		     (for-each (cut store-variable <> #f library store) keys)))
		  ((memq :all (car exported))
		   ;; search parent but put the current name
		   (for-each (cut search-library <> library store seen)
			     (map car (library-parents library))))
		  (else
		   (let ((vars (car exported))
			 (renamed (cdr exported))
			 (library (or using-name library)))
		     (for-each (cut store-variable <> #f library store) vars)
		     (for-each 
		      (^p (store-variable (car p) (cdr p) library store))
		      renamed)))))))
      (define (search-parents library store seen)
	(unless (~ seen library)
	  (set! (~ seen library) #t)
	  (and-let* ((parents (library-parents library))
		     ( (not (null? parents)) )
		     (flat (map car parents)))
	    (for-each (cut search-library <> #f store seen) flat)
	    ;; should we walk through all parents?
	    (for-each (cut search-parents <> store seen) flat))))

      (let* ((keys (hashtable-keys-list (library-table (vm-current-library))))
	     (library (library-name (vm-current-library)))
	     (ret (filter-map match? keys)))
	(for-each (cut format port ";; ~30,,,,a ~a~%" <> library) ret))
      
      (let ((h (make-equal-hashtable))
	    (seen (make-eq-hashtable)))
	(search-parents (vm-current-library) h seen)
	(for-each (lambda (p)
		    (format port ";; ~30,,,,a ~a~%" (caar p)
			    (string-join (cdr p) " ")))
		  (hashtable->alist h)))))
)