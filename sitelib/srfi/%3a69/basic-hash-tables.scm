;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; SRFI-69 Basic hash tables
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :69 basic-hash-tables)
  (export make-hash-table (rename (hashtable? hash-table?))
	  alist->hash-table

	  (rename
	   (hashtable-equivalence-function hash-table-equivalence-function)
	   (hashtable-hash-function hash-table-hash-function))

	  hash-table-ref hash-table-ref/default
	  (rename (hashtable-set! hash-table-set!)
		  (hashtable-delete! hash-table-delete!)
		  (hashtable-contains? hash-table-exists?))
	  	  
	  hash-table-update!
	  (rename (hashtable-update! hash-table-update!/default))

	  (rename (hashtable-size hash-table-size)
		  (hashtable-keys-list hash-table-keys)
		  (hashtable-values-list hash-table-values))
	  hash-table-walk hash-table-fold
	  (rename (hashtable->alist hash-table->alist))
	  hash-table-copy hash-table-merge!

	  (rename (equal-hash hash)
		  (eq-hash    hash-by-identity))
	  string-hash string-ci-hash)
  (import (rnrs)
	  ;; make-string-hashtable and make-equal-hashtable
	  (sagittarius)
	  (sagittarius control)
	  (only (util hashtables) hashtable->alist hashtable-for-each
		hashtable-fold))

  (define make-hash-table
    (case-lambda
     ((eql? hash) (make-hashtable hash eql?))
     ((eql?)
      (cond ((eq? eql? eq?)        (make-eq-hashtable))
	    ((eq? eql? eqv?)       (make-eqv-hashtable))
	    ((eq? eql? equal?)     (make-equal-hashtable))
	    ((eq? eql? string=?)   (make-string-hashtable))
	    ((eq? eql? string-ci?) (make-hashtable string-ci=? string-ci-hash))))
     (() (make-equal-hashtable))))

  (define no-entry (list 'no-entry))
  ;; a bit different from (util hashtables)
  ;; maybe it's better to adjust that one to this one
  (define (alist->hash-table alist . opts)
    (rlet1 ht (apply make-hash-table opts)
      (for-each (lambda (kv)
		  (hashtable-update!
		   ht
		   (car kv)
		   (lambda (x) (if (eq? no-entry x) (cdr kv) x))
		   no-entry)) alist)))

  (define (failure-thunk who key)
    (lambda () (error who "no association for key" key)))
  
  (define hash-table-ref
    (case-lambda
     ((ht key thunk)
      (let ((val (hashtable-ref ht key no-entry)))
	(if (eq? val no-entry)
	    (thunk)
	    val)))
     ((ht key)
      (hash-table-ref ht key (failure-thunk 'hash-table-ref key)))))

  ;; builtin hashtable-ref allow not to have default
  (define (hash-table-ref/default ht key default)
    (hashtable-ref ht key default))

  (define hash-table-update!
    (case-lambda
     ((ht key proc thunk)
      (hashtable-update! ht key
			 (lambda (v)
			   (if (eq? v no-entry)
			       (thunk)
			       (proc v)))
			 no-entry))
     ((ht key proc)
      (hash-table-update! ht key proc (failure-thunk 'hash-table-update! key)))))

  (define (wrong-type-argument-message expect got . nth)
    (if (null? nth)
	(format "expected ~a, but got ~a" expect got)
	(format "expected ~a, but got ~a, as argument ~a" expect got
		(car nth))))
  (define (hash-table-walk table proc)
    (hashtable-for-each (lambda (k v) (proc k v)) table))
  (define (hash-table-fold table kons knil) (hashtable-fold kons table knil))

  (define (hash-table-copy ht) (hashtable-copy ht #t))
  (define (hash-table-merge! ht1 ht2)
    (hashtable-for-each (lambda (k v) (hashtable-set! ht1 k v)) ht2)
    ht1)

  )
