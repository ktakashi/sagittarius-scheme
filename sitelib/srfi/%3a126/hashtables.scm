;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a126/hashtables.scm - R6RS-based hashtables
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

(library (srfi :126 hashtables)
    (export (rename (srfi:make-eq-hashtable make-eq-hashtable)
		    (srfi:make-eqv-hashtable make-eqv-hashtable)
		    (srfi:make-hashtable make-hashtable))
	    alist->eq-hashtable alist->eqv-hashtable alist->hashtable
	    weakness
	    hashtable?
	    hashtable-size
	    (rename (srfi:hashtable-ref hashtable-ref))
	    hashtable-set! hashtable-delete!
	    hashtable-contains?
	    hashtable-lookup 
	    (rename (srfi:hashtable-update! hashtable-update!))
	    hashtable-intern!
	    (rename (srfi:hashtable-copy hashtable-copy))
	    hashtable-clear! 
	    hashtable-empty-copy
	    hashtable-keys hashtable-values hashtable-entries
	    (rename (hashtable-keys-list hashtable-key-list)
		    (hashtable-values-list hashtable-value-list))
	    hashtable-entry-lists
	    hashtable-walk
	    hashtable-update-all! hashtable-prune! 
	    hashtable-merge!
	    hashtable-sum hashtable-map->lset hashtable-find
	    hashtable-empty? hashtable-pop! hashtable-inc! hashtable-dec!
	    hashtable-equivalence-function hashtable-hash-function 
	    (rename (srfi:hashtable-weakness hashtable-weakness))
	    hashtable-mutable?
	    hash-salt equal-hash string-hash string-ci-hash symbol-hash)
    (import (rnrs)
	    (sagittarius)
	    (core base)
	    ;; we can't use srfi-27, it returns the same value each time
	    (math))

;; converts SRFI-126 weakness symbols to builtin symbols
(define (->weak-key w)
  (case w
    ((weak-key) 'key)
    ((weak-value) 'value)
    ((weak-key-and-value) 'both)
    ((#f) #f) ;; no weakness
    (else 'unsupported)))

(define srfi:make-eq-hashtable
  (case-lambda
   (() (make-eq-hashtable #f #f))
   ((k) (make-eq-hashtable k #f))
   ((k w) (make-eq-hashtable k (->weak-key w)))))

(define srfi:make-eqv-hashtable
  (case-lambda
   (() (make-eqv-hashtable #f #f))
   ((k) (make-eqv-hashtable k #f))
   ((k w) (make-eqv-hashtable k (->weak-key w)))))

(define srfi:make-hashtable
  (case-lambda
   ((hash equiv) (srfi:make-hashtable hash equiv #f #f))
   ((hash equiv capacity) (srfi:make-hashtable hash equiv capacity #f))
   ((hash equiv capacity weakness)
    (define (->hash-function hash)
      (cond ((pair? hash) (car hash))
	    ((procedure? hash) hash)
	    (else 
	     (assertion-violation 'make-hashtable
				  "procedure or pair of procedure is required"
				  hash))))
    (let ((weakness (->weak-key weakness)))
      (if hash
	  (make-hashtable (->hash-function hash) equiv capacity weakness)
	  (cond ((eq? equiv eq?) (make-eq-hashtable capacity weakness))
		((eq? equiv eqv?) (make-eqv-hashtable capacity weakness))
		(else 
		 (assertion-violation 'make-hashtable
				      "equiv must be eq? or eqv? for #f hash"
				      equiv))))))))

(define alist->eq-hashtable
  (case-lambda
   ((alist) (alist->eq-hashtable (length alist) #f alist))
   ((capacity alist) (alist->eq-hashtable capacity #f alist))
   ((capacity weakness alist) 
    (let ((ht (make-eq-hashtable capacity (->weak-key weakness))))
      (for-each (lambda (v) (hashtable-set! ht (car v) (cdr v))) alist)
      ht))))

(define alist->eqv-hashtable
  (case-lambda
   ((alist) (alist->eqv-hashtable (length alist) #f alist))
   ((capacity alist) (alist->eqv-hashtable capacity #f alist))
   ((capacity weakness alist) 
    (let ((ht (make-eqv-hashtable capacity (->weak-key weakness))))
      (for-each (lambda (v) (hashtable-set! ht (car v) (cdr v))) alist)
      ht))))

(define alist->hashtable
  (case-lambda
   ((hash eqv alist) (alist->hashtable hash eqv (length alist) #f alist))
   ((hash eqv capacity alist) (alist->hashtable hash eqv capacity #f alist))
   ((hash eqv capacity weakness alist) 
    (let ((ht (srfi:make-hashtable hash eqv capacity weakness)))
      (for-each (lambda (v) (hashtable-set! ht (car v) (cdr v))) alist)
      ht))))

(define-syntax weakness
  (lambda (x)
    (syntax-case x ()
      ((_ s)
       (memq (syntax->datum #'s) '(weak-key weak-value weak-key-and-value))
       #''s)
      ;; ok we don't support the rest
      ((_ s) (syntax-violation 'weakness #'s "not supported")))))

(define srfi:hashtable-ref
  (let ((mark (list 'hashtable-ref)))
    (case-lambda
     ((ht key) (srfi:hashtable-ref ht key mark))
     ((ht key default)
      (let ((r (hashtable-ref ht key default)))
	(if (eq? r mark)
	    (assertion-violation 'hashtable-ref
				 "no key is associated but default isn't given"
				 key)
	    r))))))

(define hashtable-lookup
  (let ((m (list 'hashtable-lookup)))
    (lambda (ht key)
      (let ((r (hashtable-ref ht key m)))
	(if (eq? r m)
	    (values (undefined) #f)
	    (values r #t))))))

(define srfi:hashtable-update!
  (case-lambda
   ((ht key proc default) (hashtable-update! ht key proc default))
   ((ht key proc) 
    ;; FIXME this calculates hash twice
    (unless (hashtable-contains? ht key)
      (assertion-violation 'hashtable-update! "default isn't provided" key))
    (hashtable-update! ht key proc 'dummy))))

;; TODO should we do better way?
(define hashtable-intern! 
  (let ((m (list 'hashtable-intern!)))
    (lambda (ht key proc)
      (hashtable-update! ht key (lambda (v) (if (eq? v m) (proc) v)) m))))

(define (make-template-hashtable ht k weakness)
  (case (hashtable-type ht)
    ((eq) (make-eq-hashtable ht k weakness))
    ((eqv) (make-eqv-hashtable ht k weakness))
    ((equal) (make-equal-hashtable ht k weakness))
    ((string) (make-string-hashtable ht k weakness))
    (else (make-hashtable (hashtable-hasher ht) (hashtable-compare ht)
			  k weakness))))

(define srfi:hashtable-copy
  (case-lambda
   ((ht) (srfi:hashtable-copy ht #f))
   ((ht mutable?) (hashtable-copy ht mutable?))
   ((ht mutable? weakness) 
    (let ((r (make-template-hashtable ht (hashtable-size ht) weakness)))
      (hashtable-for-each (lambda (k v) (hashtable-set! r k v)) ht)
      r))))

(define (hashtable-empty-copy ht :optional (k #f))
  (make-template-hashtable ht k (hashtable-weakness ht)))

(define (hashtable-entry-lists ht)
  (let-values (((ks vs) (hashtable-entries ht)))
    (values (vector->list ks) (vector->list vs))))

(define (hashtable-walk ht proc) (hashtable-for-each proc ht))

(define (hashtable-update-all! ht proc)
  (for-each (lambda (k) (hashtable-update! ht k (lambda (v) (proc k v)) 'dummy))
	    (hashtable-keys-list ht)))

(define (hashtable-prune! ht proc)
  (for-each (lambda (k) (when (cdr k) (hashtable-delete! ht (car k))))
	    (hashtable-map (lambda (k v) (cons k (proc k v))) ht)))

(define (hashtable-merge! dest src)
  (hashtable-for-each (lambda (k v) (hashtable-set! dest k v)) src))

(define (hashtable-sum ht init proc) (hashtable-fold proc ht init))

(define (hashtable-map->lset ht proc) (hashtable-map proc ht))

(define (hashtable-find ht proc)
  (let ((itr (%hashtable-iter ht))
	(eof (cons #t #t)))
    (let loop ()
      (let-values (((k v) (itr eof)))
	(cond ((eq? k eof) (values (undefined) (undefined) #f))
	      ((proc k v) (values k v #t))
	      (else (loop)))))))

(define (hashtable-empty? ht) (zero? (hashtable-size ht)))

(define (hashtable-pop! ht)
  (let-values (((key value found?) (hashtable-find ht (lambda (k v) #t))))
    (unless found? 
      (assertion-violation 'hashtable-pop! "empty hashtable was given"))
    (hashtable-delete! ht key)
    (values key value)))

(define (hashtable-inc! ht key :optional (num 1))
  (hashtable-update! ht key (lambda (v) (+ v num)) 0))
(define (hashtable-dec! ht key :optional (num 1))
  (hashtable-update! ht key (lambda (v) (- v num)) 0))

(define (srfi:hashtable-weakness ht)
  (case (hashtable-weakness ht)
    ((key) 'weak-key)
    ((value) 'weak-value)
    ((both) 'weak-key-and-value)
    ;; must be #f
    (else #f)))

(define *hash-salt*
  ;; keep it fixnum range
  (let ((salt (bytevector->uinteger (read-sys-random 30)))
	(seed (getenv "SRFI_126_HASH_SEED")))
    (if (or (not seed) (string=? seed ""))
        salt
        (mod (fold-left (lambda (result char) (+ (char->integer char) result))
			0
			(string->list seed))
	     (greatest-fixnum)))))

(define-syntax hash-salt
  (syntax-rules ()
    ((_) *hash-salt*)))

)
