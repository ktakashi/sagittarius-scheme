;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a125/hash-tables.scm - Intermediate hash tables
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

(library (srfi :125 intermediate-hash-tables)
    (export make-hash-table
	    hash-table
	    hash-table-unfold
	    alist->hash-table

	    (rename (hashtable? hash-table?)
		    (hashtable-contains? hash-table-contains?))
	    hash-table-empty?
	    hash-table=?
	    (rename (hashtable-mutable? hash-table-mutable?))

	    hash-table-ref
	    (rename (hashtable-ref hash-table-ref/default))

	    hash-table-set!
	    hash-table-delete!
	    hash-table-intern!
	    hash-table-update!
	    ;; the same as R6RS' one
	    (rename (hashtable-update! hash-table-update!/default))
	    hash-table-push!
	    (rename (hashtable-pop! hash-table-pop!))
	    (rename (hashtable-clear! hash-table-clear!))

	    (rename (hashtable-size hash-table-size)
		    (hashtable-key-list hash-table-keys)
		    (hashtable-value-list hash-table-values)
		    (hashtable-entry-lists hash-table-entries))
	    hash-table-find
	    hash-table-count

	    hash-table-map
	    (rename (hashtable-for-each hash-table-for-each))
	    hash-table-map!
	    hash-table-map->list
	    hash-table-fold
	    hash-table-prune!

	    (rename (hashtable-copy hash-table-copy))
	    hash-table-empty-copy
	    (rename (hashtable->alist hash-table->alist))

	    hash-table-union!
	    hash-table-intersection!
	    hash-table-difference!
	    hash-table-xor!
	    (rename (hash-table-union! hash-table-merge!))

	    ;; these are deprecated but still there for compatibility
	    (rename (s69:hash               hash)
		    (s69:string-hash        string-hash)
		    (s69:string-ci-hash     string-ci-hash)
		    (s69:hash-by-identity   hash-by-identity)
		    (s69:hash-table-exists? hash-table-exists?)
		    (s69:hash-table-walk    hash-table-walk))
	    (rename (s69:hash-table-equivalence-function 
		     hash-table-equivalence-function)
		    (s69:hash-table-hash-function
		     hash-table-hash-function))
	    )
    (import (rename (rnrs) (equal? r6rs-equal?))
	    (only (core) equal?)
	    (core base) ;; for hashtable-for-each
	    (sagittarius) ;; for other hashtable creation
	    (srfi :1 lists) ;; for lset=?
	    (prefix (srfi :69 basic-hash-tables) s69:)
	    (srfi :126 hashtables) ;; for hashtable-pop!
	    ;; according to the SRFI, we need to use comparator
	    ;; from SRFI-128 (though, both implementations make
	    ;; the same object).
	    (srfi :128 comparators))

(define (make-hash-table pred . args)
  (define (non-negative-exact-integer? i)
    (and (integer? i) (exact? i) (>= i 0)))
  (define (get-hash-table-type this appeared)
    (define (combine this appeared)
      (if (and appeared (not (eq? appeared this))) 'both this))
    (case this
      ((weak-keys) (combine 'key appeared))
      ((weak-values) (combine 'value appeared))
      ;; sorry, we don't know you
      (else appeared)))
  ;; TODO better handling...
  (define (parse-args args)
    (let loop ((k #f) (sym #f) (args args))
      (cond ((null? args) (list k sym))
	    ((non-negative-exact-integer? (car args))
	     (loop (car args) sym (cdr args)))
	    ((get-hash-table-type (car args) sym) =>
	     (lambda (sym) (loop k sym (cdr args))))
	    ;; just ignore, maybe good for other SRFI implementations
	    (else (loop k sym (cdr args))))))
  (define (make-string-ci-hashtable . args)
    (apply make-hashtable string-ci-hash string-ci=? args))
  (define (make-r6rs-equal-hashtable . args)
    (apply make-hashtable equal-hash r6rs-equal? args))
  (define (get-ctr/hasher pred args)
    (define (get-it pred)
      (cond ((eq? pred eq?)         (values make-eq-hashtable #f))
	    ((eq? pred eqv?)        (values make-eqv-hashtable #f))
	    ((eq? pred equal?)      (values make-equal-hashtable #f))
	    ((eq? pred r6rs-equal?) (values make-r6rs-equal-hashtable #f))
	    ((eq? pred string=?)    (values make-string-hashtable #f))
	    ((eq? pred string-ci=?) (values make-string-ci-hashtable #f))
	    (else (assertion-violation 'make-hash-table
		   "non supported equality predicate without hash function"
		   pred))))
    (cond ((null? args) (get-it pred))
	  ((procedure? (car args)) (values #f (car args)))
	  (else (get-it pred))))

  (cond ((comparator? pred)
	 (apply make-hashtable
		(comparator-hash-function pred)
		(comparator-equality-predicate pred)
		(parse-args args)))
	((procedure? pred)
	 (let-values (((ctr hasher) (get-ctr/hasher pred args)))
	   ;; if hasher is found, then we need to create
	   ;; by make-hashtable
	   (if hasher
	       (apply make-hashtable hasher pred (parse-args (cdr args)))
	       ;; otherwise, juse call constructor
	       (apply ctr (parse-args args)))))
	(else
	 (assertion-violation 'make-hash-table
	  "first argument must be comparator or equality procedure" pred))))

(define (hash-table comparator . keys&values)
  (let ((ht (make-hashtable/comparator comparator)))
    (let loop ((keys&values keys&values))
      (if (null? keys&values)
	  ;; make it immutable
	  (hashtable-copy ht)
	  (let ((key (car keys&values))
		(value (cadr keys&values)))
	    (hashtable-set! ht key value)
	    (loop (cddr keys&values)))))))

(define (hash-table-unfold stop? mapper successor seed comparator . args)
  (let ((ht (apply make-hash-table comparator args)))
    (let loop ((seed seed))
      (if (stop? seed)
	  ht
	  (let-values (((key value) (mapper seed)))
	    (hashtable-set! ht key value)
	    (loop (successor seed)))))))

(define (alist->hash-table alist comparator . args)
  (let ((ht (apply make-hash-table comparator args)))
    (for-each (lambda (key&value)
		(hashtable-set! ht (car key&value) (cdr key&value)))
	      alist)
    ht))

(define (hash-table-empty? ht) (zero? (hashtable-size ht)))
;; it's rather vague to implement this procedure, but we assume
;; given 2 hashtables should have the same type equality procedure.
;; NB: I'm not even sure how it should be, but test case of the
;;     sample implementation requires like this.
(define (hash-table=? value-comparator ht1 ht2)
  (let ((pred1 (hashtable-equivalence-function ht1))
	(pred2 (hashtable-equivalence-function ht2)))
    (and-let* (( (= (hashtable-size ht1) (hashtable-size ht2)) )
	       (pred (lambda (a b) 
		       ;; predicate should never raise an error
		       ;; thus we do like this. a bit expensive
		       ;; but no choice.
		       (guard (e (else #f))
			 (and (pred1 a b) (pred2 a b)))))
	       (keys1 (hashtable-keys-list ht1))
	       (keys2 (hashtable-keys-list ht2))
	       ( (lset= pred keys1 keys2) ))
      ;; maybe getting all values would be faster?
      (for-all (lambda (k)
		 (=? value-comparator 
		     (hashtable-ref ht1 k) 
		     (hashtable-ref ht2 k)))
	       keys1))))

(define (failure-thunk who key)
  (lambda () (error who "no association for key" key)))
(define hash-table-ref 
  (let ((mark (list 'fail)))
    (lambda (ht key
		:optional (failure (failure-thunk 'hash-table-ref key))
			  (success values))
      (let ((val (hashtable-ref ht key mark)))
	(if (eq? val mark)
	    (failure)
	    (success val))))))

;; the SRFI doesn't say if the length of the list of key and value is 
;; not even number. (then it's unspecified and whatever is fine, I guess)
;; so we modify the hashtable even if the length is not even (and raise
;; an error).
(define (hash-table-set! ht . keys&values)  
  (let loop ((keys&values keys&values))
    (unless (null? keys&values)
      (let ((key (car keys&values))
	    (value (cadr keys&values)))
	(hashtable-set! ht key value)
	(loop (cddr keys&values))))))

(define (hash-table-delete! ht . keys)  
  (fold (lambda (key acc) 
	  (if (hashtable-contains? ht key)
	      (begin (hashtable-delete! ht key) (+ acc 1))
	      acc)) 0 keys))

(define hash-table-intern!
  (let ((mark (list 'hash-table-intern!)))
    (lambda (ht key failure)
      (let ((r (hashtable-ref ht key mark)))
	(if (eq? r mark)
	    (let ((r (failure)))
	      (hashtable-set! ht key r)
	      r)
	    r)))))

;; we don't do efficient way for now
(define (hash-table-update! ht key updater
	    :optional (failure (failure-thunk 'hashtable-update! key))
		      (success values))
  (hash-table-set! ht key (updater (hash-table-ref ht key failure success))))

;; not sure if this is removed or not, so just digged from old
;; specification..
;; NB: this doesn't make pair of hash-table-pop!, so it might be
;;     removed. just need confirmation
(define hash-table-push!
  (let ((mark (list 'hash-table-push!)))
    (lambda (ht key val failure)
      (let ((v (hashtable-ref ht key mark)))
	(if (eq? mark v)
	    ;; I don't understand this, but it says so
	    ;; NB: shouldn't it (cons val (failure))?
	    (hashtable-set! ht key (failure))
	    (hashtable-set! ht key (cons val v)))))))

;; different from SRFI-126
(define (hash-table-find ht proc failure)
  (let ((itr (%hashtable-iter ht))
	(eof (cons #t #t)))
    (let loop ()
      (let-values (((k v) (itr eof)))
	(cond ((eq? k eof) (failure))
	      ((proc k v))
	      (else (loop)))))))

(define (hash-table-count ht proc)
  (let ((itr (%hashtable-iter ht))
	(eof (cons #t #t)))
    (let loop ((c 0))
      (let-values (((k v) (itr eof)))
	(cond ((eq? k eof) c)
	      ((proc k v) (loop (+ c 1)))
	      (else (loop c)))))))

;; TODO expand it when the performance gets an issue.
(define (hash-table-map proc comparator ht)
  (let ((r (make-hash-table comparator)))
    (hash-table-find ht
		     (lambda (k v) (hashtable-set! r k (proc k v)) #f)
		     (lambda () r))))

(define (hash-table-map! proc ht)
  (for-each (lambda (k) (hashtable-set! ht k (proc k (hashtable-ref ht k))))
	    (hashtable-keys-list ht)))

(define (hash-table-map->list proc ht) (hashtable-map proc ht))

;; arguments can be [proc seed ht] or [ht proc seed]
;; rather stupid re-ordering, but if it is, then how it is...
(define (hash-table-fold proc seed ht)
  (if (hashtable? proc)
      (hashtable-fold seed proc ht)
      (hashtable-fold proc ht seed)))

(define (hash-table-prune! proc ht) (hashtable-prune! ht proc))

(define (hash-table-empty-copy ht) (hashtable-empty-copy ht))

;; hash-table-merge! defined in SRFI-69 should be the same as
;; this, however the SRFI-125 defines ht1 has more priority
;; whilst the SRFI-69 says nothing. so I think it's a bit of
;; imcompatibility, and that's why we define the new one here.
(define (hash-table-union! ht1 ht2)
  (hashtable-for-each (lambda (k v)
			(unless (hashtable-contains? ht1 k)
			  (hashtable-set! ht1 k v))) ht2)
  ht1)

(define (hash-table-intersection! ht1 ht2)
  (hash-table-prune! (lambda (k v) (not (hashtable-contains? ht2 k))) ht1)
  ht1)

(define (hash-table-difference! ht1 ht2)
  (hash-table-prune! (lambda (k v) (hashtable-contains? ht2 k)) ht1)
  ht1)

(define (hash-table-xor! ht1 ht2)
  (hashtable-for-each (lambda (k v)
			(if (hashtable-contains? ht1 k)
			    (hashtable-delete! ht1 k)
			    (hashtable-set! ht1 k v))) ht2)
  ht1)

)
