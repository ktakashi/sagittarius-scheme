;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a146/hash.scm - Mappings (hashmap)
;;;
;;;   Copyright (c) 2019  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :146 hash)
    (export (rename (make-hashmap hashmap)) hashmap-unfold
	    hashmap? hashmap-contains? hashmap-empty? hashmap-disjoint?
	    hashmap-ref hashmap-ref/default hashmap-key-comparator
	    hashmap-adjoin hashmap-adjoin!
	    hashmap-set hashmap-set!
	    hashmap-replace hashmap-replace!
	    hashmap-delete hashmap-delete!
	    hashmap-delete-all hashmap-delete-all!
	    
	    hashmap-intern hashmap-intern!
	    hashmap-update hashmap-update!
	    hashmap-update/default hashmap-update!/default
	    
	    hashmap-pop hashmap-pop!
	    hashmap-search hashmap-search!
	    hashmap-size hashmap-find hashmap-count hashmap-any? hashmap-every?
	    hashmap-keys hashmap-values hashmap-entries
	    hashmap-map hashmap-map->list hashmap-for-each hashmap-fold
	    hashmap-filter hashmap-filter!
	    hashmap-remove hashmap-remove!
	    hashmap-partition hashmap-partition!
	    hashmap-copy hashmap->alist alist->hashmap alist->hashmap!
	    hashmap=? hashmap<? hashmap>? hashmap<=? hashmap>=?
	    hashmap-union hashmap-intersection hashmap-difference hashmap-xor
	    hashmap-union! hashmap-intersection! hashmap-difference!
	    hashmap-xor!
	    
	    make-hashmap-comparator
	    hashmap-comparator
	    comparator?)
    (import (rnrs)
	    (srfi :114 comparators)
	    (srfi :126)
	    (only (srfi :128) comparator-register-default!)
	    (clos user)
	    (rename (only (sagittarius) %hashtable-iter)
		    (%hashtable-iter hashtable-iterator))
	    (util hashtables))

(define-syntax dopairs
  (syntax-rules ()
    ((_ (k v lis) body ...)
     (let ((l lis))
       (do ((xs l (cddr xs)))
	   ((null? xs))
	 (when (null? (cdr xs))
	   (assertion-violation 'mapping "kv-list isn't even" l))
	 (let ((k (car xs)) (v (cadr xs)))
	   body ...))))))

(define-record-type hashmap
  (fields key-comparator table)
  (protocol (lambda (p)
	      (lambda (comparator . args)
		(let ((ht (make-hashtable/comparator comparator)))
		  (dopairs (k v args) (hashtable-adjoin! ht k v))
		  (p comparator ht))))))
(define %hash hashmap-table)

(define (hashmap-unfold p f g seed comparator)
  (assert (comparator? comparator))
  (let ((m (make-hashmap comparator)))
    (do ((seed seed (g seed)))
	((p seed) m)
      (let-values (((k v) (f seed)))
	(hashmap-adjoin! m k v)))))

(define (hashmap-empty? m) (zero? (hashtable-size (%hash m))))
(define (hashmap-contains? m k) (hashtable-contains? (%hash m) k))
(define (hashmap-disjoint? m1 m2)
  (assert (hashmap? m1))
  (assert (hashmap? m2))
  (let ((t2 (%hash m2)))
    (hashtable-seek (%hash m1)
		  (lambda (k _) (hashtable-contains? t2 k))
		  (lambda (r k v) #f)
		  (lambda () #t))))

(define (hashmap-fold kons knil m)
  (assert (hashmap? m))
  (hashtable-fold kons (%hash m) knil))

(define (hashmap-copy m)
  (assert (hashmap? m))
  (hashmap-fold (lambda (k v m) (hashmap-adjoin! m k v))
		(make-hashmap (hashmap-key-comparator m)) m))

(define (hashmap-adjoin! m . args)
  (assert (hashmap? m))
  (let ((t (%hash m))) (dopairs (k v args) (hashtable-adjoin! t k v)))
  m)

(define (hashmap-adjoin m . args)
  (assert (hashmap? m))
  (apply hashmap-adjoin! (hashmap-copy m) args))

(define %unique (list #t))

(define (hashmap-ref m key :optional (failure #f) (success values))
  (assert (hashmap? m))
  (let ((v (hashtable-ref (%hash m) key %unique)))
    (if (eq? v %unique)
	(if failure
	    (failure)
	    (error 'hashmap-ref "Given key isn't in the hashmap" m key))
	(success v))))

(define (hashmap-ref/default m key default)
  (assert (hashmap? m))
  (hashtable-ref (%hash m) key default))

(define (hashmap-set m . args)
  (assert (hashmap? m))
  (apply hashmap-set! (hashmap-copy m) args))

(define (hashmap-set! m . args)
  (assert (hashmap? m))
  (let ((t (%hash m))) (dopairs (k v args) (hashtable-set! t k v)))
  m)

(define (hashmap-replace m k v)
  (assert (hashmap? m))
  (hashmap-replace! (hashmap-copy m) k v))
(define (hashmap-replace! m k v)
  (assert (hashmap? m))
  (let ((t (%hash m))) (hashtable-replace! t k v))
  m)

(define (hashmap-delete m . keys) (hashmap-delete-all m keys))
(define (hashmap-delete! m . keys) (hashmap-delete-all! m keys))

(define (hashmap-delete-all m keys)
  (assert (hashmap? m))
  (hashmap-delete-all! (hashmap-copy m) keys))

(define (hashmap-delete-all! m keys)
  (assert (hashmap? m))
  (let ((t (%hash m)))
    (for-each (lambda (k) (hashtable-delete! t k)) keys)
    m))

(define (hashmap-intern m k newval)
  (assert (hashmap? m))
  (hashmap-intern! (hashmap-copy m) k newval))

(define (hashmap-intern! m k newval)
  (assert (hashmap? m))
  (let ((v (hashmap-ref/default m k %unique)))
    (if (eq? v %unique)
	(let ((v (newval)))
	  (hashmap-set! m k v)
	  (values m v))
	(values m v))))

(define (hashmap-update m k updater . opts)
  (assert (hashmap? m))
  (apply hashmap-update! (hashmap-copy m) k updater opts))

(define (hashmap-update! m k updater :optional
			 (failure (lambda ()
				    (error 'hashmap-update!
				     "Given hashmap doesn't have key" m k)))
			 (success values))
  (assert (hashmap? m))
  (let* ((v (hashmap-ref/default m k %unique))
	 (v1 (if (eq? v %unique)
		 (updater (failure))
		 (updater (success v)))))
    (hashmap-set! m k v1)))

(define (hashmap-update/default m k updater default)
  (hashmap-update m k updater (lambda () default)))

(define (hashmap-update!/default m k updater default)
  (hashmap-update! m k updater (lambda () default)))

(define (hashmap-pop m . opt)
  (assert (hashmap? m))
  (apply hashmap-pop! (hashmap-copy m) opt))

(define (hashmap-pop! m :optional
		      (failure
		       (lambda ()
			 (error 'hashmap-pop!
				"Can't pop from an empty map" m))))
  (assert (hashmap? m))
  (let-values (((k v found?) (hashtable-find (%hash m) (lambda (k v) #t))))
    (if found?
	(begin
	  (hashtable-delete! (%hash m) k)
	  (values m k v))
	(failure))))

(define (hashmap-search m k failure success)
  (assert (hashmap? m))
  (hashmap-search! (hashmap-copy m) k failure success))

(define (hashmap-search! m k failure success)
  (assert (hashmap? m))
  (let ((v (hashmap-ref/default m k %unique)))
    (if (eq? v %unique)
	(failure (lambda (v o) (hashmap-set! m k v) (values m o)) ;; insert
		 (lambda (o) (values m o))) ;; ignore
	(success k v
		 (lambda (k v o) (hashmap-set! m k v) (values m o)) ;; update
		 (lambda (o) (hashmap-delete! m k) (values m o)))))) ;; remove

(define (hashmap-size m)
  (assert (hashmap? m))
  (hashtable-size (%hash m)))

(define (hashmap-find pred m failure)
  (assert (hashmap? m))
  (hashtable-seek (%hash m) pred (lambda (r k v) (values k v)) failure))

(define (hashmap-count pred m)
  (assert (hashmap? m))
  (hashmap-fold (lambda (k v c) (if (pred k v) (+ c 1) c)) 0 m))

(define (hashmap-any? pred m)
  (assert (hashmap? m))
  (hashtable-seek (%hash m) pred (lambda (r k v) #t) (lambda () #f)))

(define (hashmap-every? pred m)
  (assert (hashmap? m))
  (hashtable-seek (%hash m)
		(lambda (k v) (not (pred k v)))
		(lambda (r k v) #f) (lambda () #t)))

(define (hashmap-keys m) (hashtable-keys-list (%hash m)))
(define (hashmap-values m) (hashtable-values-list (%hash m)))
(define (hashmap-entries m)
  (let ((t (%hash m)))
    (values (hashtable-keys-list t) (hashtable-values-list t))))

(define (hashmap-map proc comparator m)
  (assert (hashmap? m))
  (assert (comparator? comparator))
  (let ((r (make-hashmap comparator)))
    (hashtable-for-each (lambda (k v)
			(let-values (((k v) (proc k v)))
			  (hashmap-set! r k v))) (%hash m))
    r))

(define (hashmap-for-each proc m)
  (assert (hashmap? m))
  (hashtable-for-each proc (%hash m)))

(define (hashmap-map->list proc m)
  (assert (hashmap? m))
  (hashtable-map proc (%hash m)))

(define (hashmap-filter pred m)
  (assert (hashmap? m))
  (hashmap-filter! pred (hashmap-copy m)))

(define (hashmap-filter! pred m)
  (assert (hashmap? m))
  (let-values (((keys values) (hashmap-entries m)))
    (for-each (lambda (k v)
		(unless (pred k v)
		  (hashmap-delete! m k))) keys values))
  m)

(define (hashmap-remove pred m)
  (assert (hashmap? m))
  (hashmap-remove! pred (hashmap-copy m)))
(define (hashmap-remove! pred m)
  (assert (hashmap? m))
  (hashmap-filter! (lambda (k v) (not (pred k v))) m))

(define (hashmap-partition pred m)
  (assert (hashmap? m))
  (hashmap-partition! pred (hashmap-copy m)))

(define (hashmap-partition! pred m)
  (assert (hashmap? m))
  (let ((r (make-hashmap (hashmap-key-comparator m))))
    (let-values (((keys values) (hashmap-entries m)))
      (for-each (lambda (k v)
		  (unless (pred k v)
		    (hashmap-delete! m k)
		    (hashmap-set! r k v))) keys values))
    (values m r)))

(define (alist->hashmap comparator alist)
  (assert (comparator? comparator))
  (alist->hashmap! (make-hashmap comparator) alist))
(define (alist->hashmap! m alist)
  (assert (hashmap? m))
  (for-each (lambda (p) (hashmap-adjoin! m (car p) (cdr p))) alist)
  m)

(define (hashmap->alist m)
  (assert (hashmap? m))
  (hashmap-map->list cons m))

(define (%hashmap-compare who v=? pred ms)
  (define (hashmap-compare-as-sets m1 m2 value=?)
    (define kc1 (hashmap-key-comparator m1))
    (define kc2 (hashmap-key-comparator m2))
    (define (key-compare key1 key2) (comparator-compare kc1 key1 key2))
    (define (subset? smaller larger)
      (hashtable-seek smaller
                      (lambda (k v)
			(let ((w (hashtable-ref larger k %unique)))
                          (or (eq? %unique w)
                              (not (value=? v w)))))
                      (lambda (r k v) #f)
                      (lambda () #t)))
    (cond ((eq? m1 m2) 0)
	  ((not (eq? kc1 kc2)) #f)
	  (else
	   (let* ((h1 (%hash m1))
		  (h2 (%hash m2))
		  (n1 (hashtable-size h1))
		  (n2 (hashtable-size h2)))
	     (cond
	      ((= n1 n2) (and (subset? h1 h2) 0))
	      ((< n1 n2) (and (subset? h1 h2) -1))
	      (else      (and (subset? h2 h1) 1)))))))
  (let loop ((ms ms))
    (cond ((null? (cdr ms)))
          ((hashmap-compare-as-sets (car ms) (cadr ms) v=?)
           => (lambda (r) (and (pred r) (loop (cdr ms)))))
          (else #f))))

(define-syntax define-hashmap-compare
  (syntax-rules ()
    ((_ name op)
     (define (name vcmp m . more)
       (assert (comparator? vcmp))
       (%hashmap-compare 'name
			 (comparator-equality-predicate vcmp)
			 (lambda (x) (op x 0))
			 (cons m more))))))

(define-hashmap-compare hashmap=? =)
(define-hashmap-compare hashmap<? <)
(define-hashmap-compare hashmap<=? <=)
(define-hashmap-compare hashmap>? >)
(define-hashmap-compare hashmap>=? >=)

(define (hashmap-union! m1 . more)
  (define (union-2 m1 m2)
    (assert (hashmap? m1))
    (assert (hashmap? m2))
    (hashmap-for-each (lambda (k v) (hashmap-adjoin! m1 k v)) m2)
    m1)
  (if (null? more)
      m1
      (apply hashmap-union! (union-2 m1 (car more)) (cdr more))))
(define (hashmap-union m1 . more)
  (apply hashmap-union! (hashmap-copy m1) more))

(define (hashmap-intersection! m1 . more)
  (define (intersection-2 m1 m2)
    (assert (hashmap? m1))
    (assert (hashmap? m2))
    (for-each (lambda (k)
		(unless (hashmap-contains? m2 k)
		  (hashmap-delete! m1 k))) (hashmap-keys m1))
    m1)
  (if (null? more)
      m1
      (apply hashmap-intersection! (intersection-2 m1 (car more)) (cdr more))))
(define (hashmap-intersection m1 . more)
  (apply hashmap-intersection! (hashmap-copy m1) more))

(define (hashmap-difference! m1 . more)
  (define (difference-2 m1 m2)
    (assert (hashmap? m1))
    (assert (hashmap? m2))
    (hashmap-for-each (lambda (k v) (hashmap-delete! m1 k)) m2)
    m1)
  (if (null? more)
      m1
      (apply hashmap-difference! (difference-2 m1 (car more)) (cdr more))))
(define (hashmap-difference m1 . more)
  (apply hashmap-difference! (hashmap-copy m1) more))

(define (hashmap-xor! m1 m2)
  (assert (hashmap? m1))
  (assert (hashmap? m2))
  (hashmap-for-each (lambda (k v)
		      (if (hashmap-contains? m1 k)
			  (hashmap-delete! m1 k)
			  (hashmap-set! m1 k v))) m2)
  m1)
(define (hashmap-xor m1 m2) (hashmap-xor! (hashmap-copy m1) m2))

(define (make-hashmap-compare value-comparator)
  (define (hashmap-compare m1 m2)
    (define eof (cons #f #f))
    (define kc1 (hashmap-key-comparator m1))
    (define kc2 (hashmap-key-comparator m2))
    (define (key-compare key1 key2) (comparator-compare kc1 key1 key2))
    (unless (eq? kc1 kc2)
      (error 'hashmap-comparator "Hashmap key comparaters are not the same"
	     m1 m2))
    (cond ((eq? m1 m2) 0)
	  (else
	   (let ((i1 (hashtable-iterator (%hash m1)))
		 (i2 (hashtable-iterator (%hash m2))))
	     (define (loop k1 v1 k2 v2)
               (if (eq? k1 eof)
		   (if (eq? k2 eof) 0 -1)
		   (if (eq? k2 eof)
		       1
		       (case (key-compare k1 k2)
			 ((0)
			  (case (comparator-compare value-comparator v1 v2)
			    ((0) (let-values (((k1 v1) (i1 eof))
					      ((k2 v2) (i2 eof)))
				   (loop k1 v1 k2 v2)))
			    (else => values)))
			 (else => values)))))
	     (let-values (((k1 v1) (i1 eof))
			  ((k2 v2) (i2 eof)))
	       (loop k1 v1 k2 v2))))))
  hashmap-compare)
(define (make-hashmap-comparator value-cmpr)
  (define (combine-hash-value v1 v2)
    (bitwise-and (+ (* v1 5) v2) #xFFFFFFFF))
  (define (hash-hash m)
    (define h (%hash m))
    (let ((hasher (hashtable-hash-function h)))
      (hashtable-fold (lambda (k v s)
                        (bitwise-xor s
				     (combine-hash-value
                                      (hasher k)
                                      (comparator-hash value-cmpr v))))
                      h 0)))
  (define (hash-equal? a b)
    (and (hashmap? a)
         (hashmap? b)
         (eq? (hashmap-key-comparator a) (hashmap-key-comparator b))
         (hashmap=? value-cmpr a b)))
  (make-comparator hashmap? hash-equal? #f hash-hash))

(define hashmap-comparator (make-hashmap-comparator default-comparator))

(define default-hashmap-compare (make-hashmap-compare default-comparator))
(define-method object-compare ((m1 hashmap) (m2 hashmap))
  (default-hashmap-compare m1 m2))

;; for SRFI 128
(define dummy
  (comparator-register-default! hashmap-comparator))
)
