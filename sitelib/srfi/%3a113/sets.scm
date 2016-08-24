;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sets.scm - SRFI-113 Sets and bags
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
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


;; TODO
;;  - we may want to make this built-in object in future so that charsets
;;    can be subclass of sets
;;  - we may want to  rewrite hashtable to accept comparator (partially)
;;  - it might be better to make super layer library to export <set>
;;    and <bag> (if these are built-in then we can use (clos user) for
;;    this purpose.)
(library (srfi :113 sets)
  (export set set-contains? set-unfold
	  set? set-empty? set-disjoint?
	  set-member set-element-comparator
	  set-adjoin set-adjoin! set-replace set-replace!
	  set-delete set-delete! set-delete-all set-delete-all!
	  set-search!
	  set-size set-find set-count set-any? set-every?
	  set-map set-for-each set-fold set-filter set-filter!
	  set-remove set-remove! set-partition set-partition!
	  set-copy set->list list->set list->set!
	  set=? set<? set>? set<=? set>=?
	  set-union  set-intersection  set-difference  set-xor
	  set-union! set-intersection! set-difference! set-xor!

	  bag bag-contains? bag-unfold
	  bag? bag-empty? bag-disjoint?
	  bag-member bag-element-comparator
	  bag-adjoin bag-adjoin! bag-replace bag-replace!
	  bag-delete bag-delete! bag-delete-all bag-delete-all!
	  bag-search!
	  bag-size bag-find bag-count bag-any? bag-every?
	  bag-map bag-for-each bag-fold bag-filter bag-filter!
	  bag-remove bag-remove! bag-partition bag-partition!
	  bag-copy bag->list list->bag list->bag!
	  bag=? bag<? bag>? bag<=? bag>=?
	  bag-union  bag-intersection  bag-difference  bag-xor
	  bag-union! bag-intersection! bag-difference! bag-xor!
	  
	  bag-sum bag-sum! bag-product bag-product!
	  bag-unique-size
	  bag-element-count bag-for-each-unique bag-fold-unique
	  bag-increment! bag-decrement!
	  bag->set set->bag set->bag! bag->alist alist->bag

	  set-comparator bag-comparator)
  (import (rnrs)
	  (sagittarius)		    ;; for make-hashtable/comparator
	  (sagittarius comparators) ;; for disjoint-order
	  (sagittarius control)	    ;; for dotimes 
	  (srfi :1 lists)
	  (srfi :114 comparators)
	  (util hashtables)
	  (clos user))

  ;; See TODO
  ;; sets and bags can shere the same slots
  ;; well to me bag = multi set so can be either
  ;; super class or subclass of set however
  ;; reference implementation separates this.
  ;; so obey it
  (define-class <abstract-container> (<collection>)
    ((hashtable  :init-keyword :hashtable)
     (comparator :init-keyword :comparator)))

  ;; class definition for set and bag
  (define-class <set> (<abstract-container>) ())
  (define (set? o) (is-a? o <set>))
  (define-method write-object ((o <set>) out)
    (display "#<set" out)
    (set-for-each (lambda (e) (display " " out) (write e out)) o)
    (display ">" out))

  (define-class <bag> (<abstract-container>) ())
  (define (bag? o) (is-a? o <bag>))
  (define-method write-object ((o <bag>) out)
    (display "#<bag" out)
    (bag-for-each (lambda (e) (display " " out) (write e out)) o)
    (display ">" out))

  ;; starts with 10 for now
  (define-method disjoint-order ((o <set>)) 10)
  (define-method disjoint-order ((o <bag>)) 11)

  (define (check-element sob element)
    (comparator-check-type (slot-ref sob 'comparator) element))
  
  ;; starting here is pain in the ass...
  ;; update
  (define (sob-increment! sob element count)
    (check-element sob element)
    (hashtable-update!
     (slot-ref sob 'hashtable)
     element
     (if (bag? sob)
	 (lambda (v) (+ v count))
	 (lambda (v) 1))
     0))
  (define (sob-decrement! sob element count)
    (check-element sob element)
    (hashtable-update!
     (slot-ref sob 'hashtable)
     element
     (lambda (v) (- v count))
     0))
  (define (sob-cleanup! sob)
    (define (nonpositive-keys ht)
      (filter values
	      (hashtable-map (lambda (k v) (and (<= v 0) k)) ht)))
    (let ((ht (slot-ref sob 'hashtable)))
      (for-each (lambda (key) (hashtable-delete! ht key))
		(nonpositive-keys ht))
      sob))
  
  ;; we can't define constructors with generic macro...
  (define (make-sob class comparator)
    (make class
      :hashtable (make-hashtable/comparator comparator)
      :comparator comparator))
  
  ;; basically all of the procedures are copied however i don't
  ;; like copy&paste. so we do the same trick as reference implementation
  ;; and generates all type specific procedures with macro.
  
  ;; type check macro
  (define-syntax check-type
    (syntax-rules ()
      ((_ name obj class)
       (unless (is-a? obj class)
	 (error 'name (format "object of ~a is required" 'class) obj)))))
  (define-syntax check-comparator
    (syntax-rules ()
      ((_ name a b)
       (unless (eq? (slot-ref a 'comparator) (slot-ref b 'comparator))
	 (error 'name  "not the same comparators" a b)))))
  (define-syntax sob-copy
    (syntax-rules ()
      ((_ sob ?class)
       (make ?class
	 :hashtable (hashtable-copy (slot-ref sob 'hashtable) #t)
	 :comparator (slot-ref sob 'comparator)))))
  (define-syntax sob-empty-copy
    (syntax-rules ()
      ((_ sob ?class)
       (let ((c (slot-ref sob 'comparator)))
	 (make ?class
	   :hashtable (make-hashtable/comparator c)
	   :comparator c)))))
  ;; set & bag
  (let-syntax ((define-ctr
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name comparator . elements)
		      (let ((result (make-sob ?class comparator)))
			(for-each (lambda (x) (sob-increment! result x 1))
				  elements)
			result))))))
    (define-ctr set <set>)
    (define-ctr bag <bag>))
  ;; set-copy & bag-copy
  (let-syntax ((define-copy
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name sob)
		      (check-type ?name sob ?class)
		      (sob-copy sob ?class))))))
    (define-copy set-copy <set>)
    (define-copy bag-copy <bag>))
  ;; unfold
  (let-syntax ((define-unfold
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name stop? mapper successor seed comparator)
		      (let ((result (make-sob ?class comparator)))
			(let loop ((seed seed))
			  (if (stop? seed)
			      result
			      (begin
				(sob-increment! result (mapper seed) 1)
				(loop (successor seed)))))))))))
    (define-unfold set-unfold <set>)
    (define-unfold bag-unfold <bag>))

  ;; contains?
  (let-syntax ((define-contains
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name sob member)
		      (check-type ?name sob ?class)
		      (hashtable-contains? 
		       (slot-ref sob 'hashtable) member))))))
    (define-contains set-contains? <set>)
    (define-contains bag-contains? <bag>))

  ;; empty?
  (let-syntax ((define-empty
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name sob)
		      (check-type ?name sob ?class)
		      (= 0 (hashtable-size (slot-ref sob 'hashtable))))))))
    (define-empty set-empty? <set>)
    (define-empty bag-empty? <bag>))

  ;; disjoint?
  (let-syntax ((define-disjoint
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name a b)
		      (define (disjoint? a b)
			(let ((ht (slot-ref b 'hashtable)))
			  (let loop ((keys (hashtable-keys-list
					    (slot-ref a 'hashtable))))
			    (cond ((null? keys))
				  ((hashtable-contains? ht (car keys)) #f)
				  (else (loop (cdr keys)))))))
		      (check-type ?name a ?class)
		      (check-type ?name b ?class)
		      (check-comparator ?name a b)
		      (and (disjoint? a b)
			   (disjoint? b a)))))))
    (define-disjoint set-disjoint? <set>)
    (define-disjoint bag-disjoint? <bag>))

  ;; member
  (let-syntax ((define-member
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name sob e default)
		      (define cmp (slot-ref sob 'comparator))
		      (define (same? a b) (=? cmp a b))
		      (check-type ?name sob ?class)
		      (let ((ht (slot-ref sob 'hashtable)))
			(let loop ((keys (hashtable-keys-list ht)))
			  (cond ((null? keys) default)
				((same? (car keys) e) (car keys))
				(else (loop (cdr keys)))))))))))
    (define-member set-member <set>)
    (define-member bag-member <bag>))

  ;; element-comparator
  (let-syntax ((define-cmp
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name sob)
		      (check-type ?name sob ?class)
		      (slot-ref sob 'comparator))))))
    (define-cmp set-element-comparator <set>)
    (define-cmp bag-element-comparator <bag>))

  ;; adjoin
  (let-syntax ((define-adjoin
		 (syntax-rules ()
		   ((_ ?name ?name2 ?class)
		    (begin
		      ;; might be better to make one more layer
		      ;; to reduce apply?
		      (define (?name sob . elements)
			(check-type ?name sob ?class)
			(for-each (lambda (e) (sob-increment! sob e 1))
				  elements)
			sob)
		      (define (?name2 sob . elements)
			(check-type ?name2 sob ?class)
			(apply ?name (sob-copy sob ?class) elements)))))))
    (define-adjoin set-adjoin! set-adjoin <set>)
    (define-adjoin bag-adjoin! bag-adjoin <bag>))

  ;; replace
  (let-syntax ((define-replace
		 (syntax-rules ()
		   ((_ ?name ?name2 ?class)
		    (begin
		      ;; call/cc is expensive so don't use it
		      (define (?name sob element)
			(check-type ?name sob ?class)
			(let* ((comparator (slot-ref sob 'comparator))
			       (= (comparator-equality-predicate comparator))
			       (ht (slot-ref sob 'hashtable)))
			  (comparator-check-type comparator element)
			  (let loop ((keys (hashtable-keys-list ht)))
			    (cond ((null? keys) sob)
				  ((= (car keys) element)
				   (let ((v (hashtable-ref ht (car keys) #f)))
				     (hashtable-delete! ht (car keys))
				     (hashtable-set! ht element v)
				     sob))
				  (else (loop (cdr keys)))))))
		      (define (?name2 sob element)
			(check-type ?name2 sob ?class)
			(?name (sob-copy sob ?class) element)))))))
    (define-replace set-replace! set-replace <set>)
    (define-replace bag-replace! bag-replace <bag>))

  ;; delete & delete-all
  (let-syntax ((define-delete
		 (syntax-rules ()
		   ((_ ?name ?name2 ?class)
		    (begin
		      ;; call/cc is expensive so don't use it
		      (define (?name sob . elements)
			(check-type ?name sob ?class)
			(for-each (lambda (e) (sob-decrement! sob e 1))
				  elements)
			(sob-cleanup! sob))
		      (define (?name2 sob . elements)
			(check-type ?name2 sob ?class)
			(apply ?name (sob-copy sob ?class) elements))))))
	       (define-delete-all
		 (syntax-rules ()
		   ((_ ?name ?name2 ?class)
		    (begin
		      ;; call/cc is expensive so don't use it
		      (define (?name sob elements)
			(check-type ?name sob ?class)
			(for-each (lambda (e) (sob-decrement! sob e 1))
				  elements)
			(sob-cleanup! sob))
		      (define (?name2 sob elements)
			(check-type ?name2 sob ?class)
			(?name (sob-copy sob ?class) elements)))))))
    (define-delete set-delete! set-delete <set>)
    (define-delete bag-delete! bag-delete <bag>)
    (define-delete-all set-delete-all! set-delete-all <set>)
    (define-delete-all bag-delete-all! bag-delete-all <bag>))

  ;; search!
  (define missing (list "missing"))
  (let-syntax ((define-search
		 (syntax-rules ()
		   ((_ ?name ?member ?class)
		    (define (?name sob element failure success)
		      (define (insert obj)
			(sob-increment! sob element 1)
			(values sob obj))
		      (define (ignore obj) (values sob obj))
		      (define (update new-elem obj)
			(sob-decrement! sob element 1)
			(sob-increment! sob new-elem 1)
			(values (sob-cleanup! sob) obj))
		      (define (remove obj)
			(sob-decrement! sob element 1)
			(values (sob-cleanup! sob) obj))
		      (check-type ?name sob ?class)
		      (let ((true-element (?member sob element missing)))
			(if (eq? true-element missing)
			    (failure insert ignore)
			    (success true-element update remove))))))))
    (define-search set-search! set-member <set>)
    (define-search bag-search! bag-member <bag>))

  ;; size
  (define (set-size set)
    (check-type set-size set <set>)
    (hashtable-size (slot-ref set 'hashtable)))
  (define (bag-size bag)
    (check-type bag-size bag <bag>)
    (let ((ht (slot-ref bag 'hashtable)))
      (let loop ((vals (hashtable-values-list ht))
		 (r 0))
	(if (null? vals)
	    r
	    (loop (cdr vals) (+ (car vals) r))))))

  ;; find
  (let-syntax ((define-find
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name pred sob failure)
		      (check-type ?name sob ?class)
		      (let ((ht (slot-ref sob 'hashtable)))
			(let loop ((keys (hashtable-keys-list ht)))
			  (cond ((null? keys) (failure))
				((pred (car keys)) (car keys))
				(else (loop (cdr keys)))))))))))
    (define-find set-find <set>)
    (define-find bag-find <bag>))

  ;; count
  (let-syntax ((define-count
		 (syntax-rules ()
		   ((_ ?name ?fold ?class)
		    (define (?name pred sob)
		      (check-type ?name sob ?class)
		      (?fold (lambda (e t) (if (pred e) (+ t 1) t)) 0 sob))))))
    (define-count set-count set-fold <set>)
    (define-count bag-count bag-fold <bag>))

  ;; any
  (let-syntax ((define-any
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name pred sob)
		      (check-type ?name sob ?class)
		      (let ((ht (slot-ref sob 'hashtable)))
			(let loop ((keys (hashtable-keys-list ht)))
			  (cond ((null? keys) #f)
				;; shouldn't this return result of pred?
				((pred (car keys)) #t)
				(else (loop (cdr keys)))))))))))
    (define-any set-any? <set>)
    (define-any bag-any? <bag>))

  ;; every
  (let-syntax ((define-every
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name pred sob)
		      (check-type ?name sob ?class)
		      (let ((ht (slot-ref sob 'hashtable)))
			(let loop ((keys (hashtable-keys-list ht)))
			  (cond ((null? keys) #t)
				((pred (car keys)) (loop (cdr keys)))
				(else #f)))))))))
    (define-every set-every? <set>)
    (define-every bag-every? <bag>))

  ;; for-each
  (let-syntax ((define-for-each
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name proc sob)
		      (check-type ?name sob ?class)
		      (hashtable-for-each
		       (lambda (key value) (dotimes (i value) (proc key)))
		       (slot-ref sob 'hashtable)))))))
    (define-for-each set-for-each <set>)
    (define-for-each bag-for-each <bag>))

  ;; map
  (let-syntax ((define-map
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name proc comparator sob)
		      (check-type ?name sob ?class)
		      (let ((r (make-sob ?class comparator)))
			(hashtable-for-each
			 (lambda (key value)
			   (sob-increment! r (proc key) value))
			 (slot-ref sob 'hashtable))
			r))))))
    (define-map set-map <set>)
    (define-map bag-map <bag>))

  ;; fold
  (let-syntax ((define-fold
		 (syntax-rules ()
		   ((_ ?name ?for-each ?class)
		    (define (?name proc nil sob)
		      (check-type ?name sob ?class)
		      (let ((r nil))
			;; FIXME
			(?for-each (lambda (e) (set! r (proc e r))) sob)
			r))))))
    (define-fold set-fold set-for-each <set>)
    (define-fold bag-fold bag-for-each <bag>))

  ;; filter
  (let-syntax ((define-filter
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name pred  sob)
			(check-type ?name sob ?class)
			(let ((r (make-sob ?class (slot-ref sob 'comparator))))
			  (hashtable-for-each
			   (lambda (key value)
			     (when (pred key)
			       (sob-increment! r key value)))
			   (slot-ref sob 'hashtable))
			  r))))))
    (define-filter set-filter <set>)
    (define-filter bag-filter <bag>))
  (let-syntax ((define-filter!
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name pred sob)
		      (check-type ?name sob ?class)
		      (hashtable-for-each
		       (lambda (key value)
			 (unless (pred key)
			   (sob-decrement! sob key value)))
		       (slot-ref sob 'hashtable))
		      (sob-cleanup! sob))))))
    (define-filter! set-filter! <set>)
    (define-filter! bag-filter! <bag>))

  ;; remove
  (let-syntax ((define-remove
		 (syntax-rules ()
		   ((_ ?name ?filter ?class)
		    (define (?name pred sob)
		      (check-type ?name sob ?class)
		      (?filter (lambda (x) (not (pred x))) sob))))))
    (define-remove set-remove set-filter <set>)
    (define-remove bag-remove bag-filter <bag>)
    (define-remove set-remove! set-filter! <set>)
    (define-remove bag-remove! bag-filter! <bag>))

  ;; partition
  (let-syntax ((define-partition
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name pred  sob)
		      (check-type ?name sob ?class)
		      (let ((res1 (sob-empty-copy sob ?class))
			    (res2 (sob-empty-copy sob ?class)))
			(hashtable-for-each
			 (lambda (key value)
			   (if (pred key)
			       (sob-increment! res1 key value)
			       (sob-increment! res2 key value)))
			 (slot-ref sob 'hashtable))
			(values res1 res2)))))))
    (define-partition set-partition <set>)
    (define-partition bag-partition <bag>))
    
  (let-syntax ((define-partition
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name pred  sob)
		      (check-type ?name sob ?class)
		      (let ((result (sob-empty-copy sob ?class)))
			(hashtable-for-each
			 (lambda (key value)
			   (unless (pred key)
			     (sob-decrement! sob key value)
			     (sob-increment! result key value)))
			 (slot-ref sob 'hashtable))
			(values (sob-cleanup! sob) result)))))))
    (define-partition set-partition! <set>)
    (define-partition bag-partition! <bag>))

  ;; ->list
  (let-syntax ((define->list
		 (syntax-rules ()
		   ((_ ?name ?fold ?class)
		    (define (?name sob)
		      (check-type ?name sob ?class)
		      (?fold (lambda (e l) (cons e l)) '() sob))))))
    (define->list set->list set-fold <set>)
    (define->list bag->list bag-fold <bag>))

  ;; ->set
  (let-syntax ((define->sob
		 (syntax-rules ()
		   ((_ ?name ?class)
		    (define (?name sob lst)
		      (check-type ?name sob ?class)
		      (for-each (lambda (e) (sob-increment! sob e 1)) lst)
		      sob)))))
    (define->sob list->set! <set>)
    (define->sob list->bag! <bag>))
  (define (list->set comparator list)
    (list->set! (make-sob <set> comparator) list))
  (define (list->bag comparator list)
    (list->bag! (make-sob <bag> comparator) list))

  ;; =?
  (define-syntax define-dyadic
    (syntax-rules ()
      ((_ name op)
       (define (name sob1 sob2)
	 (let ((ht1 (slot-ref sob1 'hashtable))
	       (ht2 (slot-ref sob2 'hashtable)))
	   (and (op (hashtable-size ht1) (hashtable-size ht2))
		(let-values (((keys values) (hashtable-entries ht1)))
		  (let ((len (vector-length keys)))
		    (let loop ((i 0))
		      (or (= i len)
			  (let ((value (vector-ref values i))
				(key   (vector-ref keys i)))
			    (and (op value (hashtable-ref ht2 key 0))
				 (loop (+ i 1))))))))))))))
  (define-dyadic dyadic-sob=? =)
  (define-dyadic dyadic-sob<=? <=)
  ;; from reference implementation
  (define (dyadic-sob>? sob1 sob2) (not (dyadic-sob<=? sob1 sob2)))
  (define (dyadic-sob<? sob1 sob2) (dyadic-sob>? sob2 sob1))
  (define (dyadic-sob>=? sob1 sob2) (not (dyadic-sob<? sob1 sob2)))

  (let-syntax ((define-=?
		 (syntax-rules ()
		   ((_ ?name ?= ?class)
		    (define ?name
		      (case-lambda
		       ((sob)
			(check-type ?name sob ?class)
			#t)
		       ((sob1 sob2)
			(check-type ?name sob1 ?class)
			(check-type ?name sob2 ?class)
			(check-comparator ?name sob1 sob2)
			(?= sob1 sob2))
		       ((sob1 sob2 . sobs)
			(and (?name sob1 sob2) (apply ?name sob2 sobs)))))))))
    (define-=? set=? dyadic-sob=? <set>)
    (define-=? bag=? dyadic-sob=? <bag>)
    (define-=? set<=? dyadic-sob<=? <set>)
    (define-=? bag<=? dyadic-sob<=? <bag>)
    (define-=? set>=? dyadic-sob>=? <set>)
    (define-=? bag>=? dyadic-sob>=? <bag>)
    (define-=? set<? dyadic-sob<? <set>)
    (define-=? bag<? dyadic-sob<? <bag>)
    (define-=? set>? dyadic-sob>? <set>)
    (define-=? bag>? dyadic-sob>? <bag>)
    )

  ;; set theory operations
  (define (sob-union! result sob1 sob2)
    (let ((sob1-ht (slot-ref sob1 'hashtable))
	  (sob2-ht (slot-ref sob2 'hashtable))
	  (result-ht (slot-ref result 'hashtable)))
      (hashtable-for-each
       (lambda (key value1)
	 (let ((value2 (hashtable-ref sob2-ht key 0)))
	   (hashtable-set! result-ht key (max value1 value2))))
       sob1-ht)
      (hashtable-for-each
       (lambda (key value2)
	 (let ((value1 (hashtable-ref sob1-ht key 0)))
	   (when (zero? value1)
	     (hashtable-set! result-ht key value2))))
       sob2-ht)))

  (define (sob-intersection! result sob1 sob2)
    (let ((sob1-ht (slot-ref sob1 'hashtable))
	  (sob2-ht (slot-ref sob2 'hashtable))
	  (result-ht (slot-ref result 'hashtable)))
      (hashtable-for-each
       (lambda (key value1)
	 (let ((value2 (hashtable-ref sob2-ht key 0)))
	   (hashtable-set! result-ht key (min value1 value2))))
       sob1-ht)
      (sob-cleanup! result)))
  (define (sob-difference! result sob1 sob2)
    (let ((sob1-ht (slot-ref sob1 'hashtable))
	  (sob2-ht (slot-ref sob2 'hashtable))
	  (result-ht (slot-ref result 'hashtable)))
      (hashtable-for-each
       (lambda (key value1)
	 (let ((value2 (hashtable-ref sob2-ht key 0)))
	   (hashtable-set! result-ht key (- value1 value2))))
       sob1-ht)
      (sob-cleanup! result)))
  (let-syntax ((define-theory
		 (syntax-rules ()
		   ((_ ?name ?name2 ?class ?op)
		    (begin
		      (define (?name sob1 . sobs)
			(check-type ?name sob1 ?class)
			(for-each (lambda (sob)
				    (check-type ?name sob ?class)
				    (check-comparator ?name sob sob1))
				  sobs)
			(for-each (lambda (sob) (?op sob1 sob1 sob)) sobs)
			sob1)
		      (define (?name2 sob1 . sobs)
			(check-type ?name sob1 ?class)
			(if (null? sobs)
			    sob1
			    (let ((result (sob-empty-copy sob1 ?class)))
			      (check-type ?name (car sobs) ?class)
			      (check-comparator ?name (car sobs) sob1)
			      (?op result sob1 (car sobs))
			      (if (null? (cdr sobs))
				  result
				  (apply ?name result (cdr sobs)))))))))))
    (define-theory set-union! set-union <set> sob-union!)
    (define-theory bag-union! bag-union <bag> sob-union!)
    (define-theory set-intersection! set-intersection
      <set> sob-intersection!)
    (define-theory bag-intersection! bag-intersection
      <bag> sob-intersection!)
    (define-theory set-difference! set-difference
      <set> sob-difference!)
    (define-theory bag-difference! bag-difference
      <bag> sob-difference!))

  (define (sob-xor! result sob1 sob2)
    (let ((sob1-ht (slot-ref sob1 'hashtable))
	  (sob2-ht (slot-ref sob2 'hashtable))
	  (result-ht (if (eq? result sob1)
			 ;; if the result and first argument are the same
			 ;; object, then we need to create fresh one.
			 (make-hashtable/comparator (slot-ref sob1 'comparator))
			 (slot-ref result 'hashtable))))
      (hashtable-for-each
       (lambda (key value2)
	 (let ((value1 (hashtable-ref sob1-ht key 0)))
	   (when (zero? value1)
	     (hashtable-set! result-ht key value2))))
       sob2-ht)
      (hashtable-for-each
       (lambda (key value1)
	 (let ((value2 (hashtable-ref sob2-ht key 0)))
	   (hashtable-set! result-ht key (abs (- value1 value2)))))
       sob1-ht)
      (when (eq? result sob1)
	;; ok put all elements in result
	(let ((ht (slot-ref result 'hashtable)))
	  (hashtable-for-each
	   (lambda (key value)
	     (hashtable-set! ht key value))
	   result-ht)))
      (sob-cleanup! result)))
  ;; xor
  (let-syntax ((define-xor
		 (syntax-rules ()
		   ((_ ?name ?name2 ?class)
		    (begin
		      (define (?name sob1 sob2)
			(check-type ?name sob1 ?class)
			(check-type ?name sob2 ?class)
			(check-comparator ?name sob1 sob2)
			(sob-xor! sob1 sob1 sob2))
		      (define (?name2 sob1 sob2)
			(?name (sob-copy sob1 ?class) sob2)))))))
    (define-xor set-xor! set-xor <set>)
    (define-xor bag-xor! bag-xor <bag>))
  
  ;; bag specific
  ;; bag-sum
  (define (bag-sum! bag1 . bags)
    (define (sum! result sob1 sob2)
      (let ((sob1-ht (slot-ref sob1 'hashtable))
	    (sob2-ht (slot-ref sob2 'hashtable))
	    (result-ht (slot-ref result 'hashtable)))
	(hashtable-for-each
	 (lambda (key value1)
	   (let ((value2 (hashtable-ref sob2-ht key 0)))
	     (hashtable-set! result-ht key (+ value1 value2))))
	 sob1-ht)
	(hashtable-for-each
	 (lambda (key value2)
	   (let ((value1 (hashtable-ref sob1-ht key 0)))
	     (when (zero? value1)
	       (hashtable-set! result-ht key value2))))
	 sob2-ht)))
    (check-type bag-sum! bag1 <bag>)
    (for-each (lambda (bag) (check-type bag-sum! bag <bag>)) bags)
    (for-each (lambda (bag) (sum! bag1 bag1 bag)) bags)
    bag1)
  (define (bag-sum bag1 . bags)
    (apply bag-sum! (bag-copy bag1) bags))

  ;; bag-product
  (define (bag-product! n bag)
    (define (product! result sob n)
      (let ((rht (slot-ref result 'hashtable)))
	(hashtable-for-each
	 (lambda (elem count) (hashtable-set! rht elem (* count n)))
	 (slot-ref bag 'hashtable))
	result))
    (check-type bag-product! bag <bag>)
    (unless (and (exact? n) (integer? n) (positive? n))
      (error 'bag-product! "non negative exact integer required" n))
    (product! bag bag n))
  (define (bag-product n bag)
    (bag-product! n (bag-copy bag)))

  ;; etc
  (define (bag-unique-size bag)
    (check-type bag-unique-size bag <bag>)
    (hashtable-size (slot-ref bag 'hashtable)))
  (define (bag-element-count bag elm)
    (check-type bag-element-count bag <bag>)
    (hashtable-ref (slot-ref bag 'hashtable) elm 0))
  (define (bag-for-each-unique proc bag)
    (check-type bag-for-each-unique bag <bag>)
    (hashtable-for-each
     (lambda (key value) (proc key value))
     (slot-ref bag 'hashtable)))
  (define (bag-fold-unique proc seed bag)
    (check-type 'bag-fold-unique bag <bag>)
    (let ((ht (slot-ref bag 'hashtable)))
      (fold proc seed (hashtable-keys-list ht) (hashtable-values-list ht))))

  ;; conversion
  (define (bag->set bag)
    (check-type bag->set bag <bag>)
    (rlet1 r (make-sob <set> (slot-ref bag 'comparator))
      (hashtable-for-each
       (lambda (k v) (sob-increment! r k v))
       (slot-ref bag 'hashtable))))
  (define (set->bag! bag set)
    (check-type set->bag! bag <bag>)
    (check-type set->bag! set <set>)
    (check-comparator set->bag! bag set)
    (hashtable-for-each
     (lambda (k v) (sob-increment! bag k v))
     (slot-ref set 'hashtable))
    bag)
  (define (set->bag set)
    (set->bag! (make-sob <bag> (slot-ref set 'comparator)) set))

  (define (bag->alist bag)
    (check-type bag->alist bag <bag>)
    (bag-fold-unique acons '() bag))
  (define (alist->bag comparator alist)
    (let* ((result (bag comparator))
	   (ht (slot-ref result 'hashtable)))
      (for-each (lambda (k&v)
		  (let ((k (car k&v)))
		    (unless (hashtable-contains? ht k)
		      (sob-increment! result k (cdr k&v))))) alist)
      result))

  (define (bag-increment! bag element count)
    (check-type bag-increment! bag <bag>)
    (sob-increment! bag element count)
    bag)
  (define (bag-decrement! bag element count)
    (check-type bag-increment! bag <bag>)
    (sob-decrement! bag element count)
    (sob-cleanup! bag))

  ;; comparator
  (define (set-hash set)
    (let ((hash (comparator-hash-function (slot-ref set 'comparator))))
      (set-fold
       (lambda (element result) (bitwise-xor (hash element) result))
       5381
       set)))
  (define (bag-hash bag)
    (let ((hash (comparator-hash-function (slot-ref bag 'comparator))))
      (bag-fold
       (lambda (element result) (bitwise-xor (hash element) result))
       5381
       bag)))
  (define set-comparator (make-comparator set? set=? #f set-hash))
  (define bag-comparator (make-comparator bag? bag=? #f bag-hash))
  
  )
