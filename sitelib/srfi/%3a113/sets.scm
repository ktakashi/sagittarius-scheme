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
	  bag? bag-empty bag-disjoint?
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
	  bag-element-count bag-for-each-unique bag-fold-unique
	  bag-increment! bag-decrement!
	  bag->set set->bag set->bag!

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
  (define-class <bag> (<abstract-container>) ())
  (define (bag? o) (is-a? o <bag>))

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
	      (hashtable-map (lambda (k v) (and (negative? v) k)) ht)))
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
		    (define (?name stop? mapper successor sees comparator)
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
		      (hashtable-contains? (slot-ref sob 'hashtable) member))))))
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
				  ((hashtable-contains? ht (car key)) #f)
				  (else (loop (cdr keys)))))))
		      (check-type ?name a ?class)
		      (check-type ?name b ?class)
		      (check-comparator ?name sob ?class)
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
				  elements))
		      (define (?name2 sob . elements)
			(check-type ?name2 sob ?class)
			(let ((r (sob-copy sob ?class)))
			  (apply ?name sob elements)
			  r)))))))
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
			(let ((r (sob-copy sob ?class)))
			  (?name sob element)
			  r)))))))
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
			(let ((r (sob-copy sob ?class)))
			  (?name sob element)
			  r))))))
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
			(let ((r (sob-copy sob ?class)))
			  (?name sob element)
			  r)))))))
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
    (define-count bag-count set-fold <bag>))

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
    (define-any set-any <set>)
    (define-any bag-any <bag>))

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
				(else #t)))))))))
    (define-every set-every <set>)
    (define-every bag-every <bag>))

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
			 (lambda (key value) (sob-increment! r (proc key) value))
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
		    (define (?name pred  sob)
		      (check-type ?name sob ?class)
		      (hashtable-for-each
		       (lambda (key value)
			 (unless (pred key)
			   (sob-decrement! r key value)))
		       (slot-ref sob 'hashtable)))))))
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
			   (unless (pred key)
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
    (list->set! (make-sob <bag> comparator) list))

  ;; =?
  (define-syntax define-dyadic
    (syntax-rules ()
      ((_ name op)
       (define (name sob1 sob2)
	 (let ((ht1 (slot-ref sob1 'hashtable))
	       (ht2 (slot-ref sob1 'hashtable)))
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
  (define-dyadic dyadic-sob>=? >=)
  (define-dyadic dyadic-sob>? >)
  (define-dyadic dyadic-sob<? <)
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
			(=? sob1 sob2))
		       ((sob1 sob2 . sobs)
			(and (?name sob1 sob2) (apply ?name sobs)))))))))
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

  ;; TODO union, intersection and difference
  ;; TODO bag specific procedures
  
  ;; bag specific
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
    (let* ((ht (slot-ref set 'hashtable))
	   (hash (comparator-hash-function (slot-ref set 'comparator))))
      (set-fold
       (lambda (element result) (+ (hash element) (* result 33)))
       5381
       set)))
  (define (bag-hash bag)
    (let* ((ht (slot-ref bag 'hashtable))
	   (hash (comparator-hash-function (slot-ref bag 'comparator))))
      (bag-fold
       (lambda (element result) (+ (hash element) (* result 33)))
       5381
       bag)))
  (define set-comparator (make-comparator set? set=? #f set-hash))
  (define bag-comparator (make-comparator bag? bag=? #f bag-hash))
  
  )
