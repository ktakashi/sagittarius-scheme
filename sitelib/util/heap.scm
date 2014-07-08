;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; heap.scm - Heap data structure utilities
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

;; we provide fibonacci heap as heap, it may be better to
;; provide binary heap as well and dispatch it with generic
;; function however it costs a lot on running time.
;; NOTE: CL-HEAP is using generic function but usually CL's
;;       overhead is not so big (e.g. SBCL). If we can make
;;       its cost mush less than now we may provide more than
;;       one implementation.
(library (util heap)
    (export <heap> ;; for user extension
	    heap? make-heap alist->heap
	    heap-size heap-empty? heap-min heap-compare
	    heap-clear!
	    heap-set! heap-delete!
	    heap-extract-min! heap-decrease-key!

	    ;; slow
	    heap-search heap-update! heap-ref

	    heap-merge! merge-heaps! merge-heaps
	    copy-heap

	    ;; to access value
	    heap-entry? heap-entry-key 
	    heap-entry-value
	    heap-entry-value-set!
	    )
    (import (rnrs)
	    (clos user)
	    (srfi :1 lists)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius control))

  ;; double-linked list entry
  ;; hmmm, should we export this from C side so that
  ;; it can extend?
  (define +minimum+ (list '())) ;; mark
  (define-class <entry> ()
    ((key   :init-keyword :key :reader heap-entry-key)
     (value :init-keyword :value
	    :reader heap-entry-value
	    :writer heap-entry-value-set!)  ; value can be modified
     ;; private
     next prev 
     (parent :init-value #f)
     (child :init-value #f)
     (degree :init-value 0) ;; number of children
     (marked? :init-value #f)
     ))
  ;; debug
  (define-method write-object ((e <entry>) out)
    (format out "#<entry ~s:~s>" (~ e 'key) (~ e 'value)))

  (define (heap-entry? o) (is-a? o <entry>))

  (define (make-entry key value)
    (rlet1 e (make <entry> :key key :value value)
      (set! (~ e 'next) e)
      (set! (~ e 'prev) e)))
  (define (entry-compare compare one two)
    (cond ((and one two)
	   (let ((a-key (~ one 'key))
		 (b-key (~ two 'key)))
	     (if (or (eq? a-key +minimum+) (eq? b-key +minimum+))
		 (let ((x (not (eq? a-key +minimum+)))
		       (y (not (eq? b-key +minimum+))))
		   (cond ((eq? x y) 0) ;; #f #f
			 (x 1)
			 (else -1)))
		 (compare a-key b-key))))
	  (one 1)
	  (two -1)))

  (define-class <heap> (<ordered-dictionary>)
    ;; pointer to the minimum entry
    ;; keyword argument is only for internal
    ((min :init-value #f :init-keyword :min :reader heap-min)
     (size :init-value 0 :init-keyword :size :reader heap-size)
     (compare :init-keyword :compare :reader heap-compare)))

  ;; todo from alist?
  (define (make-heap compare) (make <heap> :compare compare))
  (define (alist->heap compare alist)
    (rlet1 h (make-heap compare)
      (for-each (lambda (pv) (heap-set! h (car pv) (cdr pv))) alist)))
  (define (heap? o) (is-a? o <heap>))

  (define (heap-empty? heap) (not (~ heap 'min)))

  (define (heap-clear! heap)
    (set! (~ heap 'min) #f)
    (set! (~ heap 'size) 0)
    heap)

  (define (heap-merge! heap1 heap2)
    (define (merge-entries compare heap1 heap2)
      (define (merge1 compare heap1 heap2)
	(let1 size (+ (~ heap1 'size) (~ heap2 'size))
	  (values (merge-entry! compare (~ heap1 'min) (~ heap2 'min))
		  size)))
      (merge1 compare heap1 heap2))
    (let-values (((min size) (merge-entries (~ heap1 'compare) heap1 heap2)))
      (set! (~ heap1 'min) min)
      (set! (~ heap1 'size) size)
      ;; reset heap2
      (set! (~ heap2 'min) #f)
      (set! (~ heap2 'size) 0)
      heap1))

  ;; ONE = min
  ;; TWO = new
  ;; Initial creation:
  ;;   prev = entry = next
  ;; 
  ;; before:
  ;; +-----+      +-----+      +-----+
  ;; |     | <-P- |     | <-P- |     |
  ;; |     |      | ONE |      |     |
  ;; |     | -N-> |     | -N-> |     |
  ;; +-----+      +-----+      +-----+
  ;; 
  ;; +-----+      +-----+      +-----+
  ;; |     | <-P- |     | <-P- |     |
  ;; |     |      | TWO |      |     |
  ;; |     | -N-> |     | -N-> |     |
  ;; +-----+      +-----+      +-----+
  ;; 
  ;; after:
  ;; +-----+      +-----+      +-----+
  ;; |     | <-P- |     | \    |     | ----+  
  ;; |     |      | ONE |  \   |     |     |
  ;; |     | -N-> |     | \ P  |     | -+  |
  ;; +-----+      +-----+  N \ +-----+  |  |
  ;;                        \ \         |  |
  ;; +-----+      +-----+      +-----+  |  |
  ;; |     | <-P- |     |      |     |  |  |
  ;; |     |      | TWO |      |     |  |  |
  ;; |     | -N-> |     |      |     |  |  |
  ;; +-----+      +-----+      +-----+  |  |
  ;;                | |                 |  |
  ;;                | +-------N---------+  |
  ;;                |                      |
  ;;                +---------P------------+
  ;; 
  ;; thus (for initial one):
  ;; +-----+        +-----+       
  ;; |     | <--P-- |     | <--P----+
  ;; | ONE |        | TWO |         |
  ;; |     | --N--> |     | --N--+  |
  ;; +-----+        +-----+      |  |
  ;;   |  ^                      |  |
  ;;   |  |                      |  |
  ;;   |  +----------------------+  |
  ;;   +----------------------------+
  (define (%merge-entry! one two)
    (define (merge-them one two)      
      (let1 next (~ one 'next)
	(set! (~ one 'next) (~ two 'next))
	(set! (~ one 'next 'prev) one)
	(set! (~ two 'next) next)
	(set! (~ two 'next 'prev) two)
	(values one two)))
    (merge-them one two))

  (define (merge-entry! compare one two)
    (define (merge-them one two)
      (let-values (((one two) (%merge-entry! one two)))
	(if (negative? (entry-compare compare one two)) one two)))
    (cond ((and one two) (merge-them one two))
	  (one one)
	  (two two)
	  (else #f)))
  
  ;; to make heap intact we need to copy whole structure
  ;; of entry. this costs O(n) running time...
  ;; assume given entry is min entry means we don't have to
  ;; care about parent entry.
  (define (%copy-entry e)
    (define (m e) (make-entry (~ e 'key) (~ e 'value)))
    (define (do-child r e)
      (let1 c (~ e 'child)
	(when c
	  (let1 ne (m c)
	    (do-copy ne c (~ c 'next))
	    (set! (~ r 'child) ne)
	    (set! (~ ne 'parent) r)))))
    (define (do-copy r root e)
      (unless (eq? root e)
	(let1 ne (m e)
	  (%merge-entry! r ne)
	  (do-child ne e)
	  (do-copy r root (~ e 'next)))))
    (rlet1 r (m e)
      ;; in case
      (do-child r e)
      (do-copy r e (~ e 'next))))
  
  (define (copy-heap heap) 
    (make <heap> :min (%copy-entry (~ heap 'min)) :size (~ heap 'size)
	  :compare (~ heap 'compare)))

  (define (merge-heaps! dst heap1 . heaps)
    (define (merge-entries dst heap1 heaps)
      ;; TODO no copying
      (define (merge1 dst heap)
	(let1 copy (copy-heap heap)
	  (heap-merge! dst copy)))
      (let loop ((heaps heaps) (dst (merge1 dst heap1)))
	(if (null? heaps)
	    dst
	    (loop (cdr heaps) (merge1 dst (car heaps))))))
    (merge-entries dst heap1 heaps))

  (define (merge-heaps heap1 heap2 . heaps)
    (apply merge-heaps! (copy-heap heap1) heap2 heaps))

  (define (heap-extract-min! heap)
    (define comp (~ heap 'compare))
    (define (merge heap min-elem)
      (define (setup-table table degree)
	(if (<= (vector-length table) degree)
	    (let1 v (make-vector (+ (- degree (vector-length table)) 1) #f)
	      (vector-append table v))
	    table))

      (let loop ((to-visit (let loop ((cur (~ heap 'min)) (r '()))
			     (if (and (not (null? r))
				      (eq? (car (last-pair r)) cur))
				 (reverse! r)
				 (loop (~ cur 'next) (cons cur r)))))
		 (tree-table (vector #f)))
	(if (null? to-visit)
	    min-elem
	    (let loop2 ((cur (car to-visit)))
	      (let* ((degree (~ cur 'degree))
		     (tree-table (setup-table tree-table degree)))
		(cond ((~ tree-table degree) =>
		       (lambda (other)
			 (set! (~ tree-table degree) #f)
			 (let-values (((min max)
				       (if (< (entry-compare comp other cur) 0)
					   (values other cur)
					   (values cur other))))
			   ;;(format #t "~a:~a~%" (~ min 'key) (~ max 'key))
			   (set! (~ max 'next 'prev) (~ max 'prev))
			   (set! (~ max 'prev 'next) (~ max 'next))
			   (set! (~ max 'next) max)
			   (set! (~ max 'prev) max)
			   (set! (~ min 'child)
				 (merge-entry! comp (~ min 'child) max))
			   (set! (~ max 'parent) min)
			   (set! (~ max 'marked?) #f)
			   (set! (~ min 'degree) (+ (~ min 'degree) 1))
			   (loop2 min))))
		      (else
		       (set! (~ tree-table degree) cur)
		       (when (<= (entry-compare comp cur (~ heap 'min)) 0)
			 (set! (~ heap 'min) cur))
		       (loop (cdr to-visit) tree-table))))))))

    (when (heap-empty? heap)
      (error 'heap-extract-min! "Heap is empty" heap))
    (set! (~ heap 'size) (- (~ heap 'size) 1))
    (let1 min-elem (~ heap 'min)
      ;; discard it
      (if (eq? (~ min-elem 'next) min-elem)
	  (set! (~ heap 'min) #f)
	  (begin
	    (set! (~ min-elem 'prev 'next) (~ min-elem 'next))
	    (set! (~ min-elem 'next 'prev) (~ min-elem 'prev))
	    (set! (~ heap 'min) (~ min-elem 'next))))
      (when (~ min-elem 'child)
	(let1 cur (~ min-elem 'child)
	  (set! (~ cur 'parent) #f)
	  (let loop ((cur (~ cur 'next)))
	    (unless (eq? cur (~ min-elem 'child))
	      (set! (~ cur 'parent) #f)
	      (loop (~ cur 'next))))))

      (let1 min (merge-entry! comp (~ heap 'min) (~ min-elem 'child))
	(set! (~ heap 'min) min)
	(if (not min)
	    min-elem
	    ;; go on
	    (merge heap min-elem)))))

  (define (heap-set! heap key value)
    (rlet1 e (make-entry key value)
      (set! (~ heap 'min) (merge-entry! (~ heap 'compare) (~ heap 'min) e))
      (set! (~ heap 'size) (+ (~ heap 'size) 1))))

  ;; takes O(n)...
  (define (heap-search heap key :optional (finish values))
    (define comp (~ heap 'compare))
    (define (do-nodes node root)
      (let loop ((cur node) (first #t))
	(or (and cur (zero? (compare (~ cur 'key) key)) cur)
	    (and cur (or first (not (eq? cur node)))
		 (loop (~ cur 'next) #f))
	    ;; child if there is
	    ;; i don't think we need to traverse parent
	    ;; since we are doing from top to bottom.
	    (and-let* (( cur )
		       ( (not (eq? cur root)) )
		       (child (~ cur 'child)))
	      (loop child cur)))))
    
    ;; start with min so it must not have parent.
    (finish (do-nodes (~ heap 'min) #f)))

  (define (heap-update! heap key proc default)
    ;; a bit awkward implementation
    (define (updater e)
      (let1 e (if e e (heap-set! heap key default))
	(heap-entry-value-set! e (proc (heap-entry-value e)))
	(heap-entry-value e)))
    (heap-search heap key updater))

  (define (heap-ref heap key :optional (fallback #f))
    (heap-search heap key (lambda (e) (if e (heap-entry-value e) fallback))))

  (define (return/raise e)
    (or e (error #f "heap doesn't contain given key")))

  (define (heap-decrease-key! heap entry/key new-key)
    (define (do-decrease! heap e new-key)
      (when (positive? ((~ heap 'compare) new-key (~ e 'key)))
	(error 'heap-decrease-key! "New key exceeds old" (~ e 'key) new-key))
      (decrease-key-unchecked! heap e new-key))
    (if (is-a? entry/key <entry>)
	(do-decrease! heap entry/key new-key)
	(heap-decrease-key! heap (heap-search heap entry/key return/raise)
			    new-key)))

  (define (heap-delete! heap entry/key)
    (define (do-delete! heap e)
      (let1 okey (~ e 'key)
	(decrease-key-unchecked! heap e +minimum+)
	(rlet1 r (heap-extract-min! heap)
	  (set! (~ e 'key) okey))))
    (if (is-a? entry/key <entry>)
	(do-delete! heap entry/key)
	(heap-delete! heap (heap-search heap entry/key return/raise))))

  (define (decrease-key-unchecked! heap e new-key)
    (define comp (~ heap 'compare))
    (define (cut-node! heap e)
      (set! (~ e 'marked?) #f)
      ;; no parent, done
      (when (~ e 'parent)
	;; rewire the node's siblings around it, if it has any.
	(unless (eq? e (~ e 'next))
	  (set! (~ e 'next 'prev) (~ e 'prev))
	  (set! (~ e 'prev 'next) (~ e 'next)))
	;; if the node is one identified by its parent as its child,
	;; we need to rewrite that pointer to point to some arbitary
	;; other child.
	(when (eq? e (~ e 'parent 'child))
	  ;; if there aren't any children left and we shoul clear the
	  ;; pointer and drop the node's degree
	  (if (eq? e (~ e 'next))
	      (set! (~ e 'parent 'child) #f)
	      ;; otherwise there are any other children, pick one of them
	      ;; arbitrarily
	      (set! (~ e 'parent 'child) (~ e 'next))))
	;; decrease the degree of the parent, since it's just lost a child
	(set! (~ e 'parent 'degree) (- (~ e 'parent 'degree) 1))
	;; splice this tree into the root list by converting it to 
	;; singleton and invoking merge procedure
	(set! (~ e 'prev) e) (set! (~ e 'next) e)
	(set! (~ heap 'min) (merge-entry! comp (~ heap 'min) e))
	(if (~ e 'parent 'marked?)
	    (cut-node! heap (~ e 'parent))
	    (set! (~ e 'parent 'marked?) #t))
	(set! (~ e 'parent) #f)))

    (set! (~ e 'key) new-key)
    (when (and (~ e 'parent) (<= (entry-compare comp e (~ e 'parent)) 0))
      (cut-node! heap e))
    (when (<= (entry-compare comp e (~ heap 'min)) 0)
      (set! (~ heap 'min) e)))

)