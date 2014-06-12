;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; deque.scm - Deque utilities
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

;; MT deque implementation is based on Gauche's <mtqueue>
(library (util deque)
    (export <deque> <mtdeque>
	    make-deque make-mtdeque
	    deque? mtdeque?
	    deque-length mtdeque-max-length mtdeque-room
	    deque-empty? copy-deque
	    deque-push! deque-push-unique!
	    deque-unshift! deque-unshift-unique! 
	    deque-pop! deque-pop-all!	  ;; from tail
	    deque-shift! deque-shift-all! ;; from head
	    deque-front deque-rear
	    deque->list list->deque
	    find-in-deque remove-from-deque!
	    any-in-deque every-in-deque

	    (rename (any-in-deque exist-in-deque)
		    (every-in-deque for-all-in-deque))

	    ;; MT deque
	    deque-unshift/wait! deque-push/wait!
	    deque-shift/wait! deque-pop/wait!)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (clos user)
	    (sagittarius)
	    (srfi :1)
	    (core base)
	    (sagittarius threads)
	    (sagittarius object)
	    (sagittarius control))

  ;; implement with linked list
  (define-class <node> ()
    ((next :init-keyword :next)
     (prev :init-keyword :prev)
     (item :init-keyword :item)))
  (define-method write-object ((n <node>) out)
    (format out "#<node ~a>" (if (slot-bound? n 'item)
				 (~ n 'item)
				 #f)))
  ;; might be a bit inefficient but safer i guess
  (define (null) (make <node>))

  ;; some convenient procedures for node
  (define (->node obj) (make <node> :item obj :prev (null) :next (null)))
  (define (kons obj nodes) 
    (rlet1 n (make <node> :item obj :next nodes)
      (set! (~ nodes 'prev) n)))
  (define (snok obj nodes) 
    (rlet1 n (make <node> :item obj :prev nodes)
      (set! (~ nodes 'next) n)))
  (define (kar nodes) (~ nodes 'item))
  (define (kdr nodes) (~ nodes 'next))
  (define (set-kdr! node1 node2)
    (set! (~ node1 'next) node2)
    (set! (~ node2 'prev) node1))
  (define next kdr)
  (define (prev node) (~ node 'prev))
  (define (null-node? node) (not (slot-bound? node 'item)))
  (define (node-reverse node)
    (let loop ((node node) (r (null)))
      (if (null-node? node)
	  r
	  (loop (kdr node) (kons (kar node) r)))))

  (define (node-reverse! node)
    (if (null-node? node)
	node
	(let loop ((first node) (r (null)))
	  (if (null-node? first)
	      r
	      (let1 next (kdr first)
		(set-kdr! first r)
		(loop next first))))))
  (define (list->nodes lst)
    (let loop ((lst lst) (nodes (null)))
      (if (null? lst)
	  (node-reverse! nodes)
	  (loop (cdr lst) (kons (car lst) nodes)))))
  (define (last-node nodes)
    (if (null-node? nodes)
	(error 'last-node "empty node is given" nodes)
	(let loop ((nodes nodes))
	  (if (null-node? (kdr nodes))
	      nodes
	      (loop (kdr nodes))))))
  (define (node-length nodes)
    (let loop ((nodes nodes) (c 0))
      (if (null-node? nodes)
	  c
	  (loop (next nodes) (+ c 1)))))
  (define (node-member obj nodes cmp)
    (let loop ((nodes nodes))
      (cond ((null-node? nodes) #f)
	    ((cmp obj (kar nodes)) nodes)
	    (else (loop (kdr nodes))))))

  (define (node->list nodes)
    (let loop ((nodes nodes) (r '()))
      (if (null-node? nodes)
	  (reverse! r)
	  (loop (kdr nodes) (cons (kar nodes) r)))))

  (define-class <deque> (<sequence>)
    ((length :init-value 0)
     (head   :init-form (null))
     (tail   :init-form (null))))
  (define (deque? o) (is-a? o <deque>))

  (define-class <mtdeque> (<deque>)
    ((max-length :init-keyword :max-length :init-value -1)
     (mutex      :init-form (make-mutex))
     (reader-wait :init-form (make-condition-variable))
     (writer-wait :init-form (make-condition-variable))
     (reader-sem  :init-value 0)
     ))
  (define (mtdeque? o) (is-a? o <mtdeque>))

  (define (make-deque) (make <deque>))
  (define (make-mtdeque :key (max-length -1)) 
    (make <mtdeque> :max-length max-length))

  ;; atomic related macro and procedures
  (define-syntax with-mtq-lock
    (syntax-rules ()
      ((_ q body ...)
       (with-locking-mutex (~ q 'mutex) (lambda () body ...)))))

  ;; we can't detect the cause...
  (define-syntax wait-cv
    (syntax-rules ()
      ((_ q slot time)
       (rlet1 r (mutex-unlock! (~ q 'mutex) (~ q slot) time)
	 ;; is this correct?
	 (mutex-lock! (~ q 'mutex))))))

  (define-syntax with-mtq-light-lock
    (syntax-rules ()
      ((_ q body ...) (with-mtq-lock q body ...))))

  ;; notify
  (define-syntax notify-writers
    (syntax-rules ()
      ((_ q)
       (condition-variable-broadcast! (~ q 'writer-wait)))))
  (define-syntax notify-readers
    (syntax-rules ()
      ((_ q)
       (condition-variable-broadcast! (~ q 'reader-wait)))))


  ;;; APIs
  (define (deque-empty? q)
    (define (rec) (null-node? (~ q 'head)))
    (if (mtdeque? q)
	(with-mtq-light-lock q (rec))
	(rec)))

  (define (mtdeque-overflows? q count)
    (and (>= (~ q 'max-length) 0)
	 (> (+ count (~ q 'length)) (~ q 'max-length))))

  (define (deque-length q) (~ q 'length))

  (define (mtdeque-max-length q)
    (let1 l (~ q 'max-length)
      (and (>= l 0) l)))
  (define (mtdeque-room q)
    (with-mtq-light-lock q
     (or (and-let* ((mx (~ q 'max-length))
		    ( (>= mx 0) )
		    (r (- mx (~ q 'length)))
		    ( (>= r 0)))
	   r)
	 +inf.0)))

  (define (%deque-set-content! q lst)
    (let1 l (length lst)
      (when (negative? l)
	(assertion-violation 'list->deque
			     (wrong-type-argument-message "proper list" lst 2)
			     lst))
      (let1 nodes (list->nodes lst)
	(set! (~ q 'length) l)
	(set! (~ q 'head) nodes)
	(set! (~ q 'tail) (if (zero? l) (null) (last-node nodes))))))
  (define (%deque-set-content-node! q nodes)
    (let1 l (node-length nodes)
      (set! (~ q 'length) l)
      (set! (~ q 'head) nodes)
      (set! (~ q 'tail) (if (zero? l) (null) (last-node nodes)))))

  (define (list->deque lst :optional (class <deque>) :rest initargs)
    (rlet1 q (apply make class initargs)
      (%deque-set-content! q lst)))
  (define-method copy-deque ((q <deque>))
    (list->deque (deque->list q) (class-of q)))
  (define-method copy-deque ((q <mtdeque>))
    (list->deque (deque->list q) (class-of q) 
		 :max-length (mtdeque-max-length q)))

  (define (%deque-peek q head? :optional fallback)
    (define (rec)
      (if (not (null-node? (~ q 'head)))
	  (values #f (kar (~ q (if head? 'head 'tail))))
	  (values #t #f)))
    (let-values (((empty? r) (if (mtdeque? q)
				 (with-mtq-light-lock q (rec))
				 (rec))))
      (cond ((not empty?) r)
	    ((undefined? fallback)
	     (error 'deque-peek "deque is empty" q))
	    (else fallback))))

  (define deque-front
    (case-lambda
     ((q) (%deque-peek q #t))
     ((q default) (%deque-peek q #t default))))
  (define deque-rear
    (case-lambda
     ((q) (%deque-peek q #f))
     ((q default) (%deque-peek q #f default))))

  (define-syntax deque-op
    (syntax-rules ()
      ((_ who q proc)
       (cond ((mtdeque? q)
	      ;;(%lock-mtq q) (unwind-protect (proc #t) (%unlock-mtq q))
	      (with-mtq-light-lock q (proc #t))
	      )
	     ((deque? q) (proc #f))
	     (else
	      (assertion-violation who 
				   (wrong-type-argument-message "deque" q)
				   q))))))

  (define (deque->list q) 
    (deque-op 'deque->list q (lambda (_) (node->list (~ q 'head)))))
  ;; TODO it's inefficient
  (define (find-in-deque pred q)
    (deque-op 'find-in-deque q 
	      (lambda (_) (find pred (node->list (~ q 'head))))))
  (define (any-in-deque pred q)
    (deque-op 'any-in-deque q 
	      (lambda (_) (any pred (node->list (~ q 'head))))))
  (define (every-in-deque pred q)
    (deque-op 'every-in-deque q 
	      (lambda (_) (every pred (node->list (~ q 'head))))))

  (define-syntax q-write-op
    (syntax-rules ()
      ((_ who op q count head tail)
       (if (mtdeque? q)
	   (with-mtq-light-lock q
	    (cond ((mtdeque-overflows? q count)
		   (error who "deque is full" q))
		  (else (op q count head tail) (notify-readers q) q)))
	   (begin (op q count head tail) q)))))
  (define (%deque-push! q count head tail)
    (set! (~ q 'length) (+ (~ q 'length) count))
    (cond ((null-node? (~ q 'head))
	   (set! (~ q 'head) head)
	   (set! (~ q 'tail) tail))
	  (else
	   (set-kdr! (~ q 'tail) head)
	   (set! (~ q 'tail) tail))))
  (define (deque-push! q obj . more)
    (let* ((more (list->nodes more))
	   (head (kons obj more)))
      (let-values (((tail count) (if (null-node? more) 
				     (values head 1)
				     (values (last-node more)
					     (node-length head)))))
	(q-write-op 'deque-push! %deque-push! q count head tail))))

  (define-syntax do-with-timeout
    (syntax-rules ()
      ((_ q timeout timeout-val slot init wait-check do-ok)
       (with-mtq-lock q
	 init ;; we can't handle intr signal...
	 (let loop ()
	   (define (finish r) r)
	   (cond (wait-check 
		  (if (wait-cv q slot timeout) 
		      (loop)
		      (finish timeout-val)))
		 (else (finish do-ok))))))))
  (define (deque-push/wait! q obj :optional (timeout #f) (timeout-val #f))
    (let ((cell (->node obj)))
      (do-with-timeout q timeout timeout-val 'writer-wait
		       (begin)
		       (if (not (zero? (~ q 'max-length)))
			   (mtdeque-overflows? q 1)
			   (zero? (~ q 'reader-sem)))
		       (begin
			 (%deque-push! q 1 cell cell)
			 (notify-readers q)
			 #t))))

  (define (deque-push-unique! q cmp obj . more-obj)
    (define (pick lst xs ins)
      (cond ((null-node? ins) xs)
	    ((or (node-member (kar ins) lst cmp)
		 (node-member (kar ins) xs cmp))
	     (pick lst xs (kdr ins)))
	    (else (pick lst (kons (kar ins) xs) (kdr ins)))))
    (deque-op 'deque-push-unique! q
	      (lambda (mt?)
		(let1 xs (pick (~ q 'head) (null) 
			       (kons obj (list->nodes more-obj)))
		  (unless (null-node? xs)
		    (when (and mt? (mtdeque-overflows? q (node-length xs)))
		      (error 'deque-push-unique! "deque is full" q))
		    (let1 xs_ (node-reverse xs)
		      (%deque-push! q (node-length xs) xs_ (last-node xs_))
		      (when mt? (notify-readers q))))
		  q))))

  (define (%deque-unshift! q count head tail)
    (set-kdr! tail (~ q 'head))
    (set! (~ q 'head) head)
    (set! (~ q 'tail) (last-node tail))
    (set! (~ q 'length) (+ (~ q 'length) count)))

  (define (deque-unshift! q obj . more)
    (let ((objs (kons obj (list->nodes more))))
      (let-values (((h t c) 
		    (if (null? more)
			(values objs objs 1)
			(let ((h (node-reverse! objs)))
			  (values h (last-node h) (node-length h))))))
	(q-write-op 'deque-unshift! %deque-unshift! q c h t))))
  (define (deque-unshift/wait! q obj :optional (timeout #f) (timeout-val #f))
    (let ((cell (->node obj)))
      (do-with-timeout q timeout timeout-val 'writer-wait
		       (begin)
		       (if (not (zero? (~ q 'max-length)))
			   (mtdeque-overflows? q 1)
			   (zero? (~ q 'reader-sem)))
		       (begin
			 (%deque-unshift! q 1 cell cell)
			 (notify-readers q)
			 #t))))
  (define (deque-unshift-unique! q cmp obj . more-obj)
    (define (pick lst ins)
      (cond ((null-node? ins) lst)
	    ((node-member (kar ins) lst cmp) (pick lst (kdr ins)))
	    (else (pick (kons (kar ins) lst) (kdr ins)))))
    (deque-op 'deque-unshift-unique! q
	      (lambda (mt?)
		(let* ((h (~ q 'head))
		       (xs (pick h (kons obj (list->nodes more-obj)))))
		  (unless (eq? xs h)
		    (when (and mt? 
			       (mtdeque-overflows? 
				q (- (node-length xs) (node-length h))))
		      (error 'deque-unshift-unique! "deque is full" q))
		    (%deque-set-content-node! q xs)
		    (when mt? (notify-readers q)))
		  q))))

  (define (%deque-shift q)
    (cond ((null-node? (~ q 'head)) (values #t #f))
	  (else
	   (let1 r (kar (~ q 'head))
	     (set! (~ q 'head) (kdr (~ q 'head)))
	     (set! (~ q 'length) (- (~ q 'length) 1))
	     (values #f r)))))
  (define (deque-shift! q :optional fallback)
    (let-values (((empty? head)
		  (if (mtdeque? q)
		      (with-mtq-light-lock q (%deque-shift q))
		      (%deque-shift q))))
      (if empty?
	  (if (undefined? fallback)
	      (error 'deque-shift! "deque is empty" q)
	      fallback)
	  (begin
	    (when (mtdeque? q) (notify-writers q))
	    head))))
  (define (deque-shift/wait! q :optional (timeout #f) (timeout-val #f))
    (do-with-timeout q timeout timeout-val 'reader-wait
		       (begin
			 (set! (~ q 'reader-sem) (+ (~ q 'reader-sem) 1))
			 (notify-writers q))
		       (null-node? (~ q 'head))
		       (begin
			 (set! (~ q 'reader-sem) (- (~ q 'reader-sem) 1))
			 (let-values (((empty? r) (%deque-shift q)))
			   (notify-writers q)
			   r))))
  
  (define (deque-shift-all! q)
    (define (int)
      (rlet1 h (node->list (~ q 'head))
	(set! (~ q 'length) 0)
	(set! (~ q 'head) (null))
	(set! (~ q 'tail) (null))))
    (if (mtdeque? q)
	(with-mtq-light-lock q (rlet1 r (int) (notify-writers q)))
	(int)))

  ;; remove the last
  (define (%deque-pop q)
    (cond ((null-node? (~ q 'head)) (values #t #f))
	  (else
	   (let1 r (kar (~ q 'tail))
	     (set! (~ q 'tail) (prev (~ q 'tail)))
	     (set-kdr! (~ q 'tail) (null))
	     (set! (~ q 'length) (- (~ q 'length) 1))
	     (values #f r)))))  
  (define (deque-pop! q :optional fallback)
    (let-values (((empty? head)
		  (if (mtdeque? q)
		      (with-mtq-light-lock q (%deque-pop q))
		      (%deque-pop q))))
      (if empty?
	  (if (undefined? fallback)
	      (error 'deque-shift! "deque is empty" q)
	      fallback)
	  (begin
	    (when (mtdeque? q) (notify-writers q))
	    head))))
  (define (deque-pop/wait! q :optional (timeout #f) (timeout-val #f))
    (do-with-timeout q timeout timeout-val 'reader-wait
		     (begin
		       (set! (~ q 'reader-sem) (+ (~ q 'reader-sem) 1))
		       (notify-writers q))
		     (null-node? (~ q 'head))
		     (begin
		       (set! (~ q 'reader-sem) (- (~ q 'reader-sem) 1))
		       (let-values (((empty? r) (%deque-pop q)))
			 (notify-writers q)
			 r))))
  ;; should we do like this?
  (define (deque-pop-all! q) (reverse! (deque-shift-all! q)))

  (define (remove-from-deque! pred q)
    (deque-op 'remove-from-deque! q
	      (lambda (mt?)
		(let loop ((rs '()) (xs (~ q 'head)) (hit #f))
		  (cond ((null-node? xs)
			 (when hit
			   (when mt? (notify-writers q))
			   (%deque-set-content! q (reverse! rs)))
			 hit)
			((pred (kar xs)) (loop rs (kdr xs) #t))
			(else (loop (cons (kar xs) rs) (kdr xs) hit)))))))

  )