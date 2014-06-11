;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; queue.scm - Queue utilities
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

;; API names are taken from Gauche and slib
;; Most of the implementation details are from Gauche
;; it's just ported for Sagittarius written in Scheme
(library (util queue)
    (export <queue> <mtqueue>
	    make-queue make-mtqueue
	    queue? mtqueue?
	    queue-length mtqueue-max-length mtqueue-room
	    queue-empty? copy-queue
	    queue-push! queue-push-unique!
	    enqueue! enqueue-unique!
	    queue-pop! dequeue! dequeue-all!
	    queue-front queue-rear
	    queue->list list->queue
	    find-in-queue remove-from-queue!
	    any-in-queue every-in-queue

	    ;; MT queue
	    enqueue/wait! queue-push/wait!
	    dequeue/wait! queue-pop/wait!)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (clos user)
	    (sagittarius)
	    (srfi :1)
	    (core base)
	    (sagittarius threads)
	    (sagittarius object)
	    (sagittarius control))

  (define-class <queue> (<sequence>)
    ((length :init-value 0)
     (head   :init-value '())
     (tail   :init-value '())))
  (define (queue? o) (is-a? o <queue>))

  (define-class <mtqueue> (<queue>)
    ((max-length :init-keyword :max-length :init-value -1)
     (mutex      :init-form (make-mutex))
     (locker     :init-value #f)
     (lock-wait  :init-form (make-condition-variable))
     (reader-wait :init-form (make-condition-variable))
     (writer-wait :init-form (make-condition-variable))
     (reader-sem  :init-value 0)))
  (define (mtqueue? o) (is-a? o <mtqueue>))

  (define (make-queue) (make <queue>))
  (define (make-mtqueue :key (max-length -1)) 
    (make <mtqueue> :max-length max-length))

  ;; atomic related macro and procedures
  (define-syntax with-mtq-lock
    (syntax-rules ()
      ((_ q body ...)
       (with-locking-mutex (~ q 'mutex) (lambda () body ...)))))

  (define-syntax big-locked?
    (syntax-rules ()
      ((_ q)
       (and (thread? (~ q 'locker))
	    (not (eq? (thread-state (~ q 'locker)) 'terminated))))))

  (define-syntax wait-mtq-big-lock
    (syntax-rules ()
      ((_ q)
       (let loop ()
	 (when (big-locked? q)
	   ;; correct?
	   (mutex-lock! (~ q 'mutex))
	   (mutex-unlock! (~ q 'mutex) (~ q 'lock-wait))
	   (loop))))))

  (define-syntax with-mtq-light-lock
    (syntax-rules ()
      ((_ q body ...)
       (with-mtq-lock q (wait-mtq-big-lock q) body ...))))

  ;; notify
  (define-syntax notify-lockers
    (syntax-rules ()
      ((_ q) (condition-variable-broadcast! (~ q 'lock-wait)))))
  (define-syntax notify-writers
    (syntax-rules ()
      ((_ q) (condition-variable-broadcast! (~ q 'writer-wait)))))
  (define-syntax notify-readers
    (syntax-rules ()
      ((_ q) (condition-variable-broadcast! (~ q 'reader-wait)))))

  (define-syntax grap-mtq-big-lock
    (syntax-rules ()
      ((_ q)
       (with-mtq-light-lock q (set! (~ q 'locker) (current-thread))))))
  (define-syntax release-mtq-big-lock
    (syntax-rules ()
      ((_ q)
       (with-mtq-lock q (set! (~ q 'locker) #f) (notify-lockers q)))))

  ;; we can't detect the cause...
  (define-syntax wait-cv
    (syntax-rules ()
      ((_ q slot time)
       (begin
	 ;; again is this correct?
	 (mutex-lock! (~ q 'mutex))
	 (mutex-unlock! (~ q 'mutex) (~ q slot) time)))))

  (define (%lock-mtq q) (grap-mtq-big-lock q))
  (define (%unlock-mtq q) (release-mtq-big-lock q))

  (define (queue-empty? q)
    (define (rec) (null? (~ q 'head)))
    (if (mtqueue? q)
	(with-mtq-light-lock q (rec))
	(rec)))

  (define (mtqueue-overflows? q count)
    (and (>= (~ q 'max-length) 0)
	 (> (+ count (~ q 'length)) (~ q 'max-length))))

  (define (queue-length q) (~ q 'length))

  (define (mtqueue-max-length q)
    (let1 l (~ q 'max-length)
      (and (>= l 0) l)))
  (define (mtqueue-room q)
    (with-mtq-light-lock q
     (or (and-let* ((mx (~ q 'max-length))
		    ( (>= mx 0) )
		    (r (- mx (~ q 'length)))
		    ( (>= r 0)))
	   r)
	 +inf.0)))

  (define (%queue-set-content! q lst)
    (let ((l (length lst)))
      (when (negative? l)
	(assertion-violation 'list->queue
			     (wrong-type-argument-message "proper list" lst 2)
			     lst))
      (set! (~ q 'length) l)
      (set! (~ q 'head) lst)
      (set! (~ q 'tail) (if (zero? l) '() (last-pair lst)))))

  (define (list->queue lst :optional (class <queue>) :rest initargs)
    (rlet1 q (apply make class initargs)
      (%queue-set-content! q lst)))
  (define-method copy-queue ((q <queue>))
    (list->queue (queue->list q) (class-of q)))
  (define-method copy-queue ((q <mtqueue>))
    (list->queue (queue->list q) (class-of q) 
		 :max-length (mtqueue-max-length q)))

  (define (%queue-peek q head? :optional fallback)
    (define (rec)
      (or (and (not (null? (~ q 'head)))
	       (car (~ q (if head? 'head 'tail))))
	  #f))
    (let1 r (if (mtqueue? q)
		(with-mtq-light-lock q (rec))
		(rec))
      (cond (r)
	    ((undefined? fallback)
	     (error 'queue-peek "queue is empty" q))
	    (else fallback))))

  (define queue-front
    (case-lambda
     ((q) (%queue-peek q #t))
     ((q default) (%queue-peek q #t default))))
  (define queue-rear
    (case-lambda
     ((q) (%queue-peek q #f))
     ((q default) (%queue-peek q #f default))))

  (define-syntax queue-op
    (syntax-rules ()
      ((_ who q proc)
       (cond ((mtqueue? q)
	      (%lock-mtq q) (unwind-protect (proc #t) (%unlock-mtq q)))
	     ((queue? q) (proc #f))
	     (else
	      (assertion-violation who 
				   (wrong-type-argument-message "queue" q)
				   q))))))

  (define (queue->list q) 
    (queue-op 'queue->list q (lambda (_) (list-copy (~ q 'head)))))
  (define (find-in-queue pred q)
    (queue-op 'find-in-queue q (lambda (_) (find pred (~ q 'head)))))
  (define (any-in-queue pred q)
    (queue-op 'any-in-queue q (lambda (_) (any pred (~ q 'head)))))
  (define (every-in-queue pred q)
    (queue-op 'every-in-queue q (lambda (_) (every pred (~ q 'head)))))

  (define-syntax q-write-op
    (syntax-rules ()
      ((_ who op q count head tail)
       (if (mtqueue? q)
	   (with-mtq-light-lock q
	    (cond ((mtqueue-overflows? q count)
		   (error who "queue is full" q))
		  (else (op q count head tail) (notify-readers q))))
	   (op q count head tail)))))
  (define (%enqueue! q count head tail)
    (set! (~ q 'length) (+ (~ q 'length) count))
    (cond ((null? (~ q 'head))
	   (set! (~ q 'head) head)
	   (set! (~ q 'tail) tail))
	  (else
	   (set! (cdr (~ q 'tail)) head)
	   (set! (~ q 'tail) tail))))
  (define (enqueue! q obj . more)
    (let ((head (cons obj more)))
      (let-values (((tail count) (if (null? more) 
				     (values head 1)
				     (values (last-pair more) (length head)))))
	(q-write-op 'enqueue! %enqueue! q count head tail)
	q)))

  (define-syntax do-with-timeout
    (syntax-rules ()
      ((_ q timeout timeout-val slot init wait-check do-ok)
       (with-mtq-lock q
	 init ;; we can't handle intr signal...
	 (let loop ()
	   (wait-mtq-big-lock q)
	   (cond (wait-check 
		  (if (wait-cv q slot timeout) 
		      (loop)
		      timeout-val))
		 (else do-ok
		       (set! (~ q 'locker) #f)
		       (notify-lockers q))))))))
  (define (enqueue/wait! q obj :optional (timeout #f) (timeout-val #f))
    (let ((cell (list obj)))
      (do-with-timeout q timeout timeout-val 'writer-wait
		       (begin)
		       (if (not (zero? (~ q 'max-length)))
			   (mtqueue-overflows? q 1)
			   (zero? (~ q 'reader-sem)))
		       (begin
			 (%enqueue! q 1 cell cell)
			 (notify-readers q)
			 #t))))

  (define (enqueue-unique! q cmp obj . more-obj)
    (define (pick lst xs ins)
      (cond ((null? ins) xs)
	    ((or (member (car ins) lst cmp)
		 (member (car ins) xs cmp))
	     (pick lst xs (cdr ins)))
	    (else (pick lst (cons (car ins) xs) (cdr ins)))))
    (queue-op 'enqueue-unique! q
	      (lambda (mt?)
		(let1 xs (pick (~ q 'head) '() (cons obj more-obj))
		  (unless (null? xs)
		    (when (and mt? (mtqueue-overflows? q (length xs)))
		      (error 'enqueue-unique! "queue is full" q))
		    (let1 xs_ (reverse xs)
		      (%enqueue! q (length xs) xs_ (last-pair xs_))
		      (when mt? (notify-readers q)))))))
    q)

  (define (%queue-push! q count head tail)
    (set-cdr! tail (~ q 'head))
    (set! (~ q 'head) head)
    (set! (~ q 'tail) (last-pair tail))
    (set! (~ q 'length) (+ (~ q 'length) count)))

  (define (queue-push! q obj . more)
    (let ((objs (cons obj more)))
      (let-values (((h t c) 
		    (if (null? more)
			(values objs objs 1)
			(let ((h (reverse! objs)))
			  (values h (last-pair h) (length h))))))
	(q-write-op 'queue-push! %queue-push! q c h t))
      q))
  (define (queue-push/wait! q obj :optional (timeout #f) (timeout-val #f))
    (let ((cell (list obj)))
      (do-with-timeout q timeout timeout-val 'writer-wait
		       (begin)
		       (if (not (zero? (~ q 'max-length)))
			   (mtqueue-overflows? q 1)
			   (zero? (~ q 'reader-sem)))
		       (begin
			 (%queue-push! q 1 cell cell)
			 (notify-readers q)
			 #t))))
  (define (queue-push-unique! q cmp obj . more-obj)
    (define (pick lst ins)
      (cond ((null? ins) lst)
	    ((member (car ins) lst cmp) (pick lst (cdr ins)))
	    (else (pick (cons (car ins) lst) (cdr ins)))))
    (queue-op 'enqueue-unique! q
	      (lambda (mt?)
		(let* ((h (~ q 'head))
		       (xs (pick h (cons obj more-obj))))
		  (unless (eq? xs h)
		    (when (and mt? 
			       (mtqueue-overflows? q (- (length xs) (length h))))
		      (error 'enqueue-unique! "queue is full" q))
		    (%queue-set-content! q xs)
		    (when mt? (notify-readers q))))))
    q)

  (define (%dequeue q)
    (cond ((null? (~ q 'head)) (values #t #f))
	  (else
	   (let1 r (car (~ q 'head))
	     (set! (~ q 'head) (cdr (~ q 'head)))
	     (set! (~ q 'length) (- (~ q 'length) 1))
	     (values #f r)))))
  (define (dequeue! q :optional fallback)
    (let-values (((empty? head)
		  (if (mtqueue? q)
		      (with-mtq-light-lock q (%dequeue q))
		      (%dequeue q))))
      (if empty?
	  (if (undefined? fallback)
	      (error 'dequeue! "queue is empty" q)
	      fallback)
	  (begin
	    (when (mtqueue? q) (notify-writers q))
	    head))))
  (define (dequeue/wait! q :optional (timeout #f) (timeout-val #f))
    (do-with-timeout q timeout timeout-val 'reader-wait
		       (begin
			 (set! (~ q 'reader-sem) (+ (~ q 'reader-sem) 1))
			 (notify-writers q))
		       (queue-empty? q)
		       (begin
			 (set! (~ q 'reader-sem) (- (~ q 'reader-sem) 1))
			 (let-values (((empty? r) (%dequeue q 1 cell cell)))
			   (notify-writers q)
			   r))))
  
  (define (dequeue-all! q)
    (define (int)
      (rlet1 h (~ q 'head)
	(set! (~ q 'length) 0)
	(set! (~ q 'head) '())
	(set! (~ q 'tail) '())))
    (if (mtqueue? q)
	(with-mtq-light-lock q (rlet1 r (int) (notify-writers q)))
	(int)))

  (define queue-pop! dequeue!)
  (define queue-pop/wait! dequeue/wait!)

  (define (remove-from-queue! pred q)
    (rlet1 removed? #f
      (queue-op 'remove-from-queue! q
		(lambda (mt?)
		  (let loop ((rs '()) (xs (~ q 'head)) (hit #f))
		    (cond ((null? xs)
			   (when hit
			     (set! removed? #t)
			     (when mt? (notify-writers q))
			     (%queue-set-content! q (reverse! rs))))
			  ((pred (car xs)) (loop rs (cdr xs) #t))
			  (else (loop (cons (car xs) rs) (cdr xs) hit))))))))

  ;; TODO deque
  )