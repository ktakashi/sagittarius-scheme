;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; util/concurrent/shared-queue.scm - Shared queue
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

;; reference
;;  http://parlab.eecs.berkeley.edu/wiki/_media/patterns/sharedqueue.pdf
;; based on SharedQueue4

;; Sagittarius has mtqueue in (util queue) but for portability
(library (util concurrent shared-queue)
    (export shared-queue? make-shared-queue <shared-queue>
	    shared-queue-empty? shared-queue-size
	    shared-queue-max-length
	    shared-queue-overflows?
	    shared-queue-put! shared-queue-get!
	    shared-queue-clear!

	    ;; shared-priority-queue
	    ;; even thought the name is queue but it's not a
	    ;; sub class of shared-queue.
	    shared-priority-queue? make-shared-priority-queue
	    <shared-priority-queue>
	    shared-priority-queue-empty? shared-priority-queue-size
	    shared-priority-queue-put! shared-priority-queue-get!
	    shared-priority-queue-remove!
	    shared-priority-queue-clear!
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (srfi :18))

  (define-record-type (<shared-queue> make-shared-queue shared-queue?)
    (fields (mutable head shared-queue-head shared-queue-head-set!)
	    (mutable tail shared-queue-tail shared-queue-tail-set!)
	    ;; actually we just need do (length head) but takes O(n)
	    ;; better to have O(1).
	    (mutable size shared-queue-size shared-queue-size-set!)
	    ;; should a shared queue be expandable?
	    (immutable max-length shared-queue-max-length)
	    ;; synchronisation stuff
	    (mutable w %w %w-set!)
	    (immutable lock %lock)
	    (immutable read-cv %read-cv)
	    (immutable write-cv %write-cv)
	    )
    (protocol 
     (lambda (p)
       (lambda maybe-max
	 (let ((max-length (if (pair? maybe-max) (car maybe-max) -1)))
	   (unless (integer? max-length)
	     (assertion-violation 'make-shared-queue
				  "max-length must be an integer" max-length))
	   (p '() '() 0 max-length 0 (make-mutex) 
	      (make-condition-variable) (make-condition-variable)))))))

  (define (shared-queue-empty? sq) (null? (shared-queue-head sq)))

  (define (shared-queue-get! sq . maybe-timeout)
    (let ((timeout (if (pair? maybe-timeout) (car maybe-timeout) #f))
	  (timeout-value (if (and (pair? maybe-timeout)
				  (pair? (cdr maybe-timeout)))
			     (cadr maybe-timeout)
			     #f)))
      (mutex-lock! (%lock sq))
      (%w-set! sq (+ (%w sq) 1))
      (condition-variable-broadcast! (%write-cv sq))
      (let loop ()
	(cond ((shared-queue-empty? sq)
	       (cond ((mutex-unlock! (%lock sq) (%read-cv sq) timeout)
		      (mutex-lock! (%lock sq))
		      (loop))
		     (else
		      ;; do we need to reduce this?
		      (%w-set! sq (- (%w sq) 1))
		      timeout-value)))
	      (else
	       (%w-set! sq (- (%w sq) 1))
	       (let ((head (shared-queue-head sq)))
		 (shared-queue-head-set! sq (cdr head))
		 (when (null? (cdr head))
		   (shared-queue-tail-set! sq '()))
		 (shared-queue-size-set! sq (- (shared-queue-size sq) 1))
		 (mutex-unlock! (%lock sq))
		 (car head)))))))
  
  (define (shared-queue-put! sq obj . maybe-timeout)
    (let ((timeout (if (pair? maybe-timeout) (car maybe-timeout) #f))
	  (timeout-value (if (and (pair? maybe-timeout)
				  (pair? (cdr maybe-timeout)))
			     (cadr maybe-timeout)
			     #f)))
      (mutex-lock! (%lock sq))
      (let loop ()
	(cond ((if (zero? (shared-queue-max-length sq))
		   (zero? (%w sq))
		   (shared-queue-overflows? sq 1))
	       (cond ((mutex-unlock! (%lock sq) (%write-cv sq) timeout)
		      (mutex-lock! (%lock sq))
		      (loop))
		     (else timeout-value)))
	      (else
	       (let ((new (list obj))
		     (tail (shared-queue-tail sq)))
		 (shared-queue-tail-set! sq new)
		 (if (pair? tail)
		     (set-cdr! tail new)
		     (shared-queue-head-set! sq new))
		 (shared-queue-size-set! sq (+ (shared-queue-size sq) 1))
		 (when (> (%w sq) 0)
		   (condition-variable-broadcast! (%read-cv sq)))
		 (mutex-unlock! (%lock sq))
		 obj))))))

  (define (shared-queue-overflows? sq count)
    (and (>= (shared-queue-max-length sq) 0)
	 (> (+ count (shared-queue-size sq)) (shared-queue-max-length sq))))

  (define (shared-queue-clear! sq)
    (mutex-lock! (%lock sq))
    (shared-queue-size-set! sq 0)
    (shared-queue-head-set! sq '())
    (shared-queue-tail-set! sq '())
    (mutex-unlock! (%lock sq)))

  ;; priority queue
  ;; we do simply B-tree
  (define-record-type 
    (<shared-priority-queue> make-shared-priority-queue shared-priority-queue?)
    (fields (mutable elements %spq-es %spq-es-set!)
	    (mutable size shared-priority-queue-size %spq-size-set!)
	    ;; procedure return -1, 0 and 1
	    (immutable compare shared-priority-queue-compare)
	    ;; synchronisation stuff
	    (mutable w %spq-w %spq-w-set!)
	    (immutable lock %spq-lock)
	    (immutable cv %spq-cv)
	    )
    (protocol 
     (lambda (p)
       (lambda (compare . maybe-capacity)
	 (let ((capacity (if (pair? maybe-capacity) (car maybe-capacity) 10)))
	   (unless (integer? capacity)
	     (assertion-violation 'make-shared-priority-queue
				  "capacity must be an integer" capacity))
	   (p (make-vector capacity) 0 compare 0 (make-mutex) 
	      (make-condition-variable)))))))

  (define (shared-priority-queue-empty? spq) 
    (zero? (shared-priority-queue-size spq)))

  (define (shared-priority-queue-put! spq o)
    (define size (shared-priority-queue-size spq))
    (define (grow! spq min-capacity)
      (define old (%spq-es spq))
      ;; double if it's small, otherwise 50%.
      (let* ((capacity (+ size (if (< size 64)
				  (+ size 2)
				  (div size 2))))
	     (new (make-vector capacity)))
	(do ((i 0 (+ i 1)))
	    ((= i size))
	  (vector-set! new i (vector-ref old i)))
	(%spq-es-set! spq new)))
	      
    (mutex-lock! (%spq-lock spq))
    (%spq-w-set! spq (+ (%spq-w spq) 1))
    (when (>= size (vector-length (%spq-es spq)))
      (grow! spq (+ size 1)))
    (if (zero? size)
	(vector-set! (%spq-es spq) 0 o)
	(shift-up spq size o))
    (%spq-size-set! spq (+ size 1))
    (when (> (%spq-w spq) 0)
      (condition-variable-broadcast! (%spq-cv spq)))
    (mutex-unlock! (%spq-lock spq)))

  (define (shared-priority-queue-get! spq . maybe-timeout)
    (let ((timeout (if (pair? maybe-timeout) (car maybe-timeout) #f))
	  (timeout-value (if (and (pair? maybe-timeout)
				  (pair? (cdr maybe-timeout)))
			     (cadr maybe-timeout)
			     #f)))
      (mutex-lock! (%spq-lock spq))
      (%spq-w-set! spq (+ (%spq-w spq) 1))
      (let loop ()
	(cond ((shared-priority-queue-empty? spq)
	       (cond ((mutex-unlock! (%spq-lock spq) (%spq-cv spq) timeout)
		      (mutex-lock! (%spq-lock spq))
		      (loop))
		     (else
		      (%spq-w-set! spq (- (%spq-w spq) 1))
		      timeout-value)))
	      (else
	       (%spq-w-set! spq (- (%spq-w spq) 1))
	       (%spq-size-set! spq (- (shared-priority-queue-size spq) 1))
	       (let* ((s (shared-priority-queue-size spq))
		      (es (%spq-es spq))
		      (r (vector-ref es 0))
		      (x (vector-ref es s)))
		 (vector-set! es s #f)
		 (unless (zero? s) (shift-down spq 0 x))
		 (mutex-unlock! (%spq-lock spq))
		 r))))))

  (define (shared-priority-queue-remove! spq o)
    (define cmp (shared-priority-queue-compare spq))
    (define es (%spq-es spq))
    (define (find)
      (define len (shared-priority-queue-size spq))
      (let loop ((i 0))
	(cond ((= i len) -1)
	      ((zero? (cmp o (vector-ref es i))) i)
	      (else (loop (+ i 1))))))
    (define (remove-at spq index)
      (%spq-size-set! spq (- (shared-priority-queue-size spq) 1))
      (let ((s (shared-priority-queue-size spq)))
	(if (= s index)
	    (vector-set! es s #f)
	    (let ((moved (vector-ref es s)))
	      (vector-set! es s #f)
	      (shift-down spq index moved)
	      (when (eq? (vector-ref es index) moved)
		(shift-up spq index moved)))))
      (mutex-unlock! (%spq-lock spq)))
    (mutex-lock! (%spq-lock spq))
    (let ((index (find)))
      (if (>= index 0)
	  (begin (remove-at spq index) #t)
	  (begin (mutex-unlock! (%spq-lock spq)) #f))))

  (define (shift-up spq index o)
    (define cmp (shared-priority-queue-compare spq))
    (define es (%spq-es spq))
    (let loop ((k index))
      (if (> k 0)
	  (let* ((parent (div (- k 1) 2))
		 (e (vector-ref es parent)))
	    (if (>= (cmp o e) 0)
		(vector-set! es k o)
		(begin
		  (vector-set! es k e)
		  (loop parent))))
	  (vector-set! es k o))))

  (define (shift-down spq k x)
    (define cmp (shared-priority-queue-compare spq))
    (define es (%spq-es spq))
    (define size (shared-priority-queue-size spq))
    (define half (div size 2))
    (let loop ((k k))
      (if (< k half)
	  (let* ((child (+ (* k 2) 1))
		 (o (vector-ref es child))
		 (right (+ child 1)))
	    (let-values (((o child)
			  (if (and (< right size)
				   (> (cmp o (vector-ref es right)) 0))
			      (values (vector-ref es right) right)
			      (values o child))))
	      (if (<= (cmp x o) 0)
		  (vector-set! es k x)
		  (begin
		    (vector-set! es k o)
		    (loop child)))))
	  (vector-set! es k x))))

  (define (shared-priority-queue-clear! spq) 
    (mutex-lock! (%spq-lock spq))
    ;; clear it
    (do ((len (shared-priority-queue-size spq))
	 (es (%spq-es spq))
	 (i 0 (+ i 1)))
	((= i len) 
	 (%spq-size-set! spq 0)
	 (mutex-unlock! (%spq-lock spq)))
      (vector-set! es i #f)))
  
  )

;; Local Variables:
;; coding: utf-8-unix
;; End:
