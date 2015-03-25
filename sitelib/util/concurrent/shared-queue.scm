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
	    shared-queue-put! shared-queue-get!)
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
	       (cond ((mutex-unlock! (%lock sq) (%read-cv sq) timeout)
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
		 (mutex-unlock! (%lock sq))))))))

  (define (shared-queue-overflows? sq count)
    (and (>= (shared-queue-max-length sq) 0)
	 (> (+ count (shared-queue-size sq)) (shared-queue-max-length sq))))

  )

;; Local Variables:
;; coding: utf-8-unix
;; End:
