;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; util/concurrent/thread-pool.scm - Thread pool
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

(library (util concurrent thread-pool)
  (export make-thread-pool thread-pool? <thread-pool>
	  thread-pool-push-task!
	  thread-pool-wait-all!
	  thread-pool-release!)
  (import (rnrs)
	  (srfi :18)
	  (util concurrent shared-queue))

(define-record-type (<thread-pool> make-thread-pool thread-pool?)
  (fields threads ;; to join
	  queues  ;; shared-queues
	  )
  (protocol
   (lambda (p)
     (lambda (n)
       (define (make-executor queue)
	 (lambda ()
	   ;; use thread specific slot to know, which thread is
	   ;; available.
	   (thread-specific-set! (current-thread) 'idling)
	   (let loop ()
	     (let ((task (shared-queue-get! queue)))
	       ;; if it's #f then must be called from
	       ;; thread-pool-release!
	       (when task
		 (thread-specific-set! (current-thread) 'executing)
		 (guard (e (else #f)) (task))
		 (thread-specific-set! (current-thread) 'idling)
		 (loop))))))
       (let ((threads (make-vector n))
	     (queues  (make-vector n)))
	 (do ((i 0 (+ i 1)))
	     ((= i n) (p threads queues))
	   (let ((q (make-shared-queue)))
	     (vector-set! queues i q)
	     (vector-set! threads i
			  (thread-start!
			   (make-thread (make-executor q)))))))))))

;; TODO should we always put task to thread?
;;      maybe it's better to raise an error?
(define (thread-pool-push-task! tp task . opt)
  ;; if we didn't get any then first one, sorry.
  (define (default-handler n)
    (if (negative? n) 0 n))
  ;; idling thread is the highest
  ;; then less number ones
  (define (find-available tp add-to-back?)
    (let* ((threads (<thread-pool>-threads tp))
	   (queue (<thread-pool>-queues tp))
	   (size (vector-length threads)))
      (let loop ((i 0) (maybe -1) (qsize +inf.0))
	(if (= i size)
	    (add-to-back? maybe)
	    (let ((t (vector-ref threads i))
		  (s (shared-queue-size (vector-ref queue i))))
	      (cond ((and (zero? s) (eq? (thread-specific t) 'idling)) i)
		    ((and (add-to-back? i) (< s qsize)) (loop (+ i 1) i s))
		    (else (loop (+ i 1) maybe qsize))))))))
  (let ((where (find-available tp (if (null? opt) default-handler (car opt)))))
    (shared-queue-put! (vector-ref (<thread-pool>-queues tp) where) task)))

(define (thread-pool-wait-all! tp)
  (define (wait-queue tp)
    (define queus (vector->list (<thread-pool>-queues tp)))
    (let loop ()
      (unless (for-all shared-queue-empty? queus)
	(thread-yield!)
	(thread-sleep! 0.1)
	(loop))))
  (define (wait-threads tp)
    (define threads (vector->list (<thread-pool>-threads tp)))
    (let loop ()
      (unless (for-all (lambda (t) (eq? (thread-specific t) 'idling)) threads)
	(thread-yield!)
	(thread-sleep! 0.1)
	(loop))))
  (wait-queue tp)
  (wait-threads tp))


(define (thread-pool-release! tp)
  (vector-for-each (lambda (q) (shared-queue-put! q #f))
		   (<thread-pool>-queues tp))
  (vector-for-each (lambda (t) (thread-join! t))
		   (<thread-pool>-threads tp)))

;; TODO Should we add thread-pool-stop! ?

  )
