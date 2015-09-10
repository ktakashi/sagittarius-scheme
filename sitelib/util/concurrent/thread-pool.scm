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
	  thread-pool-size
	  thread-pool-push-task!
	  thread-pool-wait-all!
	  thread-pool-release!

	  thread-pool-thread-terminate! ;; hmmmm
	  thread-pool-thread
	  thread-pool-thread-task-running?
	  )
  (import (rnrs)
	  (srfi :18)
	  (util concurrent shared-queue))

(define (make-executor specific i queue)
  (lambda ()
    ;; use thread specific slot to know, which thread is
    ;; available.
    (vector-set! specific i 'idling)
    (let loop ()
      (let ((task (shared-queue-get! queue)))
	;; if it's #f then must be called from
	;; thread-pool-release!
	(when task
	  (vector-set! specific i 'executing)
	  (guard (e (else #f)) (task))
	  (vector-set! specific i 'idling)
	  (loop))))))
(define-record-type (<thread-pool> make-thread-pool thread-pool?)
  (fields threads ;; to join
	  queues  ;; shared-queues
	  specifics ;; switch
	  )
  (protocol
   (lambda (p)
     (lambda (n)
       (let* ((threads (make-vector n))
	      (queues  (make-vector n))
	      (specific (make-vector n #f))
	      (tp (p threads queues specific)))
	 (do ((i 0 (+ i 1)))
	     ((= i n) tp)
	   (let ((q (make-shared-queue)))
	     (vector-set! queues i q)
	     (vector-set! threads i
			  (thread-start!
			   (make-thread (make-executor specific i q)))))))))))

(define (thread-pool-thread tp id)
  (vector-ref (<thread-pool>-threads tp) id))

(define (thread-pool-size tp)
  ;; whatever is fine.
  (vector-length (<thread-pool>-threads tp)))

;; Optional argument must be a procedure takes one argument.
;;  see default-handler
(define (thread-pool-push-task! tp task . opt)
  ;; if we didn't get any then first one, sorry.
  (define (default-handler n)
    (if (negative? n) 0 n))
  ;; idling thread is the highest
  ;; then less number ones
  (define (find-available tp add-to-back?)
    (let* ((threads (<thread-pool>-threads tp))
	   (queue (<thread-pool>-queues tp))
	   (specifics (<thread-pool>-specifics tp))
	   (size (vector-length threads)))
      (let loop ((i 0) (maybe -1) (qsize +inf.0))
	(if (= i size)
	    (add-to-back? maybe)
	    (let ((t (vector-ref threads i))
		  (sp (vector-ref specifics i))
		  (s (shared-queue-size (vector-ref queue i))))
	      (cond ((and (zero? s) (eq? sp 'idling)) i)
		    ((and (add-to-back? i) (< s qsize)) (loop (+ i 1) i s))
		    (else (loop (+ i 1) maybe qsize))))))))
  (let ((where (find-available tp (if (null? opt) default-handler (car opt)))))
    (shared-queue-put! (vector-ref (<thread-pool>-queues tp) where) task)
    where))

(define (thread-pool-wait-all! tp)
  (define (wait-queue tp)
    (define queus (vector->list (<thread-pool>-queues tp)))
    (let loop ()
      (unless (for-all shared-queue-empty? queus)
	(thread-yield!)
	(thread-sleep! 0.1)
	(loop))))
  (define (wait-threads tp)
    ;; (define threads (vector->list (<thread-pool>-threads tp)))
    (define (check-specifics tp)
      (define specifics (<thread-pool>-specifics tp))
      (define size (vector-length specifics))
      (let loop ((i 0))
	(cond ((= i size) #t)
	      ((eq? (vector-ref specifics i) 'idling) (loop (+ i 1)))
	      (else #f))))
    (let loop ()
      (unless (check-specifics tp)
	(thread-yield!)
	(thread-sleep! 0.1)
	(loop))))
  (wait-queue tp)
  (wait-threads tp))


(define (thread-pool-release! tp . opt)
  (let ((type (if (null? opt) 'join (car opt))))
    (vector-for-each (lambda (q) (shared-queue-put! q #f))
		     (<thread-pool>-queues tp))
    (vector-for-each (lambda (t)
		       (case type
			 ;; default join
			 ;; ((join) (thread-join! t))
			 ((terminate) (thread-terminate! t))
			 (else (thread-join! t))))
		     (<thread-pool>-threads tp))))

(define (thread-pool-thread-terminate! tp id)
  (define threads (<thread-pool>-threads tp))
  (define queues (<thread-pool>-queues tp))
  (define sp (<thread-pool>-specifics tp))
  (let ((t (vector-ref threads id))
	(q (vector-ref queues id))
	(nq (make-shared-queue)))
    ;; clear all pending tasks.
    ;; the thread is terminated, so it's not interesting anymore
    (shared-queue-clear! q)
    (thread-terminate! t) ;; this is dangerous, don't do it casually!
    (vector-set! queues id nq)
    ;; prepare for next time
    (vector-set! threads id
		 (thread-start! (make-thread (make-executor sp id nq))))))

(define (thread-pool-thread-task-running? tp id)
  (let ((sp (<thread-pool>-specifics tp)))
    (eq? (vector-ref sp id) 'executing)))
;; TODO Should we add thread-pool-stop! ?

  )
