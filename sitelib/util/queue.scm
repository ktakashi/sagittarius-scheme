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
	    (clos user)
	    (clos core)
	    (util deque)
	    (sagittarius object)
	    (sagittarius control))

  (define-class <queue> (<sequence>)
    ((deque :init-keyword :deque)))
  (define (queue? o) (is-a? o <queue>))

  ;; only for marking
  (define-class <mtqueue> (<queue>) ())
  (define (mtqueue? o) (is-a? o <mtqueue>))

  (define (make-queue) (make <queue> :deque (make-deque)))
  (define (make-mtqueue :key (max-length -1)) 
    (make <mtqueue> :deque (make-mtdeque :max-length max-length)))

  (define (queue-length q) (deque-length (~ q 'deque)))
  (define (mtqueue-max-length q) (mtdeque-max-length (~ q 'deque)))
  (define (mtqueue-room q) (mtdeque-room (~ q 'deque)))

  (define (queue-empty? q) (deque-empty? (~ q 'deque)))

  (define (queue->list q) (deque->list (~ q 'deque)))
  (define (list->queue lst :optional (class <queue>) :rest initargs)
    (rlet1 q (make class)
      (set! (~ q 'deque) (apply list->deque lst 
				;; a bit tricky...
				(if (subtype? class <mtqueue>)
				    <mtdeque>
				    <deque>)
				initargs))))
  (define-method copy-queue ((q <queue>))
    (list->queue (deque->list (~ q 'deque)) (class-of q)))
  (define-method copy-queue ((q <mtqueue>))
    (list->queue (deque->list (~ q 'deque)) (class-of q)
		 :max-length (mtqueue-max-length q)))

  (define queue-front
    (case-lambda
     ((q) (deque-front (~ q 'deque)))
     ((q default) (deque-front (~ q 'deque) default))))
  (define queue-rear
    (case-lambda
     ((q) (deque-rear (~ q 'deque)))
     ((q default) (deque-rear (~ q 'deque) default))))

  (define (find-in-queue pred q) (find-in-deque pred (~ q 'deque)))
  (define (any-in-queue pred q) (any-in-deque pred (~ q 'deque)))
  (define (every-in-queue pred q) (every-in-deque pred (~ q 'deque)))

  (define (enqueue! q obj . more) (apply deque-push! (~ q 'deque) obj more) q)
  (define (enqueue-unique! q cmp obj . more)
    (apply deque-push-unique! (~ q 'deque) cmp obj more) q)
  (define (enqueue/wait! q obj . opts)
    (apply deque-push/wait! (~ q 'deque) obj opts))

  (define (queue-push! q obj . more) 
    (apply deque-unshift! (~ q 'deque) obj more) q)
  (define (queue-push-unique! q cmp obj . more) 
    (apply deque-unshift-unique! (~ q 'deque) cmp obj more) q)
  (define (queue-push/wait! q obj . opts) 
    (apply deque-unshift/wait! (~ q 'deque) obj opts))

  (define (dequeue! q . opts) (apply deque-shift! (~ q 'deque) opts))
  (define (dequeue-all! q) (deque-shift-all! (~ q 'deque)))
  (define (dequeue/wait! q . opts) (apply deque-shift/wait! (~ q 'deque) opts))

  (define queue-pop! dequeue!)
  (define queue-pop/wait! dequeue/wait!)  

  (define (remove-from-queue! pred q) (remove-from-deque! pred (~ q 'deque)))

  )