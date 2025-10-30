;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; util/concurrent/notifier.scm - Thread notification
;;;  
;;;   Copyright (c) 2010-2023  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (util concurrent notifier)
    (export notifier? make-notifier
	    (rename (notifier <notifier>))
	    notifier-waiting?
	    notifier-send-notification!
	    notifier-wait-notification!

	    event? make-event
	    (rename (event <event>))
	    event-set-event!
	    event-receive!
	    )
    (import (rnrs)
	    (srfi :18)
	    (srfi :19)
	    (util concurrent atomic))
(define-record-type notifier
  (fields lock cv waiter)
  (protocol (lambda (p)
	      (lambda ()
		(p (make-mutex "notifier-lock")
		   (make-condition-variable)
		   (make-atomic-fixnum 0))))))

(define (notifier-waiting? notifier)
  (> (atomic-load (notifier-waiter notifier)) 0))

(define (notifier-send-notification! notifier :optional (broadcast? #t))
  (and (notifier-waiting? notifier)
       (let ((cv (notifier-cv notifier)))
	 (if broadcast?
	     (condition-variable-broadcast! cv)
	     (condition-variable-signal! cv)))))

(define (notifier-wait-notification! notifier . maybe-timeout)
  (define to (and (not (null? maybe-timeout))
		  (adjust-timeout (car maybe-timeout))))
  (define lock (notifier-lock notifier))
  (mutex-lock! lock)
  (atomic-fixnum-inc! (notifier-waiter notifier))
  (let ((r (mutex-unlock! lock (notifier-cv notifier) to)))
    (atomic-fixnum-dec! (notifier-waiter notifier))
    r))

;; Similar to SetEvent on Win32 API
;; It holds the value until the event is read
(define-record-type event
  (fields lock cv (mutable received))
  (protocol (lambda (p)
	      (lambda (:optional (set? #f))
		(p (make-mutex "event-lock")
		   (make-condition-variable)
		   set?)))))

(define (event-set-event! event)
  (define lock (event-lock event))
  (mutex-lock! lock)
  (event-received-set! event #t)
  (condition-variable-broadcast! (event-cv event))
  (mutex-unlock! lock))

(define (event-receive! event . maybe-timeout)
  (define to (and (not (null? maybe-timeout))
		  (adjust-timeout (car maybe-timeout))))
  (define lock (event-lock event))
  (mutex-lock! lock)
  (cond ((event-received event)
	 (event-received-set! event #f)
	 (mutex-unlock! lock))
	((mutex-unlock! lock (event-cv event) to)
	 (mutex-lock! lock)
	 (let ((r (and (event-received event)
		       (event-received-set! event #f))))
	   (mutex-unlock! lock)
	   r))
	(else #f)))

(define (adjust-timeout to)
  (if (time? to)
      (case (time-type to)
	((duration) (add-duration (current-time) to))
	(else to))
      to))
)
