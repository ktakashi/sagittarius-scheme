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
	    notifier-send-notification!
	    notifier-wait-notification!)
    (import (rnrs)
	    (srfi :18))
(define-record-type notifier
  (fields lock cv (mutable waiter))
  (protocol (lambda (p)
	      (lambda ()
		(p (make-mutex "notifier-lock")
		   (make-condition-variable)
		   0)))))

(define (notifier-send-notification! notifier)
  (when (> (notifier-waiter notifier) 0)
    (condition-variable-broadcast! (notifier-cv notifier))))

(define (notifier-wait-notification! notifier . maybe-timeout)
  (define timeout (and (not (null? maybe-timeout)) (car maybe-timeout)))
  (let ((lock (notifier-lock notifier)))
    (mutex-lock! lock)
    (notifier-waiter-set! notifier (+ (notifier-waiter notifier) 1))
    (cond ((mutex-unlock! lock (notifier-cv notifier) timeout)
	   (mutex-lock! lock)
	   (notifier-waiter-set! notifier (- (notifier-waiter notifier) 1))
	   (mutex-unlock! lock)
	   #t)
	  (else
	   ;; timeout, how should we reduce this safely?
	   (notifier-waiter-set! notifier (- (notifier-waiter notifier) 1))
	   #f))))

)
