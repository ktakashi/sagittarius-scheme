;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; util/concurrent/actor.scm - Actor
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (util concurrent actor)
    (export actor? (rename actor <actor>) make-actor
	    actor-send-message!
	    actor-receive-message!
	    actor-wait!
	    actor-terminate!
	    
	    make-shared-queue-channel-actor
	    make-shared-priority-queue-channel-actor

	    make-shared-queue-channel
	    make-shared-priority-queue-channel
	    )
    (import (rnrs)
	    (srfi :18)
	    (util concurrent shared-queue))

;; base actor record
;; actor has 2 channels to receive/send messages
(define-record-type (actor %make-actor actor?)
  (fields thread receiver sender))

(define (make-shared-queue-channel . opt)
  (define sq (apply make-shared-queue opt))
  (define (receiver . opt) (apply shared-queue-get! sq opt))
  (define (sender v . opt) (apply shared-queue-put! sq v opt))
  (values receiver sender))

(define (make-shared-priority-queue-channel compare . opt)
  (define sq (apply make-shared-priority-queue compare opt))
  (define (receiver . opt) (apply shared-priority-queue-get! sq opt))
  (define (sender v . opt) (apply shared-priority-queue-put! sq v opt))
  (values receiver sender))

(define (actor-send-message! actor mesasge . opt)
  (apply (actor-sender actor) actor message opt))

(define (actor-receive-message! actor . opt)
  (apply (actor-receiver actor) actor opt))

;; need this?
(define (actor-wait! actor) (thread-join! (actor-thread actor)))
(define (actor-terminate! actor) (thread-terminate! (actor-thread actor)))

(define (make-actor task make-receiver make-sender)
  (let-values (((receiver/actor sender/client) (make-receiver))
	       ((receiver/client sender/actor) (make-sender)))
    (let ((t (make-thread (lambda () (task receiver/client sender/client)))))
      (thread-start! t)
      (%make-actor t receiver/actor sender/actor))))

;; common actors
(define (make-shared-queue-channel-actor task)
  (make-actor task make-shared-queue-channel make-shared-queue-channel))

(define (make-shared-priority-queue-channel-actor task compare)
  (define (make-channel) (make-shared-priority-queue-channel compare))
  (make-actor task make-channel make-channel))

)

