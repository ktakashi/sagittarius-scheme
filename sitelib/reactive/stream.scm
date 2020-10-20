;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; reactive/stream.scm - Reactive stream processing
;;;  
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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

;; ref https://en.wikipedia.org/wiki/Reactive_programming
;; This provides the stream processing with push type (possibly push-pull)
(library (reactive stream)
    (export observable?
	    generator->observable
	    observable-wait!
	    reactive-stream (rename (reactive-stream $))
	    -> ? !)
    (import (rnrs)
	    (srfi :158 generators-and-accumulators)
	    (util concurrent))

;; for now using an actor
(define-record-type observable
  (fields actor))
(define (generator->observable gen listener subscriber)
  (define (task reciever sender)
    (let loop ((v (gen)))
      (unless (eof-object? v)
	(let ((v2 (listener v)))
	  (unless (eof-object? v2)
	    (subscriber v2)))
	(loop (gen)))))
  (make-observable (make-shared-queue-channel-actor task)))

(define (observable-wait! observable . timeout)
  (apply actor-wait! (observable-actor observable) timeout))

(define-syntax -> (syntax-rules ()))
(define-syntax ?  (syntax-rules ()))
(define-syntax !  (syntax-rules ()))
(define-syntax reactive-stream
  (syntax-rules (-> ? !)
    ((_ "collect" gen (f ...) (s ...) ((? command) commands ...))
     (reactive-stream "collect" gen
		      (f ... (let ((p command))
			       (lambda (v)
				 (if (p v)
				     v
				     (eof-object)))))
		      (s ...) (commands ...)))
    ((_ "collect" gen (f ...) (s ...) ((-> command) commands ...))
     (reactive-stream "collect" gen
		      (f ... (let ((fi command)) fi))
		      (s ...) (commands ...)))
    ((_ "collect" gen (f ...) (s ...) ((! command) commands ...))
     (reactive-stream "collect" gen
		      (f ...)
		      (s ... (let ((su command)) su))
		      (commands ...)))
    ((_ "collect" gen (f ...) (s ...) ())
     (reactive-stream "listener" gen (values f ...) (s ...)))
    ((_ "listener" gen (f ...) (s ...))
     (reactive-stream "subscribe" gen
		      (let ((f* (list f ...)))
			(lambda (v)
			  (let loop ((f* f*) (v v))
			    (cond ((null? f*) v)
				  ((eof-object? v) v)
				  (else (loop (cdr f*) ((car f*) v)))))))
		      (values s ...)))
    ((_ "subscribe" gen listener (s ...))
     (generator->observable gen listener
			    (let ((s* (list s ...)))
			      (lambda (v)
				(do ((s* s* (cdr s*)))
				    ((null? s*))
				  ((car s*) v))))))
    ((_ gen command commands ...)
     (reactive-stream "collect" gen () () (command commands ...)))))
		      

)
