;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; util/concurrent/completable-future.scm - Completable future
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

;; not sure if we should make separate library for this...
#!nounbound
(library (util concurrent completable-future)
    (export thunk->future future-map future-flatmap
	    future-guard

	    *completable-future:default-executor*
	    
	    future-map/executor
	    future-flatmap/executor
	    future-guard/executor)
    (import (rnrs)
	    (srfi :39 parameters)
	    (util concurrent future)
	    (util concurrent executor)
	    (scheme lazy))

(define *completable-future:default-executor*
  ;; Let's not create an executor during library load
  (make-parameter
   (delay (make-fork-join-executor))
   (lambda (v)
     (cond ((promise? v) v)
	   ((executor? v) (delay v))
	   (else (assertion-violation '*completable-future:default-executor*
				      "Promise or executor required" v))))))

(define-record-type completable-future
  (parent <executor-future>)
  (fields executor)
  (protocol (lambda (n)
	      (lambda (thunk executor)
		((n thunk) executor)))))

(define thunk->future
  (case-lambda
   ((thunk)
    (thunk->future thunk (force (*completable-future:default-executor*))))
   ((thunk executor)
    (let ((future (make-completable-future thunk executor)))
      (execute-future! executor future)
      future))))

(define (search-executor future future*)
  (cond ((completable-future? future)
	 (completable-future-executor future))
	((find completable-future? future*) =>
	 (lambda (f*) (completable-future-executor (car f*))))
	(else (force (*completable-future:default-executor*)))))
(define (future-map proc future . future*)
  (apply future-map/executor 
	 (search-executor future future*) proc future future*))
;; damn...
(define (future-map/executor executor proc future . future*)
  (thunk->future (if (null? future*)
		     (lambda () (proc (future-get future)))
		     (lambda ()
		       (apply proc
			      (future-get future)
			      (map (lambda (f) (future-get f)) future*))))
		 executor))

;; For now very naive implementation...
(define (future-flatmap proc future . future*)
  (apply future-flatmap/executor
	 (search-executor future future*) proc future future*))

(define (future-flatmap/executor executor proc future . future*)
  (thunk->future
   (if (null? future*)
       (lambda ()
	 (let ((f (proc (future-get future))))
	   (future-get f)))
       (lambda ()
	 (let ((f (apply proc
			 (future-get future)
			 (map (lambda (f) (future-get f)) future*))))
	   (future-get f))))
   executor))

(define (future-guard proc future)
  (future-guard/executor
   (if (completable-future? future)
       (completable-future-executor future)
       (force (*completable-future:default-executor*)))
   proc future))

(define (future-guard/executor executor proc future)
  (thunk->future
   (lambda ()
     (guard (e (else (proc e)))
       (future-get future)))
   executor))
)
