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
    (export thunk->future future-map)
    (import (rnrs)
	    (srfi :39 parameters)
	    (util concurrent future)
	    (util concurrent executor))

(define *completable-future:default-executor*
  (make-parameter (make-thread-pool-executor 5 push-future-handler)))

(define-record-type completable-future
  (parent <executor-future>)
  (fields executor)
  (protocol (lambda (n)
	      (lambda (thunk executor)
		((n thunk) executor)))))

(define thunk->future
  (case-lambda
   ((thunk) (thunk->future thunk (*completable-future:default-executor*)))
   ((thunk executor)
    (let ((future (make-completable-future thunk executor)))
      (execute-future! executor future)
      future))))

(define (future-map proc future)
  (thunk->future (lambda opt (proc (apply future-get future opt)))
		 (if (completable-future? future)
		     (completable-future-executor future)
		     (*completable-future:default-executor*))))

  
)
