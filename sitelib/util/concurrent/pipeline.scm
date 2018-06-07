;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; util/concurrent/pipeline.scm - Concurrent pipeline
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

(library (util concurrent pipeline)
    (export )
    (import (rnrs)
	    (util concurrent actor)
	    (util concurrent shared-queue))

(define-record-type pipe-error
  (seald #t)
  (fields condition message))
(define-record-type pipe-stop-message
  (seald #t))
(define *pipe:stop-message* (make-pipe-stop-message))

(define-record-type pipe-unit
  (parent <actor>)
  (fields error-receiver)
  (lambda (n)
    (lambda (proc make-receiver make-sender make-error-sender)
      (let-values (((error-receiver/client error-sender/actor)
		    (make-error-sender)))
	((n (lambda (receiver sender)
	      (define stop? #f)
	      (let loop ((msg (receiver)))
		(guard (e (else (error-sender/actor (make-pipe-error e msg))))
		  (let ((r (proc msg)))
		    (when (pipe-stop-message? r) (set! stop? #t))
		    (sender r)))
		(unless stop? (loop (receiver)))))
	    make-sender make-error-sender #f)
	 error-receiver/client)))))

)

