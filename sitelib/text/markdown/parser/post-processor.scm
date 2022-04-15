;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/post-processor.scm - Markdown parser post processor
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (text markdown parser post-processor)
    (export make-post-processor

	    make-post-processor-spec
	    post-processor-spec?)
    (import (rnrs)
	    (record builder)
	    (text markdown parser nodes))

(define-record-type post-processor-spec
  (fields predicate
	  processor)
  (protocol (lambda (p)
	      (lambda (pred proc)
		(unless (procedure? pred)
		  (assertion-violation 'make-post-processor-spec
				       "Predicate must be a procedure" pred))
		(unless (procedure? proc)
		  (assertion-violation 'make-post-processor-spec
				       "Processor must be a procedure" proc))
		(p pred proc)))))

(define (make-post-processor . spec*)
  (define (find-spec node spec*)
    (find (lambda (spec) ((post-processor-spec-predicate spec) node)) spec*))
  (define (visit-children node)
    ;; In case of unlink or other modification, we need do via prev/next
    (let loop ((child (markdown-node:first-child node)))
      (when child
	(let ((next (markdown-node-next child)))
	  (process child)
	  (loop next)))))
  (define (process node)
    (cond ((find-spec node spec*) =>
	   (lambda (spec)
	     ((post-processor-spec-processor spec) node visit-children)))
	  (else (visit-children node)))
    node)
  process)

)
