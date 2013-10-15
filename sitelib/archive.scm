;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; archive.scm - Generic archive access
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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


(library (archive)
    (export <archive-input>
	    <archive-output>
	    <archive-entry>

	    ;; for input
	    make-archive-input
	    next-entry!
	    extract-entry

	    ;; for output
	    make-archive-output
	    create-entry
	    append-entry!

	    ;; common
	    finish!

	    archive-entry-name

	    ;; utilities
	    call-with-archive-input
	    call-with-archive-output
	    do-entry

	    )
    (import (rnrs)
	    (rnrs eval)
	    (rename (archive interface)
		    (make-archive-input  inf:make-archive-input)
		    (make-archive-output inf:make-archive-output))
	    (sagittarius control))

  ;; utilities
  (define (call-with-archive-input type source proc)
    (let1 in (make-archive-input type source)
      (let-values ((result (proc in)))
	(finish! in)
	(apply values result))))

  (define (call-with-archive-output type sink proc)
    (let1 in (make-archive-output type sink)
      (let-values ((result (proc in)))
	(finish! in)
	(apply values result))))

  ;; generic constructors
  (define (make-archive-input type source)
    (eval `(make-archive-input ',type ,source)
	  (environment '(rnrs) '(archive interface) `(archive ,type))))

  (define (make-archive-output type source)
    (eval `(make-archive-output ',type ,source)
	  (environment '(rnrs) '(archive interface) `(archive ,type))))

  (define-syntax do-entry
    (syntax-rules ()
      ((_ (e in) body ...)
       (do-entry (e in #t) body ...))
      ((_ (e in r) body ...)
       (do ((e (next-entry! in) (next-entry! in)))
	   ((not e) r)
	 body ...))))

)