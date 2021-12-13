;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; sagittarius/document/format/scribble.scm - Scribble to document parser
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

#!nounbound
(library (sagittarius document format scribble)
    (export scribble->document)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius document input)
	    ;; We use this for simplicity...
	    (scribble parser)
	    (scribble convert)
	    (srfi :1 lists))

(define (scribble->document input)
  `(document
    (info
     (source ,(document-input-filename input)))
    (content
     ,@(scribble-token*->content
	(scribble-parse (document-input-port input))))))

(define (scribble-token*->content token*)
  (let loop ((token* token*) (acc '()))
      (if (null? token*)
	  (reverse! acc)
	  (let-values (((next* next-acc)
			(consume-token* (car token*) (cdr token*) acc)))
	    (loop next* next-acc)))))

;; We need to do sort of the same as scribble->html unfortunately
(define (consume-token* token next* acc)
  ;; (write token) (newline)
  (cond ((pair? token)
	 (case (car token)
	   ((section subsection subsubsection sub*section)
	    (handle-section token next* acc))
	   ((code)
	    (values next*
		    (cons `(code (@)
				 ,@(scribble-token*->content (cdr token)))
			  acc)))
	   (else (values next* (cons token acc)))))
	(else (values next* (cons token acc)))))

(define (handle-section token next* acc0)
  (define section (car token))
  (define (section->level section)
    (case section
      ((section)       1)
      ((subsection)    2)
      ((subsubsection) 3)
      ((sub*section)   4)
      (else #f)))
  (define section-level (section->level section))
  (define (consume cur next*)
    (let loop ((acc '()) (next* next*))
      (if (null? next*)
	  (values next* (reverse! acc))
	  (let ((token (car next*)) (next* (cdr next*)))
	    (cond ((and (pair? token) (section->level (car token))) =>
		   (lambda (level)
		     (if (< cur level)
			 ;; okey sub section can be in this section
			 (let-values (((next* next-acc)
				       (consume-token* token next* acc)))
			   (loop next-acc next*))
			 ;; put it back :)
			 (values (cons token next*) (reverse! acc)))))
		  (else
		   (let-values (((next* next-acc)
				 (consume-token* token next* acc)))
		     (loop next-acc next*))))))))

  (unless section-level
    (assertion-violation 'handle-section "Unknown section" section token))

  (let-values (((attr content) (scribble-parse-attribute (cdr token)))
	       ((next* acc) (consume section-level next*)))
    (values next*
	    (cons `(section (@)
			    (header (@ (level ,(number->string section-level)))
				    ,@(scribble-token*->content content))
			    ,@acc)
		  acc0))))

)
