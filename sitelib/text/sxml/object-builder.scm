;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sxml/object-builder.scm - SXML to Scheme object builder
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (text sxml object-builder)
  (export sxml->object
	  ;; object->sxml

	  make-object-builder object-builder? sxml-object-builder
	  )
  (import (rnrs)
	  (text sxml tools))

  (define-record-type object-builder
    (fields tag?
	    >object
	    next-builder))

  (define (default-unknown-tag-handler builder sxml)
    (assertion-violation 'sxml->object "unknown tag" sxml builder))
  
  (define (sxml->object sxml builder . opt)
    (define (rec sxml builder)
      (define check-tag? (object-builder-tag? builder))
      (define ->object (object-builder->object builder))
      (define next-builder (object-builder-next-builder builder))
      (define handler (if (null? opt) default-unknown-tag-handler (car opt)))
      (let ((content (sxml:content sxml))
	    (attrs   (sxml:attr-list sxml)))
	(if (check-tag? (sxml:name sxml))
	    (->object attrs (map (lambda (c) (sxml->object c next-builder))
				 content))
	    (handler builder sxml))))
    (cond ((not builder) sxml)
	  ((and (pair? sxml) (eq? (car sxml) '*TOP*))
	   (rec (car (sxml:content sxml)) builder))
	  ((sxml:element? sxml) (rec sxml builder))
	  (else (map (lambda (c) (rec c builder)) (sxml:content sxml)))))

  (define-syntax sxml-object-builder
    (syntax-rules ()
      ((_ (tag ctr))
       (make-object-builder
	(lambda (t) (eq? t 'tag))
	ctr
	#f))
      ((_ (tag ctr next))
       (make-object-builder
	(lambda (t) (eq? t 'tag))
	ctr
	(sxml-object-builder next)))))
  
  )
