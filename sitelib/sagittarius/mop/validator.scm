;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; validator.scm -  metaclass to support :validate
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius mop validator)
    (export <validator-meta> <validator-mixin>)
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius control))

  (define-class <validator-meta> (<class>) ())
  (define-method compute-getters-and-setters ((class <validator-meta>) slots)
    (let ((r (call-next-method)))
      (for-each (lambda (acc slot)
		  (let ((pre  (slot-definition-option slot :validator #f))
			(post (slot-definition-option slot :observer #f)))
		    (when (or pre post)
		      (let1 setter
			  (cond ((and pre post)
				 (lambda (o v)
				   (slot-set-using-accessor! o acc (pre o v))
				   (post o (slot-ref-using-accessor o acc))))
				(pre
				 (lambda (o v)
				   (slot-set-using-accessor! o acc (pre o v))))
				(else
				 (lambda (o v)
				   (slot-set-using-accessor! o acc v)
				   (post o (slot-ref-using-accessor o acc)))))
			(slot-set! acc 'setter setter)))))
		r slots)
      r))

  (define-class <validator-mixin> () () :metaclass <validator-meta>)
)