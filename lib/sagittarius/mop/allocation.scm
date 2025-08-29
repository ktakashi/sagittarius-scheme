;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; allocation.scm: metaclass to support :allocation
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

(library (sagittarius mop allocation)
    (export <allocation-meta> <allocation-mixin>
	    (rename (<allocation-mixin> <allocation>))

	    <cached-allocation-meta>
	    <cached-allocation-mixin>
	    (rename (<cached-allocation-mixin> <cached-allocation>))
	    )
    (import (rnrs)
	    (sagittarius) ;; for gensym
	    (clos user)
	    (clos core)
	    (sagittarius clos))

(define-class <allocation-meta> (<class>) ())
#|
  ;; This also works but deprecated.
  (define-method compute-getters-and-setters ((class <allocation-meta>) slots)
    (let ((r (call-next-method)))
      (for-each
       (lambda (acc slot)
	 (cond ((slot-definition-option slot :allocation :instance)
		=> (lambda (type)
		     (case type
		       ((:instance))
		       ((:class)
			(let* ((init-value (slot-definition-option
					    slot :init-value #f))
			       (init-thunk (slot-definition-option 
					    slot :init-thunk #f))
			       (def (if init-thunk (init-thunk) init-value)))
			  (slot-set! acc 'setter (lambda (o v) (set! def v)))
			  (slot-set! acc 'getter (lambda (o) def))))
		       (else
			(assertion-violation '<allocation-meta>
					     "unknown :allocation type"
					     type)))))))
		r slots)
      r))
|#

(define-method compute-getters-and-setters ((class <allocation-meta>) slots)
  (let loop ((i 0) (slots slots) (r '()))
    (if (null? slots)
	(reverse! r)
	(let ((slot (car slots)))
	  (let-values (((g s b? . rest)
			(apply values
			       (compute-getter-and-setter class slot))))
	    (let* ((virtual? (and (not (null? rest)) (car rest)))
		   (sac (%make-slot-accessor
			 class slot (if virtual? -1 i) g s b?)))
	      (loop (if virtual? i (+ i 1)) (cdr slots) (cons sac r))))))))

(define-method compute-getter-and-setter ((class <allocation-meta>) slot)
  (cond ((slot-definition-option slot :allocation :instance)
	 => (lambda (type)
	      (case type
		((:instance) (call-next-method))
		((:class)
		 (let* ((init-value (slot-definition-option
				     slot :init-value #f))
			(init-thunk (slot-definition-option 
				     slot :init-thunk #f))
			(def (if init-thunk (init-thunk) init-value)))
		   (list (lambda (o) def)
			 (lambda (o v) (set! def v))
			 #f
			 #t)))
		((:delegate)
		 (let ((to-slot (slot-definition-option slot :forwarding #f)))
		   (unless (symbol? to-slot)
		     (assertion-violation 'compute-getter-and-setter
		       ":allocation :delegate requires :forwarding {slot}"))
		   (list (lambda (o) (slot-ref o to-slot))
			 (lambda (o v) (slot-set! o to-slot v))
			 #f
			 #t)))
		;; Gauche's :virtual
		((:virtual)
		 (let ((getter (slot-definition-option slot :slot-ref #f))
		       (setter (slot-definition-option slot :slot-set! #f))
		       (bound? (slot-definition-option slot :slot-bound? #f)))
		   (unless (procedure? getter)
		     (assertion-violation 'compute-getter-and-setter
		      ":allocation :virtual requires at least :slot-ref {procedure}"))
		   (list getter setter bound? #t)))
		(else
		 (assertion-violation '<allocation-meta>
				      "unknown :allocation type"
				      type)))))
	(else (call-next-method))))
(define-class <allocation-mixin> () () :metaclass <allocation-meta>)

(define cached-slot (gensym))
(define (caching-slot? slot) (slot-definition-option slot :cached #f))
(define-class <cached-allocation-meta> (<allocation-meta>) ())
(define-method compute-slots ((class <cached-allocation-meta>))
  (let ((slots (call-next-method)))
    (if (exists caching-slot? slots)
	(cons `(,cached-slot :init-thunk ,make-eq-hashtable) slots)
	slots)))
(define-method compute-getter-and-setter ((class <cached-allocation-meta>) slot)
  (let ((acc (call-next-method)))
    (if (caching-slot? slot)
	(let-values (((getter setter bound? . rest) (apply values acc)))
	  (let ((name (slot-definition-name slot)))
	    (cons* (lambda (o)
		     (cond ((hashtable-ref (slot-ref o cached-slot) name #f))
			   (else
			    (let ((v (getter o)))
			      (hashtable-set! (slot-ref o cached-slot) name v)
			      v))))
		   (and setter
			(lambda (o v)
			  (setter o v)
			  (hashtable-set! (slot-ref o cached-slot) name v)))
		   bound?
		   rest)))
	acc)))
(define-class <cached-allocation-mixin> () ()
  :metaclass <cached-allocation-meta>)
)
