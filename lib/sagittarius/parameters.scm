;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/parameters.scm - parameter library
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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

;; to make current-dynamic-environment weak-hashtable
(library (sagittarius parameters)
    (export make-parameter parameterize <parameter> parameter?)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (only (sagittarius) current-dynamic-environment))


  (define mark (list 0)) ;; unique mark

  (define-class <parameter> ()
    ((converter :init-keyword :converter)
     (init :init-keyword :init)))
  (define (parameter? o) (is-a? o <parameter>))
  
  (define-method object-apply ((p <parameter>))
    (let ((r (weak-hashtable-ref (current-dynamic-environment) p mark)))
      (if (eq? r mark)
	  (let ((init (slot-ref p 'init)))
	    (set! (~ (current-dynamic-environment) p) init)
	    init)
	  r)))
  (define-method object-apply ((p <parameter>) v)
    (let ((conv (~ p 'converter)))
      (if conv
	  (set! (~ (current-dynamic-environment) p) (conv v))	  
	  (set! (~ (current-dynamic-environment) p) v))))

  (define (make-parameter init :optional (converter #f))
    (let* ((init (if converter (converter init) init))
	   (p (make <parameter> :converter converter :init init)))
      ;; to keep parameter thread local
      (set! (~ (current-dynamic-environment) p) init)
      p))

  (define (%parameter-value-set! p v)
    (if (is-a? p <parameter>)
	(set! (~ (current-dynamic-environment) p) v)
	(p v)))

  (define (parameter-convert p v)
    (if (is-a? p <parameter>)
	(let ((conv (~ p 'converter)))
	  (if (procedure? conv)
	      (conv v)
	      v))
	;; if the parameter is procedure, e.g) current-input-port
	;; then there's not value converter. so just return given
	;; value.
	v))

  (define-syntax parameterize-aux
    (syntax-rules ()
      ;; temporaries
      ;;   P - keeps the parameter object, for the variable param may be
      ;;       reassigned during execution of body.
      ;;   L - keeps "local" value during dynamic enviornment of body.
      ;;   S - keeps "saved" value outside of parameterize.
      ((_ (param ...) (val ...) ((P L S) ...) () body)
       (let ((P param) ...)
	 ;; convert all parameter here and if it's an error
	 ;; let it raise here.
	 (let ((L (parameter-convert P val)) ... 
	       (S #f) ...)
	   (dynamic-wind
	       (lambda () (let ((t (P))) 
			    ;; the value is converted so we just need
			    ;; to set as it is
			    (%parameter-value-set! P L) 
			    (set! S t)) ...)
	       (lambda () . body)
	       (lambda () 
		 (let ((t (P)))
		   (%parameter-value-set! P S)
		   (set! L t))
		 ...)))))
      ((_ (param ...) (val ...) (tmps ...) ((p v) . more) body)
       (parameterize-aux (param ... p) (val ... v) (tmps ... (P L S))
			 more body))))

  (define-syntax parameterize
    (syntax-rules ()
      ((_ (binds ...) . body)
       (parameterize-aux () () () (binds ...) body))))

)
