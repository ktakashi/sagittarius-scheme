;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/parameters.scm - parameter library
;;;  
;;;   Copyright (c) 2010-2026  Takashi Kato  <ktakashi@ymail.com>
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
#!nounbound
(library (sagittarius parameters)
    (export make-thread-parameter thread-parameter? <thread-parameter>
	    make-parameter <parameter> parameter?

	    current-parameterization
	    parameterization? call-with-parameterization

	    thread-local? <thread-local> tlref tlset!
	    
	    parameterize
	    parameterize/dw temporarily)
    (import (core)
	    (core syntax)
	    (clos user)
	    (sagittarius)
	    (sagittarius continuations))

(define mark (list 0)) ;; unique mark

(define (parameterize-value p v)
  (let ((val (parameter-convert p v)))
    (if (thread-parameter? p)
	(make-thread-local val #t)
	val)))
(define-syntax parameterize
  (lambda (x)
    (syntax-case x ()
      ((_ ((p v) ...) e1 e2 ...)
      #'(with-continuation-mark
	    (parameterization-continuation-mark-key)
	    (parameterization-extend
	     (current-parameterization)
	     (list (cons p (parameterize-value p v)) ...))
	  (let () e1 e2 ...))))))

(define (call-with-parameterization (p parameterization?) (thunk procedure?))
  (with-continuation-mark (parameterization-continuation-mark-key) p (thunk)))

(define-class <parameter> ()
  ((converter :init-keyword :converter :reader parameter-converter)
   (init :init-keyword :init :reader parameter-val :writer parameter-val-set!)))
(define (parameter? o) (or (core-parameter? o) (is-a? o <parameter>)))
(define (make-parameter init :optional (converter #f))
  (let ((init (if converter (converter init) init)))
    (make <parameter> :converter converter :init init)))

(define (parameter-cell p)
  (parameterization-ref (current-parameterization) p))
(define (parameter-set! p v)
  (cond ((parameter-cell p) =>
	 (lambda (cell) (set-cdr! cell v)))
	(else (parameter-val-set! p v))))

(define-method object-apply ((p <parameter>))
  (cond ((parameter-cell p) => cdr)
	(else (parameter-val p))))

(define-method object-apply ((p <parameter>) v)
  (let ((conv (parameter-converter p)))
    (parameter-set! p (if conv (conv v) v))))

;; thread local
(define-class <thread-local> ()
  ((default :init-keyword :default :init-value #f
	    :reader thread-local-default :writer thread-local-default-set!)
   (inheritable? :init-keyword :inheritable? :init-value #f
		 :reader thread-local-inheritable?
		 :writer  thread-local-inheritable-set!)))

(define (make-thread-local value :optional (inheritable? #f))
  (make <thread-local> :default value :inheritable? inheritable?))
(define (thread-local? o) (is-a? o <thread-local>))
(define (tlref tl)
  (weak-hashtable-ref (current-dynamic-environment) tl
		      (thread-local-default tl)))
(define (tlset! tl obj)
  (weak-hashtable-set! (current-dynamic-environment) tl obj
		       (or (and (thread-local-inheritable? tl)
				*dictionary-transient-entry*)
			   0)))


(define-class <thread-parameter> (<parameter>) ())
(define (thread-parameter? o) (is-a? o <thread-parameter>))

(define-method object-apply ((p <thread-parameter>))
  (tlref (cond ((parameter-cell p) => cdr)
	       (else (parameter-val p)))))
(define-method object-apply ((p <thread-parameter>) v)
  (let ((conv (parameter-converter p)))
    (tlset! (cond ((parameter-cell p) => cdr)
		  (else (parameter-val p)))
	    (if conv (conv v) v))))

(define (make-thread-parameter init :optional (converter #f))
  (let ((init (if converter (converter init) init)))
    (make <thread-parameter> :converter converter
	  :init (make-thread-local init))))

(define (%parameter-value-set! p v)
  (cond ((is-a? p <thread-parameter>)
	 (let ((tl (parameter-val p)))
	   (tlset! tl v)))
	((is-a? p <parameter>) (parameter-set! p v))
	(else (p v))))

(define (parameter-convert p v)
  (if (is-a? p <parameter>)
      (let ((conv (parameter-converter p)))
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

(define-syntax parameterize/dw
  (syntax-rules ()
    ((_ (binds ...) . body)
     (parameterize-aux () () () (binds ...) body))))

;; SRFI-226
(define-syntax temporarily
  (lambda (x)
    (syntax-case x ()
      ((_ () b1 b2 ...) #'(let () b1 b2 ...))
      ((_ ((x e) ...) b1 b2 ...)
       (with-syntax (((p ...) (generate-temporaries #'(x ...)))
		     ((y ...) (generate-temporaries #'(x ...))))
	 #'(let ((p x) ... (y e) ...)
	     (let ((swap (lambda ()
			   (let ((t (p))) (p y) (set! y t))
			   ...)))
	       (dynamic-wind swap (lambda () b1 b2 ...) swap))))))))

)
