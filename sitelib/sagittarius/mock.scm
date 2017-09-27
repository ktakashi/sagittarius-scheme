;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/mock.scm - Mocking
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius mock)
    (export mock-status? mock-status-arguments-list mock-status-called-count)
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius control)
	    (sagittarius sandbox)
	    (sagittarius vm)
	    (srfi :1))

(define-record-type mock-recorder
  (fields arguments count)
  (protocol (lambda (p)
	      (lambda ()
		(p (make-eq-hashtable) (make-eq-hashtable))))))
(define-record-type mock-status
  (fields arguments-list called-count))
(define (record-mock recorder name args)
  (hashtable-update! (mock-recorder-arguments recorder) name
		     (lambda (value) (cons args value)) '())
  (hashtable-update! (mock-recorder-count recorder) name
		     (lambda (value) (+ value 1) 0)))
(define (recorder-ref recorder name)
  (make-mock-status (hashtable-ref (mock-recorder-arguments recorder) name '())
		    (hashtable-ref (mock-recorder-count recorder) name 0)))

(define-syntax mock-up
  (lambda (x)
    (syntax-case x ()
      ((k ((libs-name ...) ...) body ...)
       (with-syntax ((mock-it (datum->syntax #'k 'mock-it))
		     (mock-status (datum->syntax #'k 'mock-status)))
       #'(with-sandbox
	  (lambda ()
	    (define recorder (make-mock-recorder))
	    (define (mock-status name) (recorder-ref recorder name))
	    (define-syntax mock-it
	      (syntax-rules ()
		((_ lib (name . args) expr (... ...))
		 (let ()
		   (define-in-sandbox lib (name . args)
		     (record-mock recorder 'name args)
		     expr (... ...))))))
	    (%mockup k (libs-name ...)) ...
	    body ...)))))))

(define-syntax %mockup
  (lambda (x)
    (define (collect-exported lib-name)
      (define lib (find-library (syntax->datum lib-name) #f))
      (define (procedure-binding? e)
	(let ((g (find-binding lib e #f)))
	  (and g (procedure? (gloc-ref g)))))
      (unless lib (syntax-violation 'mockup "library not found" lib-name))
      (let ((exports (library-exported lib)))
	(cond ((memq :all (car exports))
	       (append-map (lambda (i)
			     (collect-exports&bindings (fixup-imported i)))
			   (library-imported lib)))
	      (else
	       (append (filter procedure-binding? (car exports))
		       (filter-map (lambda (e)
				     (and (procedure-binding? (car e))
					  (cadr e))) (cdr exports)))))))
    (syntax-case x ()
      ((_ k lib)
       (with-syntax (((exported ...)
		      (datum->syntax #'k (collect-exported #'lib)))
		     ((temp) (generate-temporaries '(a))))
	 (display (syntax->datum #'(exported ...))) (newline)
	 #'(begin
	     (define dummy
	       (begin
		 (eval '(library (mock temp) (export exported ...) (import lib))
		       (environment '(sagittarius)))
		 #f))
	     (define-in-sandbox 'lib (exported . args)
	       (apply (eval 'exported (environment '(mock temp))) args))
	     ...))))))
)
