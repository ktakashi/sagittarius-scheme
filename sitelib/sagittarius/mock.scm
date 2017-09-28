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
    (export mock-status? 
	    mock-status-callee-name
	    mock-status-arguments-list
	    mock-status-called-count
	    mock-up)
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius sandbox)
	    (sagittarius vm)
	    (sagittarius compiler procedure)
	    (util hashtables)
	    (srfi :1))

(define-record-type mock-recorder
  (fields arguments)
  (protocol (lambda (p) (lambda () (p (make-eq-hashtable))))))
(define-record-type mock-status
  (fields callee-name arguments-list))

(define (record-mock recorder name args)
  (hashtable-update! (mock-recorder-arguments recorder) name
		     (lambda (value) (cons args value)) '()))
(define (recorder-ref recorder name)
  (make-mock-status name
		    (hashtable-ref (mock-recorder-arguments recorder) name '())))
(define (recorder->mock-statuses recorder)
  (hashtable-map make-mock-status (mock-recorder-arguments recorder)))

(define (mock-status-called-count status)
  (length (mock-status-arguments-list status)))

(define-syntax mock-up
  (lambda (x)
    (syntax-case x ()
      ((k ((libs-name ...) ...) body ...)
       (with-syntax ((mock-it (datum->syntax #'k 'mock-it))
		     (mock-status-of (datum->syntax #'k 'mock-status-of))
		     (mock-statuses (datum->syntax #'k 'mock-statuses)))
	 #'(let ()
	     (define recorder (make-mock-recorder))
	     (define (mock-status-of name) (recorder-ref recorder name))
	     (define (mock-statuses) (recorder->mock-statuses recorder))
	     
	     (with-sandbox
	      (lambda ()
		(define-syntax mock-it
		  (syntax-rules ()
		    ((_ lib (name . args) expr (... ...))
		     (let ()
		       (define-in-sandbox lib (name . args)
			 (record-mock recorder 'name args)
			 expr (... ...))))))
		(%mockup k recorder (libs-name ...)) ...
		body ...))))))))

(define-syntax %mockup
  (lambda (x)
    (define (collect-exported lib-name)
      (define lib (find-library (syntax->datum lib-name) #f))
      (define (procedure-binding? e)
	(let ((g (find-binding lib e #f)))
	  (and g (procedure? (gloc-ref g)) (not (inline? (gloc-ref g))))))
      (define (collect-from-imports lib)
	(define (->exported imported)
	  (define (apply-spec exported spec)
	    (case (car spec)
	      ((prefix)
	       (map (lambda (e)
		      (string->symbol (format "~a~a" (cdr spec) e))) exported))
	      ((rename)
	       (let ((renames (cdr spec)))
		 (map (lambda (e)
			(cond ((assq e renames) => cadr)
			      (else e))) exported)))
	      ((only)
	       (let ((only (cdr spec)))
		 (filter (lambda (e)
			   (cond ((memq e only) => car)
				 (else #f))) exported)))
	      (else
	       (syntax-violation 'mock-up "unknown importing spec"
				 spec))))
	  (let ((lib (car imported))
		(spec (cdr imported)))
	    (let loop ((exported (collect-exported lib))
		       (spec (reverse spec)))
	      (if (null? spec)
		  exported
		  (loop (apply-spec exported (car spec)) (cdr spec))))))
	(append-map ->exported (library-imported lib)))
      
      (unless lib (syntax-violation 'mockup "library not found" lib-name))
      (let ((exports (library-exported lib)))
	(cond ((not exports) ;; only (core)
	       (syntax-violation 'mock-up "Can't mock the library"
				 (syntax->datum lib-name)))
	      ((memq :all (car exports))
	       (let ((exported (collect-from-imports lib)))
		 (filter procedure-binding? exported)))
	      (else
	       (append (filter procedure-binding? (car exports))
		       (filter-map (lambda (e)
				     (and (procedure-binding? (car e))
					  (cadr e))) (cdr exports)))))))
    (define (prefixing exporting)
      (map (lambda (e)
	     (string->symbol (format "mock:~a" e)))
	   (syntax->datum exporting)))
    (syntax-case x ()
      ((_ k recorder lib)
       (with-syntax* (((exported ...)
		       (datum->syntax #'k (collect-exported #'lib)))
		      ((renamed ...)
		       (datum->syntax #'k (prefixing #'(exported ...)))))
	 ;; we don't want to show it
	 #'(let ()
	     (define renamed
	       (eval 'exported (environment 'lib)))
	     ...
	     (define-in-sandbox 'lib (exported . args)
	       (record-mock recorder 'exported args)
	       (apply renamed args))
	     ...))))))
)
