;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; interactive.scm - REPL
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

;; This file is part of Sagittarius Scheme 
;; The library will be loaded by C when users use REPL
;; so don't change the library name and exported variable
;; (especially read-eval-print-loop) names.

;; not to create scope with let-syntax ...
#!r6rs
(library (sagittarius interactive)
    (export read-eval-print-loop
	    current-printer
	    current-reader
	    current-exception-printer
	    current-evaluator
	    current-prompter
	    current-exit
	    default-exception-printer
	    default-evaluator
	    default-printer
	    default-reader
	    default-prompter
	    default-exit)
    (import (rnrs)
	    (rnrs eval)
	    (sagittarius)
	    (sagittarius vm)
	    (sagittarius parameters))

  (define-constant *resource-file*
    (let ((home (or (getenv "HOME") ;; this is the strongest
		    (getenv "USERPROFILE") ;; for windows
		    )))
      (build-path home ".sashrc")))

  (letrec-syntax
      ((make-parameter* (syntax-rules ()
			  ((_ name default)
			   (make-parameter 
			    default
			    (lambda (x)
			      (cond 
			       ((not x) values)
			       ((procedure? x) x)
			       (else
				(assertion-violation 'name
				 (format
				  "expected procedure or #f, but got ~s"
				  x)))))))))
       (define-parameter
	 (lambda (x)
	   (define (names* k base-name)
	     (let ((bn (format "~a" (syntax->datum base-name))))
	       (datum->syntax 
		k
		(list (string->symbol (string-append "default-" bn))
		      (string->symbol (string-append "current-" bn))))))
	   (syntax-case x ()
	     ((k (base-name . args) body ...)
	      (with-syntax (((name param) (names* #'k #'base-name)))
		#'(begin
		    (define (name . args) body ...)
		    (define param (make-parameter* param name)))))))))

    (define-parameter (exception-printer c out) (report-error c out))
    (define-parameter (evaluator form env) (eval form env))
    (define-parameter (printer . args)
      (for-each (lambda (o) (write/ss o) (newline)) args))
    (define-parameter (reader in) (read/ss in))
    (define-parameter (prompter) (display "sash> "))
    (define-parameter (exit) (exit 0)))

  ;; removed :optional keyword ...
  ;; now opt is load-resource optional argument.
  (define (read-eval-print-loop . opt)
    (define interactive-environment
      (let ((env (find-library 'user #f)))
	(eval '(import (rnrs)) env)
	env))
    (let ((plugged (getenv "EMACS")))
      ;; load resource file
      (when (and (or (null? opt) (car opt)) (file-exists? *resource-file*))
	(call-with-port
	    (open-file-input-port *resource-file* #f 'block (native-transcoder))
	  (lambda (p)
	    (let loop ((form (read/ss p)))
	      (unless (eof-object? form)
		((current-evaluator) form interactive-environment)
		(loop (read/ss p)))))))
      (let ((exit? #f))
	(let loop ()
	  (call-with-current-continuation
	   (lambda (continue)
	     (with-error-handler
	       (lambda (c)
		 (flush-output-port (current-output-port))
		 ((current-exception-printer) c (current-error-port))
		 (and (serious-condition? c) (continue)))
	       (lambda ()
		 ((current-prompter))
		 (flush-output-port (current-output-port))
		 (let ((form ((current-reader) (current-input-port))))
		   (cond ((eof-object? form)
			  (set! exit? #t)
			  ((current-exit)))
			 (else
			  (and plugged 
			       (flush-output-port (current-output-port)))
			  (receive ans ((current-evaluator) form
					interactive-environment)
			    (apply (current-printer) ans)
			    (flush-output-port (current-output-port))))))))))
	  (unless exit? (loop))))))
)
