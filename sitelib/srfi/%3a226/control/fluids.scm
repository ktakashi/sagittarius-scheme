;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control/fluids.scm - SRFI-226 fluids
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :226 control fluids)
    (export define-fluid
	    define-thread-fluid
	    fluid-let
	    fluid-let*
	    define-fluidified
	    fluid-parameter)
    (import (rnrs)
	    (sagittarius parameters))

  ;; Each fluid has a hidden parameter binding
  ;; We use syntax-rules pattern matching to extract the parameter
  
  ;; define-fluid creates a parameter-backed fluid variable
  ;; The parameter is stored in a helper binding with a unique name
  (define-syntax define-fluid
    (lambda (x)
      (syntax-case x ()
	((_ name init)
	 (with-syntax ((param-name (datum->syntax #'name
				    (string->symbol
				     (string-append "%fluid-param-"
						   (symbol->string
						    (syntax->datum #'name)))))))
	   #'(begin
	       (define param-name (make-parameter init))
	       (define-syntax name
		 (identifier-syntax
		  (id (param-name))
		  ((set! id v) (param-name v))))
	       (define-syntax %extract-fluid-param-of-name
		 (syntax-rules ()
		   ((_) param-name)))))))))

  ;; define-thread-fluid creates a thread-parameter-backed fluid variable
  (define-syntax define-thread-fluid
    (lambda (x)
      (syntax-case x ()
	((_ name init)
	 (with-syntax ((param-name (datum->syntax #'name
				    (string->symbol
				     (string-append "%fluid-param-"
						   (symbol->string
						    (syntax->datum #'name)))))))
	   #'(begin
	       (define param-name (make-thread-parameter init))
	       (define-syntax name
		 (identifier-syntax
		  (id (param-name))
		  ((set! id v) (param-name v))))
	       (define-syntax %extract-fluid-param-of-name
		 (syntax-rules ()
		   ((_) param-name)))))))))

  ;; define-fluidified exposes an existing parameter as a fluid
  (define-syntax define-fluidified
    (lambda (x)
      (syntax-case x ()
	((_ name p)
	 (with-syntax ((param-name (datum->syntax #'name
				    (string->symbol
				     (string-append "%fluid-param-"
						   (symbol->string
						    (syntax->datum #'name)))))))
	   #'(begin
	       (define param-name p)
	       (define-syntax name
		 (identifier-syntax
		  (id (param-name))
		  ((set! id v) (param-name v))))
	       (define-syntax %extract-fluid-param-of-name
		 (syntax-rules ()
		   ((_) param-name)))))))))

  ;; fluid-parameter extracts the underlying parameter from a fluid
  (define-syntax fluid-parameter
    (lambda (x)
      (syntax-case x ()
	((_ id)
	 (identifier? #'id)
	 (with-syntax ((param-name (datum->syntax #'id
				    (string->symbol
				     (string-append "%fluid-param-"
						   (symbol->string
						    (syntax->datum #'id)))))))
	   #'param-name)))))

  ;; fluid-let temporarily binds fluids
  (define-syntax fluid-let
    (syntax-rules ()
      ((_ () body ...)
       (let () body ...))
      ((_ ((id val) ...) body ...)
       (parameterize (((fluid-parameter id) val) ...) body ...))))

  ;; fluid-let* sequentially binds fluids
  (define-syntax fluid-let*
    (syntax-rules ()
      ((_ () body ...)
       (let () body ...))
      ((_ ((id val)) body ...)
       (fluid-let ((id val)) body ...))
      ((_ ((id val) rest ...) body ...)
       (fluid-let ((id val))
	 (fluid-let* (rest ...) body ...)))))

)
