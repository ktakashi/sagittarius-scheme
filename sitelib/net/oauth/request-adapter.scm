;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; parameters.scm - OAuth 1.0 library.
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
(library (net oauth request-adapter)
    (export <request-adapter>
	    make-request-adapter
	    *request-adapter*
	    init-request-adapter
	    *request*
	    request
	    request-method
	    request-uri
	    abort-request
	    auth-parameters
	    post-parameters
	    get-parameters)
    (import (rnrs)
	    (clos user)
	    (sagittarius mop validator)
	    (srfi :39 parameters))

  (define (check-procedure o v)
    (or (procedure? v)
	(assertion-violation 'check-procedure
			     "argument must be a procedure" o v))
    v)

  (define-class <request-adapter> (<validator-mixin>)
    ;; all slots must be procredures
    ((request-object :init-keyword :request-object 
		     :accessor request-adapter-request-object
		     :validator check-procedure)
     (request-method :init-keyword :request-method
		     :accessor request-adapter-request-method
		     :validator check-procedure)
     (request-uri :init-keyword :request-uri 
		  :accessor request-adapter-request-uri
		  :validator check-procedure)
     (abort-request :init-keyword :abort-request
		    :accessor request-adapter-abort-request
		    :validator check-procedure)
     (auth-parameters :init-keyword :auth-parameters
		      :accessor request-adapter-auth-parameters
		      :validator check-procedure)
     (post-parameters :init-keyword :post-parameters
		      :accessor request-adapter-post-parameters
		      :validator check-procedure)
     (get-parameters :init-keyword :get-parameters
		     :accessor request-adapter-get-parameters
		     :validator check-procedure))
    )
  (define (make-request-adapter . args)
    (apply make <request-adapter> args))

  (define *request-adapter* (make-parameter #f))

  (define (init-request-adapter adapter) (*request-adapter* adapter))

  (define *request* (make-parameter #f))
  
  (define (request)
    (or (*request*)
	((request-adapter-request-object (*request-adapter*)))))
  ;; must be captal letter request method symbol
  (define (request-method :optional (request (request)))
    ((request-adapter-request-method (*request-adapter*)) request))
  ;; must be uri string
  (define (request-uri :optional (request (request)))
    ((request-adapter-request-uri (*request-adapter*)) request))
  
  (define (auth-parameters :optional (request (request)))
    ((request-adapter-auth-parameters (*request-adapter*)) request))
  (define (post-parameters :optional (request (request)))
    ((request-adapter-post-parameters (*request-adapter*)) request))
  (define (get-parameters :optional (request (request)))
    ((request-adapter-get-parameters (*request-adapter*)) request))

  ;; Return the string RESULT immediately from the request handler.
  (define (abort-request result)
    ((request-adapter-abort-request *request-adapter*) result))
)