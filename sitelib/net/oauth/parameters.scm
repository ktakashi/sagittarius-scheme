;;; -*- Scheme -*-
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

(library (net oauth parameters)
    (export sort-parameters
	    oauth-parameter?
	    remove-oauth-parameters
	    parameter
	    normalized-parameters
	    *signature-cache*)
    (import (rnrs)
	    (sagittarius)
	    (net oauth request-adapter)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :39 parameters))

  ;; the cache allows us to call normalized-parameters repeatedly
  ;; without excessive processing penalty.
  (define *parameters-cache* (make-parameter (make-weak-eq-hashtable)))
  ;; this is much more simple than maintaining multiple caches
  ;; for different parameter list flavors.
  (define *signature-cache* (make-parameter (make-weak-eq-hashtable)))

  ;; Collect request parameters and remove those excluded by the standard.
  ;; See 9.1.1.
  ;; Note: REMOVE-DUPLICATES-P has no effect right now.
  (define (normalized-parameters :key (remove-duplicates? #f))
    (define (remove-car name lis)
      (remp  (lambda (a) (equal? name (car a))) lis))
    (or (weak-hashtable-ref (*parameters-cache*) (request) #f)
	(let ((parameters (append (remove-car "realm" (auth-parameters))
				  (post-parameters)
				  (get-parameters))))
	  (weak-hashtable-set!
	   (*signature-cache*) (request)
	   (and-let* ((s (assoc "oauth_signature" parameters)))
	     (cadr s)))
	  (let* ((parameters (remove-car "oauth_signature" parameters))
		 (sorted-parameters (sort-parameters parameters)))
	    ;; for now we don't support removing duplicated parameters
	    sorted-parameters))))

  (define (parameter name :key (test equal?))
    (and-let* ((s (assoc name (normalized-parameters) test)))
      (cadr s)))

  ;; Sort parameters according to the OAuth spec.
  (define (sort-parameters parameters)
    (when (assoc "oauth_signature" parameters)
      (assertion-violation 'sort-parameters
			   "oauth_signature must not be in parameters"
			   parameters))
    (list-sort (lambda (a b)
		 (string< (car a) (car b))) parameters))

  ;; Return #t if parameter start with "oauth_".
  (define (oauth-parameter? parameter)
    (string-prefix? "oauth_" parameter))

  (define (remove-oauth-parameters parameters)
    (remp (lambda (p) (oauth-parameter? (car p))) parameters))

  )