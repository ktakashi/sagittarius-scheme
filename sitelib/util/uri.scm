;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; util/uri.scm - URI utilities
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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
(library (util uri)
    (export open-uri)
    (import (rnrs)
	    (rnrs eval)
	    (rfc uri))

;; underlying implementation must return a port
(define open-uri
  (let ((cache (make-eq-hashtable)))
    (lambda (uri . opts)
      (let-values (((scheme specific) (uri-scheme&specific uri)))
	(unless scheme
	  (assertion-violation 'open-uri
			       "Full URI is required" uri))
	(let ((target (string->symbol scheme)))
	  (cond ((hashtable-ref cache target #f) =>
		 (lambda (proc) (apply proc uri opts)))
		(else
		 (guard (e (else
			    (raise (condition 
				    (make-error)
				    (make-message-condition "Non supported URI")
				    (make-who-condition 'open-uri)
				    (make-irritants-condition uri)
				    e))))
		   (let ((proc (eval 'open-uri
				     (environment `(util uri ,target)))))
		     (hashtable-set! cache target proc)
		     (apply proc uri opts))))))))))
)
