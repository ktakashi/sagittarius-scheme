;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/password/checker.scm - Password policy checker
;;;
;;;   Copyright (c) 2023  Takashi Kato  <ktakashi@ymail.com>
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
(library (security password checker)
    (export password-policy-compliant?)
    (import (rnrs)
	    (security password policy)
	    (sagittarius generators)
	    (text unicode)
	    (util list))

(define (password-policy-compliant? policy password)
  (let-values (((a r) (password-policy->rules policy)))
    (let ((g (string->unicode-break-generator password grapheme-strategy)))
      (generator-for-each a g)
      (r))))

(define (password-policy->rules policy)
  (if (single-password-policy? policy)
      ((single-password-policy-rule policy))
      (fold2 (lambda (policy accumlator result-retriever)
	       (let-values (((a r) (password-policy->rules policy)))
		 (values (lambda (s) (a s) (accumlator s))
			 (lambda () (and (r) (result-retriever))))))
	     values
	     (lambda () #t)
	     (compound-password-policy-policies policy))))

)
