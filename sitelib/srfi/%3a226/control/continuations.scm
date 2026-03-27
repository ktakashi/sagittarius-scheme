;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control/continuations.scm - SRFI-226 continuations
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
(library (srfi :226 control continuations)
    (export ;; Condition types
	    &continuation
	    make-continuation-violation
	    continuation-violation?
	    continuation-violation-prompt-tag
	    
	    ;; Continuation procedures
	    call-with-current-continuation
	    call/cc
	    call-with-composable-continuation
	    call-with-non-composable-continuation
	    call-in-continuation
	    call-in
	    return-to
	    
	    ;; Inspection
	    continuation?
	    non-composable-continuation?
	    continuation-prompt-available?
	    
	    ;; Barriers
	    call-with-continuation-barrier
	    
	    ;; Dynamic wind
	    dynamic-wind
	    unwind-protect)
    (import (except (rnrs) call-with-current-continuation call/cc dynamic-wind)
	    (only (rnrs) dynamic-wind)
	    (rename (sagittarius continuations)
		    (call-with-delimited-current-continuation
		     call-with-non-composable-continuation)
		    (composable-continuation? %composable-continuation?))
	    (sagittarius control))

  ;; Non-composable continuation predicate
  (define (non-composable-continuation? obj)
    (and (continuation? obj)
	 (not (%composable-continuation? obj))))

  ;; call/cc captures up to the default prompt tag
  (define (call-with-current-continuation proc)
    (call-with-non-composable-continuation proc))

  (define call/cc call-with-current-continuation)

  ;; call-in: like call-in-continuation but for non-composable continuations only
  (define (call-in cont proc . objs)
    (unless (non-composable-continuation? cont)
      (assertion-violation 'call-in
	"expected non-composable continuation" cont))
    (apply call-in-continuation cont proc objs))

  ;; return-to: deliver values to continuation
  (define (return-to cont . objs)
    (apply call-in-continuation cont values objs))

)
