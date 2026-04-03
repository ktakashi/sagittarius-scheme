;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control/conditions.scm - SRFI-226 conditions
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
(library (srfi :226 control conditions)
    (export ;; R6RS conditions
	    &condition condition? condition
	    simple-conditions
	    &warning warning?
	    &serious serious-condition?
	    &error error?
	    &violation violation?
	    &assertion assertion-violation?
	    &irritants irritants-condition? condition-irritants
	    &who who-condition? condition-who
	    &message message-condition? condition-message
	    &non-continuable non-continuable-violation?
	    &implementation-restriction implementation-restriction-violation?
	    &lexical lexical-violation?
	    &syntax syntax-violation? syntax-violation-form
	    syntax-violation-subform
	    &undefined undefined-violation?
	    
	    ;; Continuation conditions
	    &continuation
	    make-continuation-violation
	    continuation-violation?
	    continuation-violation-prompt-tag
	    
	    ;; SRFI-226 condition types
	    &parameterization
	    make-parameterization-condition
	    parameterization-condition?
	    condition-parameterization
	    
	    &continuation-marks
	    make-continuation-marks-condition
	    continuation-marks-condition?
	    condition-continuation-marks
	    
	    &exception-handler-stack
	    make-exception-handler-stack-condition
	    exception-handler-stack-condition?
	    condition-exception-handler-stack)
    ;; Thread conditions are exported from (srfi :226 control threads)
    (import (rnrs conditions)
	    (sagittarius continuations))

  ;; &parameterization - stores parameterization at origin of condition
  (define-condition-type &parameterization &condition
    make-parameterization-condition parameterization-condition?
    (parameterization condition-parameterization))

  ;; &continuation-marks - stores continuation mark set at origin
  (define-condition-type &continuation-marks &condition
    make-continuation-marks-condition continuation-marks-condition?
    (continuation-marks condition-continuation-marks))

  ;; &exception-handler-stack - stores exception handler stack at origin
  (define-condition-type &exception-handler-stack &condition
    make-exception-handler-stack-condition exception-handler-stack-condition?
    (exception-handler-stack condition-exception-handler-stack))

)
