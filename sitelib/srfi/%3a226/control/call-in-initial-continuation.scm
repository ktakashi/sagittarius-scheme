;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control/call-in-initial-continuation.scm - initial continuation
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
(library (srfi :226 control call-in-initial-continuation)
    (export ;; Uncaught exception conditions - re-exported from threads
	    &uncaught-exception
	    make-uncaught-exception-condition
	    uncaught-exception-condition?
	    uncaught-exception-condition-reason
	    
	    ;; Initial continuation
	    call-in-initial-continuation)
    (import (rnrs)
	    (rename (sagittarius continuations)
		    (call-in-initial-continuation %call-in-initial-continuation))
	    ;; Import uncaught-exception from threads to use consistent type
	    (srfi :226 control threads))

;; call-in-initial-continuation runs thunk in a fresh continuation context
;; while preserving the current parameterization.
;;
;; Implementation:
;; 1. %call-in-initial-continuation clears marks (except parameterization),
;;    winders, and prompts via C binding
;; 2. Install a default prompt so call/cc works inside
;; 3. Wrap uncaught exceptions in uncaught-exception-condition
(define (call-in-initial-continuation thunk)
  (guard (e (else
	     ;; Re-raise as continuable uncaught-exception-condition
	     (raise-continuable (make-uncaught-exception-condition e))))
    (%call-in-initial-continuation thunk)))

)
