;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control/interrupts.scm - SRFI-226 thread interrupts
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
(library (srfi :226 control interrupts)
    (export current-interrupt-level
	    disable-interrupts!
	    enable-interrupts!
	    with-interrupts-disabled
	    with-interrupts-enabled
	    thread-interrupt!)
    (import (rnrs)
	    (sagittarius threads)
	    (sagittarius parameters))

  ;; current-interrupt-level is a thread parameter
  ;; Initial value is 0 (interrupts enabled)
  (define current-interrupt-level (make-thread-parameter 0))

  ;; Increase interrupt level (disable interrupts)
  (define (disable-interrupts!)
    (current-interrupt-level (+ (current-interrupt-level) 1)))

  ;; Decrease interrupt level (enable interrupts)
  (define (enable-interrupts!)
    (let ((level (current-interrupt-level)))
      (when (<= level 0)
	(assertion-violation 'enable-interrupts!
	  "cannot lower interrupt level below zero"))
      (current-interrupt-level (- level 1))))

  ;; Convenience syntax for disabling interrupts
  (define-syntax with-interrupts-disabled
    (syntax-rules ()
      ((_ body ...)
       (parameterize ((current-interrupt-level
		       (+ (current-interrupt-level) 1)))
	 body ...))))

  ;; Convenience syntax for enabling interrupts
  (define-syntax with-interrupts-enabled
    (syntax-rules ()
      ((_ body ...)
       (let ((level (current-interrupt-level)))
	 (when (<= level 0)
	   (assertion-violation 'with-interrupts-enabled
	     "cannot lower interrupt level below zero"))
	 (parameterize ((current-interrupt-level (- level 1)))
	   body ...)))))

)
