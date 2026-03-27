;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; srfi/%3a226/control.scm - SRFI-226 Control Operators
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
(library (srfi :226 control)
    (export ;; From times
	    time?
	    current-time
	    seconds+

	    ;; From prompts
	    &continuation
	    make-continuation-violation
	    continuation-violation?
	    continuation-violation-prompt-tag
	    make-continuation-prompt-tag
	    continuation-prompt-tag?
	    default-continuation-prompt-tag
	    call-with-continuation-prompt
	    abort-current-continuation

	    ;; From continuations
	    call-with-current-continuation
	    call/cc
	    call-with-composable-continuation
	    call-with-non-composable-continuation
	    call-in-continuation
	    call-in
	    return-to
	    continuation?
	    non-composable-continuation?
	    continuation-prompt-available?
	    call-with-continuation-barrier
	    dynamic-wind
	    unwind-protect

	    ;; From shift-reset
	    shift reset shift-at reset-at
	    control prompt control-at prompt-at

	    ;; From inspection
	    ;; (continuation? and non-composable-continuation? from continuations)

	    ;; From continuation-marks
	    with-continuation-mark
	    with-continuation-marks
	    call-with-immediate-continuation-mark
	    continuation-marks
	    current-continuation-marks
	    continuation-mark-set?
	    continuation-mark-set->list
	    continuation-mark-set->list*
	    continuation-mark-set->iterator
	    continuation-mark-set-first
	    make-continuation-mark-key
	    continuation-mark-key?

	    ;; From parameters
	    make-parameter
	    make-thread-parameter
	    parameter?
	    parameterize
	    current-parameterization
	    parameterization?
	    call-with-parameterization
	    temporarily

	    ;; From exceptions
	    with-exception-handler
	    exception-handler-stack
	    raise
	    raise-continuable
	    guard
	    else =>

	    ;; From conditions
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
	    
	    ;; SRFI-226 specific condition types
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
	    condition-exception-handler-stack

	    ;; From fluids
	    define-fluid
	    define-thread-fluid
	    fluid-let
	    fluid-let*
	    define-fluidified
	    fluid-parameter

	    ;; From call-in-initial-continuation
	    &uncaught-exception
	    make-uncaught-exception-condition
	    uncaught-exception-condition?
	    uncaught-exception-condition-reason
	    call-in-initial-continuation

	    ;; From promises
	    delay
	    force
	    make-promise
	    promise?

	    ;; From threads
	    ;; Thread conditions
	    &thread
	    make-thread-condition
	    thread-condition?
	    &thread-already-terminated
	    make-thread-already-terminated-condition
	    thread-already-terminated-condition?
	    &thread-timeout
	    make-thread-timeout-condition
	    thread-timeout-condition?
	    &thread-abandoned-mutex
	    make-thread-abandoned-mutex-condition
	    thread-abandoned-mutex-condition?
	    &concurrent-modification
	    make-concurrent-modification-violation
	    concurrent-modification-violation?
	    
	    ;; Thread record type
	    thread
	    
	    ;; Thread procedures
	    thread?
	    current-thread
	    make-thread
	    thread-start!
	    thread-yield!
	    thread-sleep!
	    thread-terminate!
	    thread-schedule-terminate!
	    thread-join!
	    
	    ;; Mutex
	    make-mutex
	    mutex?
	    mutex-state
	    mutex-lock!
	    mutex-unlock!
	    
	    ;; Condition variables
	    make-condition-variable
	    condition-variable?
	    condition-variable-signal!
	    condition-variable-broadcast!

	    ;; From thread-locals
	    make-thread-local
	    thread-local?
	    tlref
	    tlset!

	    ;; From interrupts
	    current-interrupt-level
	    disable-interrupts!
	    enable-interrupts!
	    with-interrupts-disabled
	    with-interrupts-enabled
	    thread-interrupt!)
    (import (srfi :226 control times)
	    (srfi :226 control prompts)
	    (srfi :226 control continuations)
	    (srfi :226 control shift-reset)
	    (srfi :226 control inspection)
	    (srfi :226 control continuation-marks)
	    (srfi :226 control parameters)
	    (srfi :226 control exceptions)
	    (srfi :226 control conditions)
	    (srfi :226 control fluids)
	    (srfi :226 control call-in-initial-continuation)
	    (srfi :226 control promises)
	    (srfi :226 control threads)
	    (srfi :226 control thread-locals)
	    (srfi :226 control interrupts))

)
