;;; -*- Scheme -*-
;;;
;;; muilithreading.scm - SRFI-18 multithreading
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

#!core
(library (srfi :18 multithreading)
    (export current-thread
	    thread?
	    make-thread
	    thread-name
	    thread-specific
	    thread-specific-set!
	    thread-start!
	    thread-yield!
	    thread-sleep!
	    thread-terminate!
	    thread-join!
	    mutex?
	    make-mutex
	    mutex-name
	    mutex-specific
	    mutex-specific-set!
	    mutex-state
	    mutex-lock!
	    mutex-unlock!
	    condition-variable?
	    make-condition-variable
	    condition-variable-name
	    condition-variable-specific
	    condition-variable-specific-set!
	    condition-variable-signal!
	    condition-variable-broadcast!
	    current-time
	    time?
	    time->seconds
	    seconds->time
	    current-exception-handler
	    ;; this conflicts R6RS
	    with-exception-handler
	    raise
	    join-timeout-exception?
	    abandoned-mutex-exception?
	    terminated-thread-exception?
	    uncaught-exception?
	    uncaught-exception-reason)
    (import (sagittarius threads)
	    (only (sagittarius) current-exception-handler)
	    (rename (sagittarius) (with-error-handler with-exception-handler))
	    (only (sagittarius time) current-time time? time->seconds seconds->time))
)