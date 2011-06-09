;;; -*- Scheme -*-
;;;
;;; time.scm - srfi-19 time library
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

(load-dynamic-library "sagittarius--time")
(library (sagittarius time)
    (export make-time
	    time?
	    time-type
	    time-nanosecond
	    time-second
	    set-time-type!
	    set-time-nanosecond
	    set-time-second
	    copy-time
	    current-time

	    ;; symbols
	    time-duration
	    time-monotonic
	    time-tai
	    time-utc
	    ;;time-thread
	    ;;time-process

	    ;; time convertion
	    time->seconds seconds->time

	    ;; compiration
	    time<=? time<?
	    time=?
	    time>=? time>?

	    ;; calculation
	    time-difference time-difference!
	    add-duration add-duration!
	    subtract-duration subtract-duration!
	    )
    (import (core)
	    (sagittarius time impl))

  (define time-duration  'time-duration)
  (define time-utc       'time-utc)
  (define time-tai       'time-tai)
  (define time-monotonic 'time-monotonic)
  ;;(define time-thread    'time-thread)
  ;;(define time-process   'time-process)

)