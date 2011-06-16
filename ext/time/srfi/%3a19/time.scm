;;; -*- Scheme -*-
;;;
;;; time.scm - SRFI-19 time
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
(library (srfi :19 time)
    (export make-time
	    time?
	    time-type
	    time-nanosecond
	    time-second
	    set-time-type!
	    set-time-nanosecond!
	    set-time-second!
	    copy-time
	    current-time

	    ;; symbols
	    time-duration
	    time-monotonic
	    time-tai
	    time-utc
	    time-thread
	    time-process

	    ;; compiration
	    time<=? time<?
	    time=?
	    time>=? time>?

	    ;; calculation
	    time-difference time-difference!
	    add-duration add-duration!
	    subtract-duration subtract-duration!
	    
	    ;; current date and resolution
	    current-date current-julian-day
	    current-modified-julian-day
	    time-resolution

	    ;; date
	    make-date
	    date? date-nanosecond
	    date-second date-minute date-hour
	    date-day date-month date-year
	    date-zone-offset
	    date-year-day
	    date-week-day
	    date-week-number

	    ;; converter
	    ;; time-monotonic->*
	    time-monotonic->date
	    time-monotonic->julian-day
	    time-monotonic->modified-julian-day
	    time-monotonic->time-tai
	    time-monotonic->time-tai!
	    time-monotonic->time-utc
	    time-monotonic->time-utc!
	    ;; time-tai->*
	    time-tai->date
	    time-tai->julian-day
	    time-tai->modified-julian-day
	    time-tai->time-monotonic
	    time-tai->time-monotonic!
	    time-tai->time-utc
	    time-tai->time-utc!
	    ;; time-utc->*
	    time-utc->date
	    time-utc->julian-day
	    time-utc->modified-julian-day
	    time-utc->time-monotonic
	    time-utc->time-monotonic!
	    time-utc->time-tai
	    time-utc->time-tai!

	    ;; date->*
	    date->julian-day
	    date->modified-julian-day
	    date->time-monotonic
	    date->time-tai
	    date->time-utc
	    
	    ;; julian-day->*
	    julian-day->date
	    julian-day->time-monotonic
	    julian-day->time-tai
	    julian-day->time-utc

	    ;; converte to string
	    date->string string->date)
    (import (sagittarius time)))