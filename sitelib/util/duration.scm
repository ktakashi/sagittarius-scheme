;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; util/duration.scm - Duration
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
(library (util duration)
    (export duration:of-nanos
	    duration:of-micros
	    duration:of-millis
	    duration:of-seconds
	    duration:of-minutes
	    duration:of-hours
	    duration:of-days
	    )
    (import (rnrs)
	    (srfi :19))
(define *nanos-per-second*   1000000000)
(define *seconds-per-minute* 60)
(define *seconds-per-hour*   (* 60 *seconds-per-minute*))
(define *seconds-per-day*    (* *seconds-per-hour* 24))

(define (duration:of-nanos nanos)
  (let ((sec (div nanos *nanos-per-second*))
	(n (mod nanos *nanos-per-second*)))
    (if (< n 0)
	(make-time time-duration (+ n *nanos-per-second*) (- sec 1))
	(make-time time-duration n sec))))

(define (duration:of-millis millis)
  (let ((sec (div millis 1000))
	(n (mod millis 1000)))
    (if (< n 0)
	(make-time time-duration (* (+ n 1000) 1000000) (- sec 1))
	(make-time time-duration (* n 1000000) sec))))

(define (duration:of-micros micro)
  (let ((sec (div millis 1000000))
	(n (mod millis 1000000)))
    (if (< n 0)
	(make-time time-duration (* (+ n 1000000) 1000000) (- sec 1))
	(make-time time-duration (* n 1000) sec))))

(define (duration:of-seconds seconds) (make-time time-duration 0 seconds))
(define (duration:of-minutes minutes)
  (make-time time-duration 0 (* minutes *seconds-per-minute*)))
(define (duration:of-hours hour)
  (make-time time-duration 0 (* hour *seconds-per-hour*)))
(define (duration:of-days days)
  (make-time time-duration 0 (* days *seconds-per-day*)))

)
