;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/time-util.scm - common procedures between time and timezone
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius time-util)
    (export tm:time->julian-day-number
	    tm:decode-julian-day-number
	    tm:sihd tm:sid tm:nano
	    tm:tai-epoch-in-jd)
    (import (core) (sagittarius))
  ;; moved from time.scm to share with timezone.scm
  (define-constant tm:sid  86400)    ; seconds in a day
  (define-constant tm:sihd 43200)    ; seconds in a half day
  ;; julian day number for 'the epoch'
  (define-constant tm:tai-epoch-in-jd 4881175/2)
  (define-constant tm:nano (expt 10 9))

  ;; gives the seconds/date/month/year
  (define (tm:decode-julian-day-number jdn)
    (let* ((days (truncate jdn))
           (a (+ days 32044))
           (b (quotient (+ (* 4 a) 3) 146097))
           (c (- a (quotient (* 146097 b) 4)))
           (d (quotient (+ (* 4 c) 3) 1461))
           (e (- c (quotient (* 1461 d) 4)))
           (m (quotient (+ (* 5 e) 2) 153))
           (y (+ (* 100 b) d -4800 (quotient m 10))))
      (values ; seconds date month year
       (* (- jdn days) tm:sid)
       (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
       (+ m 3 (* -12 (quotient m 10)))
       (if (>= 0 y) (- y 1) y))
      ))
  (define (tm:time->julian-day-number seconds tz-offset)
    (+ (/ (+ seconds
             tz-offset
             tm:sihd)
          tm:sid)
       tm:tai-epoch-in-jd))


)
