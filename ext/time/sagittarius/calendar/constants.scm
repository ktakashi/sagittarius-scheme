;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calendar/constants.scm - Constant variables
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; reference
;; * Calendrical Calculations - Nachum Dershowitz and Edward M. Reingold
;;   http://reingold.co/cc-paper.pdf
;; * Calendrical Calculations, II: Three Historical Calendars
;;     Edward M. Reingold, Nachum Dershowitz, and Stewart M. Clamen
;;   http://reingold.co/cc2-paper.pdf
(library (sagittarius calendar constants)
    (export +julian-day-offset+ +epoch-in-utc-second+
	    +sunday+ +monday+ +tuesday+ +wednesday+
	    +thursday+ +friday+ +saturday+

	    calendar-unit-set calendar-unit
	    +calendar-unit:nanosecond+
	    +calendar-unit:second+
	    +calendar-unit:minute+
	    +calendar-unit:hour+
	    +calendar-unit:day+
	    +calendar-unit:week+
	    +calendar-unit:month+
	    +calendar-unit:year+

	    +common-time-calendar-unit-set+
	    )
    (import (only (rnrs) define define-enumeration)
	    (sagittarius))
;;; JD offset of absolute date 1 of gregorian (1/1/1)
;;; (date->julian-day (make-date 0 0 0 12 1 1 1 0)) => 1721426
;;; so absolute date 0 is 1721426 - 1
(define-constant +julian-day-offset+ 1721425)

;; (make-time 0 +epoch-in-utc-second+) points 1/1/1 12:0:0.0 GMT
(define-constant +epoch-in-utc-second+ 62135553600)
;; or should we use this as absolute 0?
;; (make-time 0 +epoch-in-utc-second+) points -1/12/31 12:0:0.0 GMT
;; (define-constant +epoch-in-utc-second+ -62135640000)

;; day of week
(define-constant +sunday+ 0)
(define-constant +monday+ 1)
(define-constant +tuesday+ 2)
(define-constant +wednesday+ 3)
(define-constant +thursday+ 4)
(define-constant +friday+ 5)
(define-constant +saturday+ 6)

;; let me use this for fun
(define-enumeration calendar-unit
  (nanosecond second minute hour day week month year)
  calendar-unit-set)
;; units which can be common if the following conditions are met:
;; 1 second = 1000000000 nanoseconds
;; 1 minute = 60 seconds
;; 1 hour   = 60 minutes
;; 1 day    = 24 hours
;; as far as I know, we can relay on this for calendars defined on earth.
;; TODO bad name... 
(define +common-time-calendar-unit-set+
  (calendar-unit-set nanosecond second minute hour day))
(define-constant +calendar-unit:nanosecond+ (calendar-unit nanosecond))
(define-constant +calendar-unit:second+     (calendar-unit second))
(define-constant +calendar-unit:minute+     (calendar-unit minute))
(define-constant +calendar-unit:hour+       (calendar-unit hour))
(define-constant +calendar-unit:day+        (calendar-unit day))
(define-constant +calendar-unit:week+       (calendar-unit week))
(define-constant +calendar-unit:month+      (calendar-unit month))
(define-constant +calendar-unit:year+       (calendar-unit year))

)
