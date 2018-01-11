;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/calendar/gregorian.scm - Gregorian calculation
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
(library (sagittarius calendar gregorian)
    (export gregorian->absolute)
    (import (rnrs))

;; TODO move
(define-syntax sum
  (syntax-rules ()
    ((_ expr index init cond)
     (do ((tmp 0 (+ tmp expr)) (index init (+ index 1)))
	 ((not cond) tmp)))))

(define (last-day-of-gregorian-month month year)
  (if (and (= month 2)
	   (zero? (mod year 4))
	   (not (memv (mod year 400) '(100 200 300))))
      29
      (vector-ref '#(31 28 31 30 31 30 31 31 30 31 30 31) (- month 1))))

;; TODO H:M:S.SSS as well
(define (gregorian->absolute d m y)
  (define y-1 (- y 1))
  (+ d 
     (sum (last-day-of-gregorian-month m* y) m* 1 (< m* m))
     (* 365 y-1)
     (div y-1 4)
     (- (div y-1 100))
     (div y-1 400)))
(define (absolute->gregorian d)
  ;; TODO H:M:S.SSS as well
  (let* ((approx (div d 366))
	 (year (+ approx
		  (sum 1 y approx (>= d (gregorian->absolute 1 1 (+ 1 y))))))
	 (month (+ 1 (sum 1 m 1 (> d (gregorian->absolute
				      (last-day-of-gregorian-month m year) m
				      year)))))
	 (day (- d (- (gregorian->absolute 1 month year) 1))))
    (values year month day)))

)

