;; -*- scheme -*-

;; testing mutil thread
;; this file will be called from one upper
;; so load path must be like this
(add-load-path "./time")

(import (srfi :64 testing)
	(rnrs)
	(sagittarius time))

(define (test-one-utc-tai-edge utc tai-diff tai-last-diff)
  (let* (;; right on the edge they should be the same
	 (utc-basic (make-time 'time-utc 0 utc))
	 (tai-basic (make-time 'time-tai 0 (+ utc tai-diff)))
	 (utc->tai-basic (time-utc->time-tai utc-basic))
	 (tai->utc-basic (time-tai->time-utc tai-basic))
	 ;; a second before they should be the old diff
	 (utc-basic-1 (make-time 'time-utc 0 (- utc 1)))
	 (tai-basic-1 (make-time 'time-tai 0 (- (+ utc tai-last-diff) 1)))
	 (utc->tai-basic-1 (time-utc->time-tai utc-basic-1))
	 (tai->utc-basic-1 (time-tai->time-utc tai-basic-1))
	 ;; a second later they should be the new diff
	 (utc-basic+1 (make-time 'time-utc 0 (+ utc 1)))
	 (tai-basic+1 (make-time 'time-tai 0 (+ (+ utc tai-diff) 1)))
	 (utc->tai-basic+1 (time-utc->time-tai utc-basic+1))
	 (tai->utc-basic+1 (time-tai->time-utc tai-basic+1))
	 ;; ok, let's move the clock half a month or so plus half a second
	 (shy (* 15 24 60 60))
	 (hs (/ (expt 10 9) 2))
	 ;; a second later they should be the new diff
	 (utc-basic+2 (make-time 'time-utc hs (+ utc shy)))
	 (tai-basic+2 (make-time 'time-tai hs (+ (+ utc tai-diff) shy)))
	 (utc->tai-basic+2 (time-utc->time-tai utc-basic+2))
	 (tai->utc-basic+2 (time-tai->time-utc tai-basic+2))
	 )
    (and (time=? utc-basic tai->utc-basic)
	 (time=? tai-basic utc->tai-basic)
	 (time=? utc-basic-1 tai->utc-basic-1)
	 (time=? tai-basic-1 utc->tai-basic-1)
	 (time=? utc-basic+1 tai->utc-basic+1)
	 (time=? tai-basic+1 utc->tai-basic+1)
	 (time=? utc-basic+2 tai->utc-basic+2)
	 (time=? tai-basic+2 utc->tai-basic+2) 
	 )))

(define (date= d1 d2)
  (and (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (= (date-day d1) (date-day d2))
       (= (date-hour d1) (date-hour d2))
       (= (date-second d1) (date-second d2))
       (= (date-nanosecond d1) (date-nanosecond d2))
       (= (date-zone-offset d1) (date-zone-offset d2))))

(test-begin "(run-time-test)")
(let ((t (make-time time-utc 10 20)))
  (test-assert "time?" (time? t))
  (test-equal "time type" (time-type t) time-utc)
  (test-equal "time nanosecond" (time-nanosecond t) 10)
  (test-equal "time second" (time-second t) 20)
  (let ((ct (copy-time t)))
    (test-equal "copied time type" (time-type ct) time-utc)
    (test-equal "copied time nanosecond" (time-nanosecond ct) 10)
    (test-equal "copied time second" (time-second ct) 20)))
(test-assert "time=?(1)" (time=? (make-time time-utc 20 10) (make-time time-utc 20 10)))
(test-assert "time=?(2)" (not (time=? (make-time time-utc 20 10) (make-time time-utc 10 10))))
(test-assert "time=?(3)" (not (time=? (make-time time-utc 10 10) (make-time time-utc 20 10))))

(test-assert "time>=?(1)" (time>=? (make-time time-utc 20 10) (make-time time-utc 20 10)))
(test-assert "time>=?(2)" (time>=? (make-time time-utc 30 10) (make-time time-utc 20 10)))
(test-assert "time>=?(3)" (not (time>=? (make-time time-utc 10 10) (make-time time-utc 20 10))))

(test-assert "time>?(1)" (not (time>? (make-time time-utc 20 10) (make-time time-utc 20 10))))
(test-assert "time>?(2)" (time>? (make-time time-utc 30 10) (make-time time-utc 20 10)))
(test-assert "time>?(3)" (not (time>? (make-time time-utc 10 10) (make-time time-utc 20 10))))

(test-assert "time<=?(1)" (time<=? (make-time time-utc 20 10) (make-time time-utc 20 10)))
(test-assert "time<=?(2)" (time<=? (make-time time-utc 10 10) (make-time time-utc 20 10)))
(test-assert "time<=?(3)" (not (time<=? (make-time time-utc 30 10) (make-time time-utc 20 10))))

(test-assert "time<?(1)" (not (time<? (make-time time-utc 20 10) (make-time time-utc 20 10))))
(test-assert "time<?(2)" (time<? (make-time time-utc 10 10) (make-time time-utc 20 10)))
(test-assert "time<?(3)" (not (time<? (make-time time-utc 30 10) (make-time time-utc 20 10))))

(test-equal "time difference"
	    '(#t #t)
	    (let ((t1 (make-time 'time-utc 0 3000))
		  (t2 (make-time 'time-utc 0 1000))
		  (t3 (make-time 'time-duration 0 2000))
		  (t4 (make-time 'time-duration 0 -2000)))
	      (list (time=? t3 (time-difference t1 t2))
		    (time=? t4 (time-difference t2 t1)))))

(test-assert "TAI UTC Conversions"
	     (and
	      (test-one-utc-tai-edge 915148800  32 31)
	      (test-one-utc-tai-edge 867715200  31 30)
	      (test-one-utc-tai-edge 820454400  30 29)
	      (test-one-utc-tai-edge 773020800  29 28)
	      (test-one-utc-tai-edge 741484800  28 27)
	      (test-one-utc-tai-edge 709948800  27 26)
	      (test-one-utc-tai-edge 662688000  26 25)
	      (test-one-utc-tai-edge 631152000  25 24)
	      (test-one-utc-tai-edge 567993600  24 23)
	      (test-one-utc-tai-edge 489024000  23 22)
	      (test-one-utc-tai-edge 425865600  22 21)
	      (test-one-utc-tai-edge 394329600  21 20)
	      (test-one-utc-tai-edge 362793600  20 19)
	      (test-one-utc-tai-edge 315532800  19 18)
	      (test-one-utc-tai-edge 283996800  18 17)
	      (test-one-utc-tai-edge 252460800  17 16)
	      (test-one-utc-tai-edge 220924800  16 15)
	      (test-one-utc-tai-edge 189302400  15 14)
	      (test-one-utc-tai-edge 157766400  14 13)
	      (test-one-utc-tai-edge 126230400  13 12)
	      (test-one-utc-tai-edge 94694400   12 11)
	      (test-one-utc-tai-edge 78796800   11 10)
	      (test-one-utc-tai-edge 63072000   10 0)
	      (test-one-utc-tai-edge 0   0 0) ;; at the epoch
	      (test-one-utc-tai-edge 10   0 0) ;; close to it ...
	      (test-one-utc-tai-edge 1045789645 32 32))) ;; about now ...
  

(test-assert "TAI-Date Conversions"
	     (and
	      (date= (time-tai->date (make-time time-tai 0 (+ 915148800 29)) 0)
		     (make-date 0 58 59 23 31 12 1998 0))
	      (date= (time-tai->date (make-time time-tai 0 (+ 915148800 30)) 0)
		     (make-date 0 59 59 23 31 12 1998 0))
	      (date= (time-tai->date (make-time time-tai 0 (+ 915148800 31)) 0)
		     (make-date 0 60 59 23 31 12 1998 0))
	      (date= (time-tai->date (make-time time-tai 0 (+ 915148800 32)) 0)
		     (make-date 0 0 0 0 1 1 1999 0))))

(test-assert "Date-UTC Conversions"
	     (and
	      (time=? (make-time time-utc 0 (- 915148800 2))
		      (date->time-utc (make-date 0 58 59 23 31 12 1998 0)))
	      (time=? (make-time time-utc 0 (- 915148800 1))
		      (date->time-utc (make-date 0 59 59 23 31 12 1998 0)))
	      ;; yes, I think this is acutally right.
	      (time=? (make-time time-utc 0 (- 915148800 0))
		      (date->time-utc (make-date 0 60 59 23 31 12 1998 0)))
	      (time=? (make-time time-utc 0 (- 915148800 0))
		      (date->time-utc (make-date 0 0 0 0 1 1 1999 0)))
	      (time=? (make-time time-utc 0 (+ 915148800 1))
		      (date->time-utc (make-date 0 1 0 0 1 1 1999 0)))))

(test-assert "TZ Offset conversions"
	     (let ((ct-utc (make-time time-utc 6320000 1045944859))
		   (ct-tai (make-time time-tai 6320000 1045944891))
		   (cd (make-date 6320000 19 14 15 22 2 2003 -18000)))
	       (and
		(time=? ct-utc (date->time-utc cd))
		(time=? ct-tai (date->time-tai cd)))))

(test-assert "date?" (date? (current-date)))
(test-assert "julian-day->date" (date? (julian-day->date (date->julian-day (current-date)))))
(test-equal "date convertion"
	    `(58937459/24 
	      1337447/24
	      ,(make-time time-monotonic 0 1308092433)
	      ,(make-time time-tai 0 1308092433)
	      ,(make-time time-utc 0 1308092400))
	    (let ((d (make-date 0 0 0 0 15 6 2011 3600))
		  (ret '()))
	      (set! ret (cons (date->julian-day d) ret))
	      (set! ret (cons (date->modified-julian-day d) ret))
	      (set! ret (cons (date->time-monotonic d) ret))
	      (set! ret (cons (date->time-tai d) ret))
	      (set! ret (cons (date->time-utc d) ret))
	      (reverse ret)))

(test-assert "string->date"
	     (date? (string->date "1981/12/2 00:00:00" "~Y/~m/~d ~H:~M:~S")))

(test-equal "date->string"
	    "1981/12/02 00:00:00"
	    (date->string (make-date 0 0 0 0 2 12 1981 3600)
			  "~Y/~m/~d ~H:~M:~S")
	    )
(test-end)
  
