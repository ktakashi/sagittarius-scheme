;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme time)
    (export current-jiffy current-second jiffies-per-second)
    (import (rnrs)
	    (srfi :19))

  (define scale 1000000000)

  (define (jiffies-per-second) scale)
  (define (current-jiffy) (return-sec time-monotonic))
  (define (current-second) (inexact (return-sec time-tai)))
  
  (define (return-sec sym)
    (let ((t (current-time sym)))
      (+ (/ (time-nanosecond t) scale)
	 (time-second t))))

)
