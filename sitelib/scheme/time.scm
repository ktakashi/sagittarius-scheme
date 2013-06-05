;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme time)
    (export current-jiffy current-second jiffies-per-second)
    (import (rnrs)
	    (srfi :19))

  (define scale 1000000000)

  (define (jiffies-per-second) scale)
  (define (current-jiffy) (exact (round (* (current-second) scale))))
  (define (current-second) 
    (let ((t (current-time time-tai))) 
      (inexact (+ (/ (time-nanosecond t) scale) (time-second t)))))
)
