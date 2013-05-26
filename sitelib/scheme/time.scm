;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme time)
    (export current-jiffy current-second jiffies-per-second)
    (import (rnrs)
	    (srfi :19))

  (define scale 1000)

  (define (jiffies-per-second) scale)
  (define (current-jiffy) (exact (round (* (current-second) scale))))
  (define (current-second) 
    (let ((t (current-time time-tai))) 
      (+ (/ (time-nanosecond t) (inexact scale)) (time-second t))))
)
