#!compatible
(library (srfi :19)
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
	    date->string string->date
     )
    (import (srfi :19 time))
)
