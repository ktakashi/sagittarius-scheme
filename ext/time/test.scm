;; -*- scheme -*-

;; testing mutil thread
;; this file will be called from one upper
;; so load path must be like this
(add-load-path "./time")

(import (rnrs)
	(srfi :64 testing))

(include "test-time.scm")

;; timezone 
;; reset first
(test-runner-reset (test-runner-get))
(include "test-timezone.scm")

;; reset
(test-runner-reset (test-runner-get))
(include "test-calendar.scm")
