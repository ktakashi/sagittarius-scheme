;; -*- scheme -*-

;; testing mutil thread
;; this file will be called from one upper
;; so load path must be like this
(add-load-path "./time")

(import (srfi :64 testing)
	(rnrs)
	(sagittarius time)
	;; for slot access
	(clos user))

(include "test-time.scm")

;; timezone 
(import (sagittarius timezone))

;; rest first
(test-runner-reset (test-runner-get))
(include "test-timezone.scm")

