(import (rnrs)
	(util file)
	(sagittarius io)
	(srfi :64)
	(util logging))
;; tests
(define (print-log logger)
  (trace-log logger "trace")
  (debug-log logger "debug")
  (info-log logger "info")
  (warn-log logger "warn")
  (error-log logger "error")
  (fatal-log logger "fatal")
  (terminate-logger! logger))

(test-begin "Logging")
(test-assert (logger? (make-logger +info-level+)))
(test-assert (not (async-logger? (make-logger +info-level+))))
(test-equal "info\nwarn\nerror\nfatal\n"
	    (with-output-to-string
	      (lambda ()
		(print-log (make-logger +info-level+ (make-appender "~l"))))))

(test-assert (logger? (make-async-logger +debug-level+)))
(test-assert (async-logger? (make-async-logger +debug-level+)))
(test-equal "debug\ninfo\nwarn\nerror\nfatal\n"
	    (with-output-to-string
	      (lambda ()
		(print-log (make-async-logger
			    +debug-level+ 
			    (make-appender "~l")
			    (make-file-appender "~l" "log.log"))))))



(test-equal "debug\ninfo\nwarn\nerror\nfatal\n" (file->string "log.log"))

(test-assert (appender? (make-appender "appender")))
(test-assert (not (file-appender? (make-appender "appender"))))
(test-assert (appender? (make-file-appender "appender" "log.log")))
(test-assert (file-appender? (make-file-appender "appender" "log.log")))

(test-end)
