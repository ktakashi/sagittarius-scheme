(import (rnrs)
	(util file)
	(sagittarius io)
	(srfi :13)
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

(let ((file  "log.log"))
  (when (file-exists? file) (delete-file file))
  (test-assert (logger? (make-async-logger +debug-level+)))
  (test-assert (async-logger? (make-async-logger +debug-level+)))

  (test-equal "debug\ninfo\nwarn\nerror\nfatal\n"
	      (with-output-to-string
		(lambda ()
		  (print-log (make-async-logger
			      +debug-level+ 
			      (make-appender "~l")
			      (make-file-appender "~l" file))))))
  (test-equal "debug\ninfo\nwarn\nerror\nfatal\n" (file->string file))

  (test-assert (appender? (make-appender "appender")))
  (test-assert (not (file-appender? (make-appender "appender"))))
  (test-assert (appender? (make-file-appender "appender" file)))
  (test-assert (file-appender? (make-file-appender "appender" file))))

(let ((file "rolling.log")
      (expects '("12345678901" "12345678902" "12345678903" "12345678904")))
  (define logger
    (make-logger +trace-level+
		 (make-rolling-file-appender "~m" file 10)))
  (when (file-exists? file) (delete-file file))
  (for-each (lambda (v) (trace-log logger v)) expects)
  (let ((log-files (find-files "." :pattern file)))
    (define (comp a b)
      (if (= (string-length a) (string-length b))
	  (string<? a b)
	  (> (string-length a) (string-length b))))
    (for-each (lambda (e v) 
		(test-equal "rolling file appender" e (string-trim-right v)))
	      expects
	      (map file->string (list-sort comp log-files)))
    (for-each delete-file log-files)))


(test-end)
