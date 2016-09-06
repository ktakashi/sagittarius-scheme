(import (rnrs)
	(util file)
	(sagittarius io)
	(srfi :13)
	(srfi :18)
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

(test-assert trace-log)
(test-assert logger-trace?)
(test-assert debug-log)
(test-assert logger-debug?)
(test-assert info-log)
(test-assert logger-info?)
(test-assert warn-log)
(test-assert logger-warn?)
(test-assert error-log)
(test-assert logger-error?)
(test-assert fatal-log)
(test-assert logger-fatal?)

(test-assert (integer? +trace-level+))
(test-assert (integer? +debug-level+))
(test-assert (integer? +info-level+ ))
(test-assert (integer? +warn-level+ ))
(test-assert (integer? +error-level+))
(test-assert (integer? +fatal-level+))

(test-assert (< +trace-level+ +debug-level+ +info-level+
		+warn-level+ +error-level+ +fatal-level+))

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

(let ((file "daily.log")
      (expects '("12345678901" #f
		 "12345678902" #f 
		 "12345678903" #f
		 "12345678904")))
  (define logger
    (make-logger +trace-level+
      (make-daily-rolling-file-appender "~m" file "~Y-~m-~d-~k~M~S")))
  (when (file-exists? file) (delete-file file))
  (for-each (lambda (v) 
	      (if v
		  (trace-log logger v)
		  (thread-sleep! 1))) expects)
  (let ((log-files (find-files "." :pattern file)))
    (define (comp a b)
      (if (= (string-length a) (string-length b))
	  (string<? a b)
	  (> (string-length a) (string-length b))))
    (for-each (lambda (e v) 
		(test-equal "daily rolling file appender"
			    e (string-trim-right v)))
	      (filter values expects)
	      (map file->string (list-sort comp log-files)))
    (for-each delete-file log-files)))

(let ()
  (define (make-test-logger) (make-logger +info-level+ (make-appender "~m")))
  (define-logger-storage (lookup register)
    (loggers (test-logger make-test-logger)
	     (test-logger3 (lambda () (make-logger +info-level+)))))
  (test-assert "lookup(1)" (logger? (lookup 'test-logger)))
  (test-assert "register" (register 'test-logger2 (make-logger +warn-level+)))
  (test-assert "lookup(2)" (eq? (lookup 'test-logger) (lookup 'test-logger)))
  (test-assert "lookup(3)"
	       (not (eq? (lookup 'test-logger) (lookup 'test-logger2))))
  (test-assert "lookup(4)" (logger? (lookup 'test-logger3)))
  (test-assert "lookup(5)" (not (lookup 'test-logger4))))

(test-end)
