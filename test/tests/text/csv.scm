;; -*- mode:scheme; coding:utf-8; -*-
#!compatible
(import (rnrs)
	(sagittarius)
	(text csv)
	(srfi :64 testing))


(define csv-file (string-append (current-directory) "/test/data/data.csv"))
(define expected-result "\"header\",\"value\"\r\n\"r1\",\"r2\",\"r3\"\r\n\"r4\",\"r5\",\"r6\",\r\n\"a,\"\",a\",\"b\",\"c\"\r\n")

(test-begin "(run-text-csv-tests)")
;; read first line as its header
(call-with-input-file csv-file
  (lambda (p)
    (let ((csv (csv-read p #t)))
      (test-equal "header (1)" '("header" "value") (csv-header csv))
      (test-equal "records (1)" 
		  '(("r1" "r2" "r3")
		    ("r4" "r5" "r6" "")
		    ("a,\",a" "b" "c")) (csv-records csv))
      (test-equal "write (1)" 
		  expected-result (call-with-string-output-port
				   (lambda (p) (csv-write csv p))))
      )))
;; not read first line as its header

(call-with-input-file csv-file
  (lambda (p)
    (let ((csv (csv-read p)))
      (test-equal "header (2)" '() (csv-header csv))
      (test-equal "records (2)" 
		  '(("header" "value")
		    ("r1" "r2" "r3")
		    ("r4" "r5" "r6" "")
		    ("a,\",a" "b" "c")) (csv-records csv))
      (test-equal "write (2)"
		  expected-result (call-with-string-output-port
				   (lambda (p) (csv-write csv p))))
      )))

(test-end)
