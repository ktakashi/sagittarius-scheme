
#!compatible
(library (tests text csv)
    (export run-text-csv-tests)
    (import (rnrs)
	    (sagittarius)
	    (text csv)
	    (srfi :64))

  (define csv-file (string-append (current-directory) "/test/data.csv"))
  (define expected-result "\"header\",\"value\"\r\n\"r1\",\"r2\",\"r3\"\r\n\"r4\",\"r5\",\"r6\",\r\n\"a,\"\",a\",\"b\",\"c\"\r\n")

  (define (run-text-csv-tests)
    ;; read first line as its header
    (call-with-input-file csv-file
      (lambda (p)
	(let ((csv (csv-read p #t)))
	  (test-equal '("header" "value") (csv-header csv))
	  (test-equal '(("r1" "r2" "r3")
			("r4" "r5" "r6" "")
			("a,\",a" "b" "c")) (csv-records csv))
	  (test-equal expected-result (call-with-string-output-port
					 (lambda (p) (csv-write csv p))))
	  )))
    ;; not read first line as its header
    (call-with-input-file csv-file
      (lambda (p)
	(let ((csv (csv-read p)))
	  (test-equal '() (csv-header csv))
	  (test-equal '(("header" "value")
			("r1" "r2" "r3")
			("r4" "r5" "r6" "")
			("a,\",a" "b" "c")) (csv-records csv))
	  (test-equal expected-result (call-with-string-output-port
				       (lambda (p) (csv-write csv p))))
	  )))
    )
  )