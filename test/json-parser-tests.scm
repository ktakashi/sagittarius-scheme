(import (rnrs)
	(text json parse)
	(text json parser)
	(text json config)
	(sagittarius generators)
	(peg)
	(util file)
	(srfi :13)
	(srfi :64)
	(srfi :127))

(define strict (json-parser-config
		(strict? #t)
		(allow-comment? #f)
		(max-nesting-depth 2048)))

(define parsing-test-files
  (find-files "./test/JSONTestSuite/test_parsing" :pattern "\\.json$"))

(define json:parser-strict (make-json-parser strict))

(define (parser-json-read in)
  (let-values (((s v nl)
		(json:parser-strict (generator->lseq (->generator in)))))
    (unless (parse-success? s)
      (error 'parser-json-read "failed to parse JSON" nl))
    v))

(define (run parse . excludes)
  (define (json-test-file-name file)
    (let ((i (string-index-right file #\/)))
      (if i
	  (substring file (+ i 1) (string-length file))
	  file)))
  (define (run-test parse file)
    (unless (member file excludes)
      (let ((parse-error? (string-prefix? "n_" (json-test-file-name file))))
	(guard (e (else (test-assert file parse-error?)))
	  (call-with-input-file file parse)
	  (test-assert file (not parse-error?))))))
  (for-each (lambda (file) (run-test parse file)) parsing-test-files))

(test-begin "JSON parsing test")

;;(run (make-json-reader strict))
(run parser-json-read)

(test-end)
  
		
