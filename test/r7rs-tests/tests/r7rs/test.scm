;; -*- mode: scheme; coding: utf-8; -*-
#!r7rs
(define-library (tests r7rs test)
  (import (scheme base)
	  (prefix (srfi :64 testing) srfi:))
  (export test-equal test-error test-true test-false
	  test-approximate  test-unspecified
	  test-begin test-end)
  
  (define undef (if #f #f))

  (define (test-on-test-end-detail runner)
    (define (%test-write-result1 pair port)
      (display "  " port)
      (display (car pair) port)
      (display ": " port)
      (write (cdr pair) port)
      (newline port))
    (let ((log  (srfi:test-runner-aux-value runner))
	  (kind (srfi:test-result-ref runner 'result-kind)))
      (when (memq kind '(xpass fail))
	(let* ((results (srfi:test-result-alist runner))
	       (source-file (assq 'source-file results))
	       (source-line (assq 'source-line results))
	       (test-name (assq 'test-name results)))
	  (when (or source-file source-line)
	    (if source-file (display (cdr source-file)))
	    (display ":")
	    (if source-line (display (cdr source-line)))
	    (display ":"))
	  (display (if (eq? kind 'xpass) "XPASS" "FAIL"))
	  (when test-name
	    (display " ")(display (cdr test-name)))
	  (newline))
	(let ((expected (srfi:test-result-ref runner 'expected-value))
	      (actual   (srfi:test-result-ref runner 'actual-value)))
	  (display #\tab)(display "expected value: ")(write expected)(newline)
	  (display #\tab)(display "  actual value: ")(write actual)(newline)))
      (when (output-port? log)
	(display "Test end:" log)
	(newline log)
	(let loop ((list (srfi:test-result-alist runner)))
	  (if (pair? list)
	      (let ((pair (car list)))
		;; Write out properties not written out by on-test-begin.
		(if (not (memq 
			  (car pair)
			  '(test-name source-file source-line source-form)))
		    (srfi:%test-write-result1 pair log))
		(loop (cdr list))))))))

  (define (test-runner-detail)
    (let ((runner (srfi:test-runner-simple)))
      (srfi:test-runner-on-test-end! runner test-on-test-end-detail)
      runner))

  (define-syntax test-equal
    (syntax-rules ()
      ((_ expect expr)
       (srfi:test-equal 'expr expect expr))))

  (define-syntax test-true
    (syntax-rules ()
      ((_ test)
       (srfi:test-assert 'test test))))

  (define-syntax test-false
    (syntax-rules ()
      ((_ test)
       (srfi:test-assert 'test (not test)))))

  (define-syntax test-unspecified
    (syntax-rules ()
      ((_ test)
       (srfi:test-equal 'test undef test))))

  (define-syntax test-error
    (syntax-rules ()
      ((_ test)
       (srfi:test-error 'test test))))

  (define-syntax test-begin
    (syntax-rules ()
      ((_ msg)
       (begin
	 (srfi:test-runner-factory test-runner-detail)
	 (srfi:test-begin msg)))))

  (define-syntax test-end
    (syntax-rules ()
      ((_)
       (srfi:test-end))))
)