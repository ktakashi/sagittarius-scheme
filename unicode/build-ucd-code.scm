;; -*- scheme -*-
;; the input files are alist of pairs.
;; so we make struct for the pairs likes this:
;; struct {
;;    SgChar in;
;;    SgChar out;
;; } simple_lower_case[10] = {
;;    { 111, 113 }, // so on
;; }
(library (build-ucd-code)
    (export build-ucd-code check-ucd-files)
    (import (rnrs) (sagittarius) (srfi :13))

(define (input-file-path f)
  (format "ucd/~a.datum" f))

(define (output-file-path f)
  (format "~a.inc" f))

(define (write-head out struct datum)
  (format out
"
/* -*- C -*-
   This file was generated by build-ucd-code.scm. DO NOT EDIT!
 */
static struct {
  int32_t in;
  int32_t out;
} ~a[~a] = {~%" struct (length datum)))

(define (write-foot out)
  (display
"};
" 
out))

(define (generate-struct-name file)
  (format "s_~a" (string-map (lambda (ch)
			       (if (char=? ch #\-)
				   #\_
				   ch))
			     file)))

(define (c-out datum)
  (cond ((number? datum) (format "~aL" datum))
	((boolean? datum)
	 (if datum 'TRUE 'FALSE))
	((symbol? datum) datum)
	(else
	 (error 'convert "unknown datum" datum))))

(define (convert converter file)
  (let* ((in (open-file-input-port (input-file-path file)
				   (file-options no-truncate)
				   'block
				   (native-transcoder)))
	 (datum (read in)))
    (close-port in)
    (let ((ofile (output-file-path file)))
      (when (file-exists? ofile) (delete-file ofile))
      (call-with-output-file ofile
	(lambda (out)
	  (converter file out datum))))))


(define converter-1
  (lambda (file out datum)
    (write-head out (generate-struct-name file) datum)
    (for-each (lambda (p)
		(format out "  { ~a, ~a },~%" 
			(c-out (car p))
			(c-out (cdr p))))
	      datum)
    (write-foot out)))

(define (write-head-with-count out count struct datum)
  (format out
"
/* -*- C -*-
   This file was generated by build-ucd-code.scm. DO NOT EDIT!
 */
static struct {
  int32_t in;
  int32_t out[~a];
} ~a[~a] = {~%" count struct (length datum)))

(define converter-2
  (lambda (file out datum)
    (define longest 0)
    (define (search-longest)
      (let loop ((datum datum))
	(unless (null? datum)
	  (set! longest (max (length (cdar datum)) longest))
	  (loop (cdr datum))))
      longest)
    (search-longest)
    (format #t "~s: longest ~a~%" file longest)
    (write-head-with-count out longest (generate-struct-name file) datum)
    (for-each (lambda (p)
		(format out "  { ~a, { " (c-out (car p)))
		(let loop ((i 0)
			   (rest (cdr p)))
		  (unless (and (null? rest)
			       (= i longest))
		    (unless (= i 0)
		      (display ", " out))
		    (if (null? rest)
			(display " 0L" out)
			(format out "~a" (c-out (car rest))))
		    (loop (+ i 1) (if (null? rest)
				      rest
				      (cdr rest)))))
		(format out " }, },~%"))
	      datum)
    (write-foot out)))

;; this is too big for 32 bits
(define (write-head64 out struct datum)
  (format out
"
/* -*- C -*-
   This file was generated by build-ucd-code.scm. DO NOT EDIT!
 */
static struct {
  int64_t in;
  int64_t out;
} ~a[~a] = {~%" struct (length datum)))


(define converter-3
  (lambda (file out datum)
    (define (c-out datum)
      (cond ((number? datum) (format "~aLL" datum))
	    ((boolean? datum)
	     (if datum 'TRUE 'FALSE))
	    ((symbol? datum) datum)
	    (else
	     (error 'convert "unknown datum" datum))))
    (write-head64 out (generate-struct-name file) datum)
    (for-each (lambda (p)
		(format out "  { ~a, ~a },~%" 
			(c-out (car p))
			(c-out (cdr p))))
	      datum)
    (write-foot out)))

(define (write-head-for-numeric out struct datum)
  (format out
"
/* -*- C -*-
   This file was generated by build-ucd-code.scm. DO NOT EDIT!
 */
static struct {
  int32_t in;
  int64_t nume;
  int64_t deno;
} ~a[~a] = {~%" struct (length datum)))
(define converter-4
  (lambda (file out datum)
    (define (c-out datum :optional (suffix "LL"))
      (cond ((number? datum) (format "~a~a" datum suffix))
	    ((boolean? datum)
	     (if datum 'TRUE 'FALSE))
	    ((symbol? datum) datum)
	    (else
	     (error 'convert "unknown datum" datum))))
    (write-head-for-numeric out (generate-struct-name file) datum)
    (for-each (lambda (p)
		(let ((v (cdr p)))
		  (format out "  { ~a, ~a, ~a },~%" 
			  (c-out (car p) "L")
			  (c-out (numerator v))
			  (c-out (denominator v)))))
	      datum)
    (write-foot out)))

(define (build-ucd-code)
  (convert converter-1 "other-alphabetic")
  (convert converter-1 "other-lowercase")
  (convert converter-1 "other-uppercase")
  (convert converter-1 "simple-lowercase")
  (convert converter-1 "simple-titlecase")
  (convert converter-1 "simple-uppercase")
  (convert converter-1 "canonical-class")
  (convert converter-1 "compatibility")
  (convert converter-1 "general-category-1")
  (convert converter-1 "general-category-2")
  

  (convert converter-2 "case-folding")
  (convert converter-2 "special-casing-lower")
  (convert converter-2 "special-casing-title")
  (convert converter-2 "special-casing-upper")
  (convert converter-2 "decompose")

  (convert converter-3 "compose")
  ;; since unicode 7.0 numeric-property has 64 bit value.
  (convert converter-4 "numeric-property"))

(define (check-ucd-files)
  (for-all (lambda (f)
	     (let ((inc-file (output-file-path f)))
	       (file-exists? inc-file)))
	     '("other-alphabetic"
	       "other-lowercase"
	       "other-uppercase"
	       "simple-lowercase"
	       "simple-titlecase"
	       "simple-uppercase"
	       "canonical-class"
	       "compatibility"
	       "general-category-1"
	       "general-category-2"
	       "case-folding"
	       "special-casing-lower"
	       "special-casing-title"
	       "special-casing-upper"
	       "decompose"
	       "compose"
	       "numeric-property")))

)
