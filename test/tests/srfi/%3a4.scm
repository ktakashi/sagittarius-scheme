#< (srfi :4) >
(import (rnrs)
	(srfi :4)
	(srfi :64 testing)
	(sagittarius control))

;; reader test helper
(define (read-from-string str)
  (read (open-string-input-port str)))

(define-syntax test-approx
  (syntax-rules ()
    ((_ tname expected val)
     (cond ((list? expected)
	    (for-each (lambda (e v)
			(test-approximate tname e v 0.1))
		      expected val))
	   ;; well, we know all uvector have slot 'value
	   ((is-a? expected <sequence>)
	    (let ((bv (slot-ref val 'value))
		  (ev (slot-ref expected 'value)))
	      (dotimes (i (bytevector-length ev))
		(test-approximate tname 
				  (bytevector-u8-ref ev i)
				  (bytevector-u8-ref ev i)
				  0.1))))
	   (else
	    (test-approximate tname expected val 0.1))))))

(define-syntax test-uvector
  (lambda (x)
    (syntax-case x ()
      ((k tag default)
       #'(test-uvector tag default test-equal))
      ((k tag default compare)
       (let ((name (string-append #'tag "vector"))
	     (gen-name (lambda (prefix n suffix)
			 (string->symbol
			  (string-append prefix n suffix)))))
	 (with-syntax ((test-name (datum->syntax #'k name))
		       (pred (datum->syntax #'k
			      (gen-name "" name "?")))
		       (ctr (datum->syntax #'k
			     (gen-name "make-" name "")))
		       (len (datum->syntax #'k
			     (gen-name "" name "-length")))
		       (ref (datum->syntax #'k
			     (gen-name "" name "-ref")))
		       (set (datum->syntax #'k
			     (gen-name "" name "-set!")))
		       (->list (datum->syntax 
				#'k (gen-name "" name "->list")))
		       (list-> (datum->syntax 
				#'k (gen-name "list->" name ""))))
	   #'(begin
	       (test-assert (string-append test-name "?") (pred (ctr 0)))
	       (test-equal (string-append  test-name "-length") 0 (len (ctr 0)))
	       (compare (string-append test-name "-ref")
			   default (ref (ctr 1 default) 0))
	       (let ((uv (ctr 1 0)))
		 (set uv 0 default)
		 (compare (string-append test-name "-set!")
			  default (ref (ctr 1 default) 0)))
	       (compare (string-append test-name "->list") '(default default)
			(->list (ctr 2 default)))
	       (compare (string-append "list->" test-name) (ctr 2 default)
			(list-> '(default default))))))))))

(test-begin "SRFI-4")

;; basic tests
(test-uvector "u8" #x00)
(test-uvector "u8" #xFF)

(test-uvector "s8" #x-80)
(test-uvector "s8" #x7F)

(test-uvector "u16" #x0000)
(test-uvector "u16" #xFFFF)

(test-uvector "s16" #x-8000)
(test-uvector "s16" #x7FFF)

(test-uvector "u32" #x0000)
(test-uvector "u32" #xFFFFFFFF)

(test-uvector "s32" #x-80000000)
(test-uvector "s32" #x7FFFFFFF)

(test-uvector "u64" #x0000)
(test-uvector "u64" #xFFFFFFFFFFFFFFFF)

(test-uvector "s64" #x-8000000000000000)
(test-uvector "s64" #x7FFFFFFFFFFFFFFF)

;; these test must be compared by approximate.
;; but I'm not sure if this test-approx is good enough or not...
(test-uvector "f32" 1.00001 test-approx)
(test-uvector "f32" 999.009 test-approx)

(test-uvector "f64" 0.000000000000001 test-approx)
(test-uvector "f64" 10.0e100 test-approx)

;; reader tests
(test-assert "#u8(1 2 3)" (u8vector? (read-from-string "#u8(1 2 3)")))
(test-assert "#s8(1 2 3)" (s8vector? (read-from-string "#s8(1 2 3)")))
(test-assert "#u16(1 2 3)" (u16vector? (read-from-string "#u16(1 2 3)")))
(test-assert "#s16(1 2 3)" (s16vector? (read-from-string "#s16(1 2 3)")))

(test-assert "#u32(1 2 3)" (u32vector? (read-from-string "#u32(1 2 3)")))
(test-assert "#s32(1 2 3)" (s32vector? (read-from-string "#s32(1 2 3)")))
(test-assert "#u64(1 2 3)" (u64vector? (read-from-string "#u64(1 2 3)")))
(test-assert "#s64(1 2 3)" (s64vector? (read-from-string "#s64(1 2 3)")))

(test-assert "#f32(1 2 3)" (f32vector? (read-from-string "#f32(1 2 3)")))
(test-assert "#f64(1 2 3)" (f64vector? (read-from-string "#f64(1 2 3)")))

(test-end)