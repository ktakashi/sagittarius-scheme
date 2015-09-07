(import (except (rnrs) define-record-type)
	(only (scheme base) bytevector)
	(srfi :4)
	(srfi :9)
	(srfi :123)
	(srfi :64))

;; tests are extracted from sample implementation
;; Original copyright of the tests

;; Copyright © 2015  Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-record-type <foo> (make-foo a b) foo?
  (a foo-a set-foo-a!)
  (b foo-b))

(test-begin "SRFI-123")

(test-begin "ref")

(test-assert "bytevector" (= 1 (ref (bytevector 0 1 2) 1)))
(test-assert "hashtable" (let ((table (make-eqv-hashtable)))
			   (hashtable-set! table 'foo 0)
			   (= 0 (ref table 'foo))))
(test-assert "hashtable default" (let ((table (make-eqv-hashtable)))
				   (= 1 (ref table 0 1))))
(test-assert "pair" (= 1 (ref (cons 0 1) 'cdr)))
(test-assert "list" (= 1 (ref (list 0 1 2) 1)))
(test-assert "string" (char=? #\b (ref "abc" 1)))
(test-assert "vector" (= 1 (ref (vector 0 1 2) 1)))
(test-assert "record" (= 1 (ref (make-foo 0 1) 'b)))

(test-assert "srfi-4" (= 1 (ref (s16vector 0 1 2) 1)))
(test-end "ref")

(test-assert "ref*" (= 1 (ref* '(_ #(_ (0 . 1) _) _) 1 1 'cdr)))

(test-begin "ref setter")
(let ((bv (bytevector 0 1 2)))
			    (set! (ref bv 1) 3)
			    (= 3 (ref bv 1)))
(test-assert "bytevector" (let ((bv (bytevector 0 1 2)))
			    (set! (ref bv 1) 3)
			    (= 3 (ref bv 1))))
(test-assert "hashtable" (let ((ht (make-eqv-hashtable)))
			   (set! (ref ht 'foo) 0)
			   (= 0 (ref ht 'foo))))
(test-assert "pair" (let ((p (cons 0 1)))
		      (set! (ref p 'cdr) 2)
		      (= 2 (ref p 'cdr))))
(test-assert "list" (let ((l (list 0 1 2)))
		      (set! (ref l 1) 3)
		      (= 3 (ref l 1))))
(test-assert "string" (let ((s (string #\a #\b #\c)))
			(set! (ref s 1) #\d)
			(char=? #\d (ref s 1))))
(test-assert "vector" (let ((v (vector 0 1 2)))
			(set! (ref v 1) 3)
			(= 3 (ref v 1))))
(test-assert "record" (let ((r (make-foo 0 1)))
			(set! (ref r 'a) 2)
			(= 2 (ref r 'a))))

(test-assert "bad record assignment"
	     (not (guard (err (else #f)) (set! (ref (make-foo 0 1) 'b) 2) #t)))

(test-assert "srfi-4" (let ((s16v (s16vector 0 1 2)))
			(set! (ref s16v 1) 3)
			(= 3 (ref s16v 1))))
(test-end "ref setter")

(test-assert "ref* setter"
	     (let ((obj (list '_ (vector '_ (cons 0 1) '_) '_)))
	       (set! (ref* obj 1 1 'cdr) 2)
	       (= 2 (ref* obj 1 1 'cdr))))
;; end of extracted tests


;; register-getter-with-setter! tests
(import (sagittarius) (srfi :17))
;; use weak-box
(define (wb-ref wb ignore) (weak-box-ref wb))
(define (wb-set! wb ignore value) (weak-box-set! wb value))
(let ((proc (getter-with-setter wb-ref wb-set!)))
  (test-assert "register-getter-with-setter!"
	       (register-getter-with-setter! weak-box? proc #f)))
;; (setter weak-box-ref) would return weak-box-set!
(test-equal "registered getter" 1 (ref (make-weak-box 1) 'dummy))
	    (let ((wb (make-weak-box 1)))
	      (set! (~ wb 'dummy) 5)
	      (ref wb 'dummy))
(test-equal "registered setter" 5
	    (let ((wb (make-weak-box 1)))
	      (set! (~ wb 'dummy) 5)
	      (ref wb 'dummy)))

(test-end "SRFI-123")
